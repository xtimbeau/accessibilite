# changement de l'algorithme 
# la sélection des paires s'effectue au coeur du calcul et repose sur la majoration
# par le calcul des distances entre les destinations du paquet

iso_accessibilite <- function(
  quoi,                            # sf avec des variables numériques qui vont être aggrégées
  ou=NULL,                         # positions sur lesquelles sont calculés les accessibilités (si NULL, sur une grille)
  res_quoi=Inf,                    # projection éventuelle des lieux sur une grille
  resolution=ifelse(is.null(ou), 200, Inf),
  fun_quoi="any",                    # si projection fonction d'agrégation
  routing,                         # défini le moteur de routage
  tmax=10L,                        # en minutes
  pdt=1L,
  chunk=5000000,                   # paquet envoyé
  future=TRUE,                  
  out=ifelse(is.finite(resolution), resolution, "raster"),
  ttm_out= FALSE, 
  logs = localdata,
  dir=NULL)                        # ne recalcule pas les groupes déjà calculés, attention !  
{
  start_time <- Sys.time()
  
  # 1. découpe les groupes d'origines
  # 2. par groupe
  #    1. prend un point au hasard
  #    2. calcule les distances entre ce point et les cibles
  #    3. calcule les distance entre le point et les autres du paquet
  #    4. sélectionne pour chaque auter point du paquet les cibles atteignables
  #    5. calcule les distances
  # 4. aggrege
  # 5. cumule
  # 6. rasterize
  
  dir.create("{logs}/logs" %>% glue, showWarnings = FALSE)
  timestamp <- lubridate::stamp("15-01-20 10h08.05", orders = "dmy HMS", quiet=TRUE) (lubridate::now())
  logfile <- glue("{logs}/logs/iso_accessibilite.{routing$type}.{timestamp}.log")
  log_appender(appender_file(logfile))
  
  log_success("Calcul accessibilité version 2")
  log_success("{capture.output(show(routing))}")
  log_success("")
  log_success("tmax:{tmax}")
  log_success("pdt:{pdt}")
  log_success("chunk:{f2si2(chunk)}")
  log_success("resolution:{resolution}")
  log_success("future:{future}")
  log_success("out:{out}")
  
  opp_var <- names(quoi %>% dplyr::as_tibble() %>% dplyr::select(where(is.numeric)))
  if(length(opp_var)==0)
  {
    opp_var <- "c"
    quoi <- quoi %>% dplyr::mutate(c=1)
  }
  
  names(opp_var) <- opp_var
  
  log_success("les variables sont {c(opp_var)}")
  
  # fabrique les points d'origine (ou) et les points de cibles (ou)
  # dans le système de coordonnées 4326
  
  ouetquoi <- iso_ouetquoi_4326(
    ou=ou, 
    quoi=quoi,  
    res_ou=resolution, 
    res_quoi=res_quoi,
    opp_var=opp_var,
    fun_quoi=fun_quoi,
    resolution=resolution)
  
  ou_4326 <- ouetquoi$ou_4326
  quoi_4326 <- ouetquoi$quoi_4326
  
  npaires_brut <- as.numeric(nrow(quoi_4326))*as.numeric(nrow(ou_4326))
  
  # établit les paquets (sur une grille)
  
  groupes <- iso_split_ou(
    ou=ou_4326, 
    quoi=quoi_4326,
    chunk=chunk,
    routing=routing,
    tmax=tmax)
  
  ou_4326 <- groupes$ou
  ou_gr <- groupes$ou_gr
  k <- groupes$subsampling
  log_success("{f2si2(nrow(ou_4326))} ou, {f2si2(nrow(quoi_4326))} quoi")
  log_success("{length(ou_gr)} groupes, {k} subsampling")
  
  pids <- furrr::future_map(1:(future::nbrOfWorkers()), ~future:::session_uuid()[[1]])
  if(is.null(dir)) 
    dir <- tempdir()
  
  if(!is.null(routing$groupes)) 
    routing <- swap2tmp_routing(routing)

  message("...calcul des temps de parcours")

  with_progress({
    pb <- progressor(steps=length(ou_gr))
    if(routing$future&future)
    {
      plan(plan())
      lt <- log_threshold()
      access <- furrr::future_map( ou_gr, function(g) {
        log_threshold(lt)
        log_appender(logger::appender_file(logfile))
        pb()
        rrouting <- get_routing(routing, g)
        access_on_groupe(g, ou_4326, quoi_4326, rrouting, k, tmax, opp_var, ttm_out, pids, dir)
        },.options=furrr::furrr_options(seed=TRUE, 
                                        packages=c("data.table", "logger", "osrm", "matrixStats", "rdist", "stringr", "glue"),
                                        scheduling = 1))
      }
    else
      {
        access <- purrr::map(ou_gr, function(g) {
          pb()
          rrouting <- get_routing(routing, g)
          access_on_groupe(g, ou_4326, quoi_4326, rrouting, k, tmax, opp_var, ttm_out, pids, dir)
        })
        }
    access <- rbindlist(access)
  },handlers=handler_progress(format=":bar :percent :eta", width=80))
  
  if(ttm_out)
  {
    gc()
    plan(plan()) # pour reprendre la mémoire
    access <- map(access$file,~{
      tt <- fread(.x)
      tt[, .(fromId, toId, travel_time)]
      setkey(tt, fromId)
      setindex(tt, toId)
      tt
      })
    names(access) <- ou_gr
    res <- list(
      type = "dt",
      origin = routing$type,
      origin_string = routing$string,
      string = "matrice de time travel {routing$type} precalculee" %>% glue,
      time_table = access,
      fromId = ou_4326[, .(id, lon, lat, x, y)],
      toId = quoi_4326[, .(id, lon, lat, x, y)], 
      groupes=ou_gr,
      resolution=groupes$resINS,
      res_ou = res_ou,
      res_quoi = res_quoi,
      ancres=FALSE, 
      future=TRUE)
    }
  else
    {
      npaires <- sum(access[, .(npaires=npep[[1]]), by=fromId][["npaires"]])
      access[, `:=`(npea=NULL, npep=NULL)]
      
      groupes_ok <- unique(access$gr)
      groupes_pasok <- setdiff(ou_gr, groupes_ok)
      ou_pasok <- ou_4326[gr%chin%groupes_pasok, .N]
      
      log_warn("{ou_pasok} ({signif(ou_pasok/nrow(ou_4326)*100, 1)}%) origines non evaluees ")
      access[,gr:=NULL]
      message("...cumul")
      setnames(access, new="temps", old="travel_time")
      all_times <- CJ(fromId=unique(access$fromId),temps=c(1:tmax), sorted=FALSE)
      access <- merge(all_times, access, by=c("fromId", "temps"), all.x=TRUE)
      for (v in opp_var) 
        set(access, i=which(is.na(access[[v]])), j=v, 0)
      setorder(access, fromId, temps)
      access_c <- access[, list.append(map(.SD, cumsum), temps=temps),
                         by=fromId,
                         .SDcols=opp_var]
      tt <- seq(pdt, tmax, pdt)
      access_c <- access_c[temps%in%tt, ]
      access_c <- merge(access_c, ou_4326, by.x="fromId", by.y="id")
      
      r_xy <- access_c[, .(x=x[[1]], y=y[[1]]), by=fromId] [, fromId:=NULL]
  
      if(is.numeric(out)) {
        outr <- resolution
        out <- "raster"
      }
      res <- switch(
        out,
        data.table = access_c,
        sf = access_c %>% as_tibble() %>% st_as_sf(coords=c("x","y"), crs=3035),
        raster = {
          message("...rastérization")
          xxyy <- access_c[, .(x=x[[1]], y=y[[1]]), by=fromId]
          sqs <- st_as_sf(xxyy, coords=c("x", "y"), crs=3035) %>% st_buffer(outr/2)
          if (!is.null(ou))
            r <- raster_ref(ou %>% st_transform(3035), outr)
          else
            r <- raster_ref(quoi %>% st_transform(3035), outr)
          ttn <- str_c("iso",tt, "m")
          map(opp_var, function(v) {
            r_xy <- dcast(access_c, fromId~temps, value.var=v)
            names(r_xy) <- c("fromId", ttn)
            r_xy[, geometry:=sqs$geometry]
            r_xy <- r_xy %>% as.data.frame() %>% st_as_sf()
            brick(
              map(ttn, ~{
                rz <- fasterize::fasterize(r_xy, r, field=.x)
                names(rz) <- .x
                rz}))})
          })
      dtime <- as.numeric(Sys.time()) - as.numeric(start_time)  
      red <- 100*(npaires_brut-npaires)/npaires_brut
      tmn <- second2str(dtime)
      speed_b <- npaires_brut/dtime
      speed <- npaires/dtime
      mtime <- "{tmn} - {f2si2(npaires)} routes - {f2si2(speed_b)} routes(brut)/s - {f2si2(speed)} routes/s - {signif(red,2)}% reduction" %>% glue()
      message(mtime)
      log_success("{routing$string} en {mtime}")
      attr(res, "routing")<- ("{routing$string} en {mtime}" %>% glue)
      }
  res
  }

# pour dt_ttm, optimisé si le routing est précalculé

iso_access_dt <- function(
  quoi,                            # sf avec des variables numériques qui vont être aggrégées
  ou=NULL,                         # positions sur lesquelles sont calculés les accessibilités (si NULL, sur une grille)
  res_quoi=Inf,                    # projection éventuelle des lieux sur une grille
  resolution=ifelse(is.null(ou), 200, Inf),
  fun_quoi="any",                    # si projection fonction d'agrégation
  routing,                         # défini le moteur de routage
  tmax=10L,                        # en minutes
  pdt=1L,
  future=TRUE)
{
  start_time <- Sys.time()
  
  # 1. découpe les groupes d'origines
  # 2. par groupe
  #    1. prend un point au hasard
  #    2. calcule les distances entre ce point et les cibles
  #    3. calcule les distance entre le point et les autres du paquet
  #    4. sélectionne pour chaque auter point du paquet les cibles atteignables
  #    5. calcule les distances
  # 4. aggrege
  # 5. cumule
  # 6. rasterize
  
  opp_var <- names(quoi %>%
                     dplyr::as_tibble() %>%
                     dplyr::select(where(is.numeric)))
  
  if(length(opp_var)==0)
  {
    opp_var <- "c"
    quoi <- quoi %>%
      dplyr::mutate(c=1)
  }
  
  names(opp_var) <- opp_var
  
  # fabrique les points d'origine (ou) et les points de cibles (ou)
  # dans le système de coordonnées 4326
  
  ouetquoi <- iso_ouetquoi_4326(
    ou=ou, 
    quoi=quoi,  
    res_ou=resolution, 
    res_quoi=res_quoi,
    opp_var=opp_var,
    fun_quoi=fun_quoi,
    resolution=resolution)
  
  ou_4326 <- ouetquoi$ou_4326
  quoi_4326 <- ouetquoi$quoi_4326
  
  npaires_brut <- as.numeric(nrow(quoi_4326))*as.numeric(nrow(ou_4326))
  
  # établit les paquets (sur une grille)
  
  groupes <- iso_split_ou(
    ou=ou_4326, 
    quoi=quoi_4326,
    chunk=10,
    routing=routing,
    tmax=tmax)
  
  ou_4326 <- groupes$ou
  ou_gr <- groupes$ou_gr
  
  message("...calcul des temps de parcours")
  
  g_routing <- split_routing(routing, ou_gr)
  
  with_progress({
    pb <- progressor(steps=length(ou_gr))
    if(routing$future&future)
    {
      plan(plan())
      access <- furrr::future_imap( g_routing, function(r, g) {
        pb()
        dt_access_on_groupe(g, ou_4326[gr==g], quoi_4326, r, tmax, opp_var)
      },.options=furrr::furrr_options(seed=TRUE, 
                                      packages=c("data.table", "logger", "osrm", "matrixStats", "rdist", "stringr", "glue"),
                                      scheduling = 1))
    }
    else
    {
      access <- purrr::imap(g_routing, function(r,g) {
        pb()
        dt_access_on_groupe(g, ou_4326[gr==g], quoi_4326, r, tmax, opp_var)
      })
    }
    access <- rbindlist(access)
  },handlers=handler_progress(format=":bar :percent :eta", width=80))
  
  message("...cumul")
  setnames(access, new="temps", old="travel_time")
  all_times <- CJ(fromId=unique(access$fromId),temps=c(1:tmax), sorted=FALSE)
  access <- merge(all_times, access, by=c("fromId", "temps"), all.x=TRUE)
  for (v in opp_var) 
    set(access, i=which(is.na(access[[v]])), j=v, 0)
  setorder(access, fromId, temps)
  access_c <- access[,
                     list.append(map(.SD, cumsum), temps=temps),
                     by=fromId,
                     .SDcols=opp_var]
  tt <- seq(pdt, tmax, pdt)
  access_c <- access_c[temps%in%tt, ]
  access_c <- merge(access_c, ou_4326, by.x="fromId", by.y="id")
  r_xy <- access_c[, .(x=x[[1]], y=y[[1]]), by=fromId] [, fromId:=NULL]
  outr <- resolution
  message("...rastérization")
  r_xy <- access_c[, .(x=x[[1]], y=y[[1]]), by=fromId] [, fromId:=NULL]
  if (!is.null(ou))
    r <- raster_ref(ou %>% st_transform(3035), outr)
  else
    r <- raster_ref(quoi %>% st_transform(3035), outr)
  res <- map(opp_var, function(v) {
      brick(
        map(tt, ~{
          rz <- raster::rasterize(r_xy, r, field=access_c[temps==.x, .SD, .SDcols=v])
          names(rz) <- str_c("iso",.x, "m")
          rz}))})
    dtime <- as.numeric(Sys.time()) - as.numeric(start_time)  
    tmn <- second2str(dtime)
    speed_b <- npaires_brut/dtime
    mtime <- "{tmn} - {f2si2(npaires)} routes - {f2si2(speed_b)} routes(brut)/s" %>% glue()
    message(mtime)
  res
}

# fonctions internes ----------------

# iso_ouetquoi projette sur 4326 les coordonnées et fabrique les grilles nécessaires en donnant en sortie les ou et quoi utilisés pour ttm

iso_ouetquoi_4326 <- function(ou, quoi, res_ou, res_quoi, opp_var, fun_quoi="any", resolution=res_quoi, rf=5)
{
  # projection éventuelle sur une grille 3035 à la résolution res_quoi ou resolution
  if (!("sfc_POINT" %in% class(st_geometry(quoi)))|is.finite(res_quoi))
  {
    if((!is.finite(res_quoi))) 
      res_quoi <- resolution
    if("sfc_POINT" %in% class(st_geometry(quoi))) 
      quoi <- quoi %>% st_buffer(resolution/5)
    
    quoi <- quoi %>% st_transform(3035)
    gc()
    rrr_3035 <- 
      brick(
        map(
          opp_var,
          ~(
            fasterize(
              quoi,
              raster::disaggregate(raster_ref(quoi, resolution), fact=rf), 
              fun=fun_quoi,
              background=0,
              field=.x))))
    rr_3035 <- raster::aggregate(rrr_3035, fact=rf, fun=mean)
    xy_3035 <- raster::coordinates(rr_3035)
    quoi_3035 <- data.table(rr_3035 %>% as.data.frame(),
                            x=xy_3035[,1] %>% round(), 
                            y=xy_3035[,2]%>% round())
    quoi_4326 <- quoi_3035 %>% as.data.table
    keep <- purrr::reduce(
      purrr::map(opp_var, ~{
      xx <- quoi_4326[[.x]]
      (!is.na(xx))&(xx!=0)}),
      and)
    quoi_4326 <- quoi_4326[keep,]
    xy_3035 <-quoi_4326[, .(x,y)] %>% as.matrix
    xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
    rm(rrr_3035,rr_3035)
    gc()
  }
  else
  {
    xy_3035 <- st_coordinates(quoi %>% st_transform(3035))
    xy_4326 <- st_coordinates(quoi %>% st_transform(4326))
    quoi_4326 <- quoi %>%
      as_tibble() %>%
      dplyr::select(all_of(opp_var)) %>%
      as.data.table()
  }
  quoi_4326[, `:=`(lon = xy_4326[, 1],
                   lat = xy_4326[, 2],
                   x= xy_3035[,1] %>% round(),
                   y=xy_3035[,2] %>% round())]
  quoi_4326[, id := .I]
  setkey(quoi_4326, id)
  
  if (is.null(ou))
  {
    # pas de ou, on le prend égal à quoi en forme
    ou_3035 <- raster_ref(quoi %>% st_transform(3035), resolution = res_ou)
    ncells <- 1:(ou_3035@ncols * ou_3035@nrows)
    xy_3035 <-  raster::xyFromCell(ou_3035, ncells)
    xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
    ou_4326 <-
      data.table(
        lon = xy_4326[, 1],
        lat = xy_4326[, 2],
        x = xy_3035[, 1] %>% round(),
        y = xy_3035[, 2] %>% round()
      )
  }
  else {
    # un ou
    if (is.finite(res_ou)&&!("sfc_POINT" %in% class(st_geometry(ou))))
    {
      # pas de points mais une résolution, on crée la grille
      ou_3035 <- ou %>% st_transform(3035)
      rr_3035 <- fasterize::fasterize(ou_3035, raster_ref(ou_3035, res_ou), fun = "any")
      xy_3035 <- raster::xyFromCell(rr_3035, which(raster::values(rr_3035) == 1))
      xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
      ou_4326 <- data.table(lon = xy_4326[, 1],
                            lat = xy_4326[, 2],
                            x = xy_3035[,1] %>% round(),
                            y = xy_3035[,2] %>% round())
    }
    else 
    {
      # des points, une résolution ou pas, on garde les points
      if (!("sfc_POINT" %in% class(st_geometry(ou)))) ou <- st_centroid(ou)
      xy_3035 <- ou %>% st_transform(3035) %>% st_coordinates()
      xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
      ou_4326 <- data.table(lon = xy_4326[, 1],
                            lat = xy_4326[, 2],
                            x = xy_3035[, 1] %>% round(),
                            y = xy_3035[, 2] %>% round())
    }
  }
  
  ou_4326[, id := .I]
  setkey(ou_4326, id)
  
  list(ou_4326=ou_4326, quoi_4326=quoi_4326)
}

iso_split_ou <- function(ou, quoi, chunk=NULL, routing, tmax=60) 
{
  n <- min(100, nrow(ou))
  mou <- ou[sample(.N, n), .(x,y)] %>% as.matrix
  mquoi <- quoi[, .(x,y)] %>% as.matrix
  nquoi <- median(matrixStats::rowSums2(rdist::cdist(mou, mquoi) <= vmaxmode(routing$mode)*tmax))

  size <- as.numeric(nrow(ou))*as.numeric(nquoi)
  bbox <- matrix(c(xmax=max(ou$lon), xmin=min(ou$lon), ymax=max(ou$lat), ymin= min(ou$lat)), nrow=2)
  bbox <- sf_project(pts=bbox, from=st_crs(4326), to=st_crs(3035))
  surf <- (bbox[1,1]-bbox[2,1])*(bbox[1,2]-bbox[2,2])
  n_t <- if(is.null(routing$n_threads)) 1 else routing$n_threads
  
  if(!is.null(routing$groupes)) 
  {
    ngr <- length(routing$groupes)
    out_ou <- merge(ou, routing$fromId[, .(x,y,gr)], by=c("x","y"))
    ou_gr <- unique(out_ou$gr)
    names(ou_gr) <- ou_gr
    resolution <- routing$resolution
    subsampling <- 0
  }
  else
  {
    ngr <- min(max(8, round(size/chunk)), round(size/1000)) # au moins 8 groupes, au plus des morceaux de 1k
    resolution <- 12.5*2^floor(max(0,log2(sqrt(surf/ngr)/12.5)))
    
    subsampling <- min(max(n_t,floor(resolution/(0.1*tmax*vmaxmode(routing$mode)))),8)
    
    idINS <- idINS3035(ou$x, ou$y, resolution)
    uidINS <- unique(idINS)
    
    out_ou <- ou
    out_ou[, `:=`(gr=idINS)]
    ou_gr <- set_names(as.character(unique(out_ou$gr)))
  }
  log_success("taille:{f2si2(size)} gr:{f2si2(ngr)} res_gr:{resolution}")
  list(ou=out_ou, ou_gr=ou_gr, resINS=resolution, subsampling=subsampling)
}

swap2tmp_routing <- function(routing) {
  if(is.null(routing$groupes))
    return(routing)
  if(!is.null(routing$tempdir))
    return(routing)
  dir <- tempdir()
  routing$time_table <- map_chr(routing$groupes, ~{
    file <- "{dir}/{.x}.csv" %>% glue
    fwrite(routing$time_table[[.x]], file=file)
    file
  })
  routing$tempdir <- dir
  routing
  }

get_routing <- function(routing, groupe) {
  if(is.null(routing$groupes))
    return(routing)
  routing$time_table <- fread(routing$time_table[[groupe]])
  routing
}


vmaxmode <- function(mode)
{
  vitesse <- case_when(
    "TRANSIT"%in%mode ~ 40/60*1000,
    "CAR"%in%mode ~ 80/60*1000,
    "BIKE"%in%mode ~ 12/60*1000,
    "WALK"%in%mode ~ 5/60*1000,
    TRUE ~ 60/60*1000) # vitesse en metre par minute
  vitesse
}

# fonctions pour accessibilite2

select_ancres <- function(s_ou, k, routing)
{
  if(nrow(s_ou)<=1) 
    return(list(les_ou=s_ou, 
                les_ou_s=s_ou$id, 
                error=NULL, 
                result=data.table()))
  
  des_iou <- sample.int(nrow(s_ou), max(1,min(nrow(s_ou)%/%4, k)), replace=FALSE) %>% sort
  les_ou <- s_ou[des_iou]
  les_autres_ou <- s_ou[-des_iou]
  # on calcule les distances entre les points choisis (ancres) et les autres
  ttm_ou <- iso_ttm(o = les_ou, d = les_autres_ou, tmax=100, routing=routing)
  if(!is.null(ttm_ou$error)) 
    {
    log_warn("erreur de routeur ttm_ou {ttm_ou$error}")
    ttm_ou$result <- data.table()
  }
  ttm_ou$les_ou_s <- str_c(les_ou$id, collapse=",")
  ttm_ou$les_ou <- les_ou
  ttm_ou
}

ttm_on_closest <- function(ppou, s_ou, quoi, ttm_0, les_ou, tmax, routing, grdeou)
  {
    ss_ou <- s_ou[!(id%in%les_ou$id)] [closest==ppou]
    if(nrow(ss_ou)>0)
    {
      marge <- max(ss_ou[["tt"]])
      cibles_ok <- ttm_0[fromId==ppou&travel_time<=tmax+marge][["toId"]]
      if(length(cibles_ok)>0)
      { 
        ttm <- iso_ttm(o = ss_ou, d = quoi[cibles_ok], tmax=tmax+1, routing=routing)
        if(is.null(ttm$error))
          if(nrow(ttm$result)>0)
            return(ttm$result[, npea:=length(cibles_ok)] [, npep:=length(unique(toId)), by=fromId])
        else # 0 row ttm
        {
          log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
          log_warn("la matrice des distances et des opportunites (ttm) est vide")
          return(NULL)
        }
        else # ttm$error
        {
          log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
          log_warn("erreur {ttm$error}")
          return(NULL)
        }
      }
      else # cibles_ok lenght nulle
      {
        log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
        log_warn("pas de cibles")
        return(NULL)
      }
    }
    else # ss_ou vide
    {
      log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
      log_warn("pas d'origines")
      return(NULL)
    }
}

get_pid <- function(pids)
{
  cpid <- future:::session_uuid()[[1]]
  if(cpid%in%pids) 
    stringr::str_c("[",which(pids==cpid),"] ")
  else
    ""
}

dt_access_on_groupe <- function(groupe, ou, quoi, routing, tmax, opp_var)
{
  ttm <- iso_ttm(o=ou, d=quoi, tmax=tmax+1, routing=routing)$result
  ttm_d1 <- merge(ttm, quoi, by.x="toId", by.y="id", all.x=TRUE)
  ttm_d <- ttm_d1[, purrr::map(.SD,~sum(., na.rm=TRUE)),
                  by=c("fromId", "travel_time"),
                  .SDcols=(opp_var)]
  ttm_d
}

is.in.dir <- function(groupe, dir)
{
  lf <- list.files(dir)
  str_c(groupe, ".csv")%in%lf
}

access_on_groupe <- function(groupe, ou_4326, quoi_4326, routing, k, tmax, opp_var, ttm_out, pids, dir)
{
  spid <- get_pid(pids)
  
  s_ou <- ou_4326[gr==groupe, .(id, lon, lat, x, y)]
  log_debug("{spid} aog:{groupe} {k} {nrow(s_ou)}")
  
  if(ttm_out&&is.in.dir(groupe, dir))
    {
    log_success("carreau:{groupe} dossier:{dir}")
    return(data.table(file = str_c(dir, "/", groupe, ".csv")))
  }

  if(is.null(routing$ancres))
  {
    tic()
    ttm_ou <- select_ancres(s_ou, k, routing)        
    les_ou_s <- ttm_ou$les_ou_s
    
    if(is.null(ttm_ou$error))
    {
      if(nrow(ttm_ou$result)>0)
        {
        closest <- ttm_ou$result[, .(closest=fromId[which.min(travel_time)],
                                     tt = min(travel_time)),
                                 by=toId][, id:=toId][, toId:=NULL]
        s_ou <- merge(s_ou, closest, by="id")
        delay <- max(s_ou[["tt"]])
      }
      else
      {
        delay <- 0
      }
      log_debug("{spid} ttm_ou:{nrow(ttm_ou$result)}")
      
      # on filtre les cibles qui ne sont pas trop loin selon la distance euclidienne
      quoi_f <- minimax_euclid(from=ttm_ou$les_ou, to=quoi_4326, dist=vmaxmode(routing$mode)*(tmax+delay))
      
      log_debug("{spid} quoi_f:{nrow(quoi_f)}")
      
      # distances entre les ancres et les cibles
      if(any(quoi_f))
        ttm_0 <- iso_ttm(o = ttm_ou$les_ou, 
                          d = quoi_4326[quoi_f],
                          tmax=tmax+delay+3,
                          routing=routing)
      else
        ttm_0 <- list(error=NULL, result=data.table())

      if(!is.null(ttm_0$error)) 
      {
        log_warn("carreau:{groupe} ou_id:{les_ou_s} erreur ttm_0 {ttm_0$error}")
        ttm_0 <- data.table()
      }
      else 
        ttm_0 <- ttm_0$result
      
      if(nrow(ttm_0)>0)
      {
        pproches <- sort(unique(s_ou$closest))
        ttm_0[ , npea:=nrow(quoi_4326)] [, npep:=length(unique(toId)), by=fromId]
        log_debug("{spid} toc {nrow(ttm_0)}")
        
        if(!is.null(pproches))
          {
          # boucle sur les ancres
          ttm <- rbindlist( 
            purrr::map(pproches, 
                function(close)
                  ttm_on_closest(close, s_ou, quoi_4326, ttm_0, ttm_ou$les_ou, tmax, routing, groupe)
                )
            )
          ttm <- rbind(ttm_0[travel_time<=tmax], ttm[travel_time<=tmax])
        }
        else
          ttm <- ttm_0
        
        time <- toc(quiet = TRUE)
        dtime <- (time$toc - time$tic) 
        
        if(nrow(ttm)> 0)
          {
          paires_fromId <- ttm[, .(npep=npep[[1]], npea=npea[[1]]), by=fromId]
          npea <- sum(paires_fromId$npea)
          npep <- sum(paires_fromId$npep)
          
          speed_log <-stringr::str_c(spid,
                            length(pproches),
                            " ancres ", f2si2(npea),
                            "@",f2si2(npea/dtime),"p/s demandees, ",
                            f2si2(npep),"@",f2si2(npep/dtime), "p/s retenues")
        
          if(!ttm_out)
          {         
            ttm_d <- merge(ttm, quoi_4326, by.x="toId", by.y="id", all.x=TRUE)
            ttm_d <- ttm_d[, map(.SD,~sum(., na.rm=TRUE)),
                           by=c("fromId", "travel_time"),
                           .SDcols=(opp_var)]
            ttm_d <- merge(ttm_d, paires_fromId, by="fromId")
            ttm_d[, gr:=groupe]
          }
          else
          {
            ttm_d <- ttm[, .(fromId, toId, travel_time)]
          }
          log_success("{spid} carreau:{groupe} {speed_log}")
        }
        else 
        {
          ttm_d <- NULL
          log_warn("carreau:{groupe} ttm vide") 
        }
      } # close nrow(ttm_0)>0
      else # nrow(ttm_0)==0
      {
        time <- toc(quiet=TRUE)
        log_warn("carreau:{groupe} ou_id:{les_ou_s} ttm_0 vide")
        log_warn("la matrice des distances entre les ancres et les opportunites est vide")
        ttm_d <- NULL
      }
    }
    else  # nrow(ttm_ou)==0
    {
      log_warn("paquet:{groupe} ou_id:{les_ou_s} ttm_ou vide")
      log_warn("la matrice des distances interne au carreau est vide")
      ttm_d <- NULL}
  }
  else # ancres=FALSE pas besoin de finasser, on y va brutal puisque c'est déjà calculé
  {
    ttm <- iso_ttm(o=s_ou, d=quoi_4326, tmax=tmax+1, routing=routing)$result
    ttm_d1 <- merge(ttm, quoi_4326, by.x="toId", by.y="id", all.x=TRUE)
    ttm_d <- ttm_d1[, purrr::map(.SD,~sum(., na.rm=TRUE)),
                   by=c("fromId", "travel_time"),
                   .SDcols=(opp_var)]
    ttm_d2 <- ttm_d1[, .(npea=.N, npep=.N), by=fromId] [, gr:=groupe]
    ttm_d <- merge(ttm_d, ttm_d2, by="fromId")
  }
  
  if(ttm_out)
  {
    file <- stringr::str_c(dir,"/", groupe, ".csv")
    data.table::fwrite(ttm_d, file)
    ttm_d <- data.table::data.table(file=file)
  }
 ttm_d
}

minimax_euclid <- function(from, to, dist)
{
  m_from <- from[, .(x,y)] %>% as.matrix()
  m_to <- to[, .(x,y)] %>% as.matrix()
  dfrom2to <- rdist::cdist(m_from, m_to)
  dmin2to <- matrixStats::colMins(dfrom2to)
  dmin2to<=dist
}

# routing engines ---------------------


iso_ttm <- function(o, d, tmax, routing)
{
  log_debug("iso_ttm:{tmax} {nrow(o)} {nrow(d)}")
  r <- switch(routing$type,
         "r5" = r5_ttm(o, d, tmax, routing),
         "otpv1" = otpv1_ttm(o, d, tmax, routing),
         "osrm" = osrm_ttm(o, d, tmax, routing),
         "dt"= dt_ttm(o, d, tmax, routing))
  log_debug("result:{nrow(r$result)}")
  r
  }

safe_ttm <- function(routing)
{
  switch(routing$type,
         "r5" = r5_ttm,
         "otpv1" = otpv1_ttm,         
         "osrm" = osrm_ttm,
         "dt"= dt_ttm)
}

delayRouting <- function(delay, routing)
{
  switch(routing$type,
         "r5" = {
           res <- routing
           res$departure_datetime <-
             as.POSIXct(routing$departure_datetime+delay*60,
                        format = "%d-%m-%Y %H:%M:%S")
           res},
         "otpv1" = {routing}, # à faire
         "osrm" = {routing}, # pas d'heure de départ
         "data.table"= {routing}) # à faire
}

r5_ttm <- function(o, d, tmax, routing)
{
  o <- o[, .(id=as.character(id),lon,lat)]
  d <- d[, .(id=as.character(id),lon,lat)]
  safe_ttm <- purrr::safely(r5r::travel_time_matrix)
  
  res <- safe_ttm(
    r5r_core = routing$core,
    origins = o,
    destinations = d,
    mode=routing$mode,
    departure_datetime = routing$departure_datetime,
    max_walk_dist = routing$max_walk_dist,
    max_trip_duration = tmax+1,
    time_window = as.integer(routing$time_window),
    percentiles = routing$percentile,
    walk_speed = routing$walk_speed,
    bike_speed = routing$bike_speed,
    max_rides = routing$max_rides,
    n_threads = routing$n_threads,
    verbose=FALSE)
  
  if(!is.null(res$error))
  {
    gc()
    res <- safe_ttm(
      r5r_core = routing$core,
      origins = o,
      destinations = d,
      mode=routing$mode,
      departure_datetime = routing$departure_datetime,
      max_walk_dist = routing$max_walk_dist,
      max_trip_duration = tmax+1,
      time_window = as.integer(routing$time_window),
      percentiles = routing$percentile,
      walk_speed = routing$walk_speed,
      bike_speed = routing$bike_speed,
      max_rides = routing$max_rides,
      n_threads = routing$n_threads,
      verbose=FALSE)
    if(is.null(res$error)) log_warn("second r5::travel_time_matrix ok")
  }
  
  if (is.null(res$error)&&nrow(res$result)>0)
    res$result[, `:=`(fromId=as.integer(fromId), toId=as.integer(toId), travel_time=as.integer(travel_time))]
  else
    log_warn("erreur r5::travel_time_matrix, retourne une matrice vide après 2 essais")
  res
}

otpv1_ttm <- function(o, d, tmax, routing)
{
  # ca marche pas parce que OTP ne renvoie pas de table
  # du coup il faudrait faire ça avec les isochrones
  # ou interroger OTP paire par paire
  # la solution ici est très très lente et donc pas utilisable
  
  o[, `:=`(k=1, fromId=id, fromlon=lon, fromlat=lat)]
  d[, `:=`(k=1, toId=id, tolon=lon, tolat=lat)]
  paires <- merge(o,d, by="k", allow.cartesian=TRUE)
  temps <- furrr::future_map_dbl(1:nrow(paires), ~{
    x <- paires[.x, ]
    t <- otpr::otp_get_times(
      routing$otpcon,  
      fromPlace= c(x$fromlat, x$fromlon),
      toPlace= c(x$tolat, x$tolon), 
      mode= routing$mode,
      date= routing$date,
      time= routing$time,
      maxWalkDistance= routing$maxWalkDistance,
      walkReluctance = routing$walkReluctance,
      arriveBy = routing$arriveBy,
      transferPenalty = routing$transferPenalty,
      minTransferTime = routing$minTransferTime,
      detail = FALSE,
      includeLegs = FALSE)
    if(t$errorId=="OK") t[["duration"]]
    else NA
  })
  paires[ , .(fromId, toId)] [, temps:=as.integer(temps)]
}

osrm_ttm <- function(o, d, tmax, routing)
{
  options(osrm.server = routing$osrm.server, 
          osrm.profile = routing$osrm.profile)
  # safe_table <- safely(osrm::osrmTable)
  l_o <- o[, .(id, lon, lat)]
  l_d <- d[, .(id, lon, lat)]
  # if(routing$future)
  #   {
  #   ptable <- future_map(1:nrow(o), function(i) {
  #     options(osrm.server = routing$osrm.server, 
  #             osrm.profile = routing$osrm.profile)
  #     osrm::osrmTable(
  #       src = l_o[i,],
  #       dst= l_d,
  #       exclude=NULL,
  #       gepaf=FALSE,
  #       measure="duration")
  #     },.options=furrr_options(seed=TRUE, 
  #                              packages=c("data.table", "osrm", "sf", "utils", "RCurl", "jsonlite", "purrr")))
  #   
  #   table <- NULL
  #   table$error <- NULL
  #   table$result$duration <- do.call(rbind, map(ptable, ~.$duration))
  #   }
  # else
  # {
  table <- list(
    result = osrm::osrmTable(
      src = l_o,
      dst= l_d,
      exclude=NULL,
      gepaf=FALSE,
      measure="duration"),
    error =NULL)
  # }
  
  if(is.null(table$error))
  {
    dt <- data.table(table$result$duration, keep.rownames = TRUE)
    dt[, fromId:=rn %>% as.integer] [, rn:=NULL]
    dt <- melt(dt, id.vars="fromId", variable.name="toId", value.name = "travel_time", variable.factor = FALSE)
    dt <- dt[travel_time<tmax,]
    dt[, `:=`(toId = as.integer(toId), travel_time = as.integer(ceiling(travel_time)))]
    table$result <- dt
  }
  table
}

dt_ttm <- function(o, d, tmax, routing)
{
  o_rid <- merge(o[, .(oid=id, x, y)], routing$fromId[, .(rid=id, x, y)], by=c("x", "y"))
  d_rid <- merge(d[, .(did=id, x, y)], routing$toId[, .(rid=id, x, y)], by=c("x", "y"))
  ttm <- routing$time_table[(fromId%in%o_rid$rid), ][(toId%in%d_rid$rid),][(travel_time<tmax), ]
  ttm <- merge(ttm, o_rid[, .(oid, fromId=rid)], by="fromId")
  ttm <- merge(ttm, d_rid[, .(did, toId=rid)], by="toId")
  ttm <- ttm[, `:=`(fromId=NULL, toId=NULL)]
  setnames(ttm,old=c("oid", "did"), new=c("fromId", "toId"))
  list(
    error=NULL,
    result=ttm
  )
}

routing_setup_r5 <- function(path,
                             date="17-12-2019 8:00:00",
                             mode=c("WALK", "TRANSIT"),
                             montecarlo=1L,
                             max_walk_dist= Inf,
                             time_window=1L,
                             percentiles=50L,
                             walk_speed = 5.0,
                             bike_speed = 12.0,
                             max_rides= 5L,
                             n_threads= parallel::detectCores(logical=FALSE))
{
  env <- parent.frame()
  path <- glue::glue(path, .envir = env)
  mode_string <- stringr::str_c(mode, collapse = "&")
  r5r::stop_r5()
  core <- r5r::setup_r5(data_path = path, verbose=FALSE)
  core$setNumberOfMonteCarloDraws(as.integer(montecarlo))
  mtnt <- lubridate::now()
  list(
    type = "r5",
    string=glue::glue("r5 routing {mode_string} sur {path} à {mtnt}"),
    core = core,
    montecarlo = as.integer(montecarlo),
    time_window = as.integer(time_window),
    departure_datetime = as.POSIXct(date, format = "%d-%m-%Y %H:%M:%S", tz=Sys.timezone()),
    mode=mode,
    percentiles=percentiles,
    max_walk_dist = max_walk_dist,
    walk_speed=walk_speed,
    bike_speed=bike_speed,
    max_rides=max_rides,
    n_threads=as.integer(n_threads),
    future=FALSE)
}

getr5datafromAzFS <- function(jeton_sas, path="IDFM", endpoint="https://totostor.file.core.windows.net")
{
  fl_endp_sas <- AzureStor::storage_endpoint(endpoint, sas=jeton_sas)
  cont <- AzureStor::storage_container(fl_endp_sas, "timbsmb")
  AzureStor::storage_multidownload(cont, src= glue::glue("{path}/*.*") , dest=glue::glue("{path}/" , overwrite=TRUE))
}

routing_setup_otpv1 <- function(
  router,
  port=8000,
  memory="8G",
  rep=DVFdata,
  date="12-17-2019 8:00:00",
  mode=c("WALK", "TRANSIT"),
  max_walk_dist= 2000,
  precisionMeters=50)
  
{
  mode_string <- str_c(mode, collapse = "&")
  list(
    type = "otpv1",
    string="otpv1 routing {mode_string} sur {router}(:{port}) à {now()}" %>% glue,
    otpcon = OTP_server(router=router, port=port, memory = memory, rep=rep),
    date = unlist(str_split(date, " "))[[1]],
    time= unlist(str_split(date, " "))[[2]],
    mode=mode,
    batch = FALSE,
    arriveBy = FALSE, 
    walkReluctance= 2, 
    maxWalkDistance= max_walk_dist,
    transferPenalty = 0, 
    minTransferTime = 0,
    clampInitialWait= 0,
    offRoadDistanceMeters=50,
    precisionMeters=precisionMeters, 
    future=FALSE)
}

routing_setup_osrm <- function(
  server=5000,
  profile="driving",
  future=TRUE)
  
{
  list(
    type = "osrm",
    string="osrm routing localhost:{server} profile {profile} à {now()}" %>% glue,
    osrm.server = "http://localhost:{server}/" %>% glue,
    osrm.profile=profile,
    future=TRUE,
    mode=switch(profile,
                driving="CAR",
                walk="WALK",
                bike="BIKE"))
}

# Illustrations---------------------

r5_isochrone <- function(lon, lat,                         # en coordonnées lon, lat
                         resolution= 50,
                         r5_core,                      # pointeur sur le core r5 utilisé ou chemin vers le dosssier
                         mode=c("WALK", "TRANSIT"),    # TRANSIT, CAR, WALK etc...
                         date="17-12-2019 8:00:00",    # au format = "%d-%m-%Y %H:%M:%S"
                         max_walk_dist=2000,           # en mètres
                         temps_max=10L,                # en minutes
                         time_window=1L,
                         montecarlo=1L,
                         plot=FALSE,
                         nthreads=parallel::detectCores(logical=FALSE))
{
  tic()

  if(is.character(r5_core)) {
    Message("Lecture du schéma géographique")
    core <- setup_r5(data_path =r5_core)
  }
  
  else core <- r5_core
  assertthat::assert_that("jobjRef"%in% class(core))
  
  r5_core$setNumberOfMonteCarloDraws(as.integer(montecarlo))
  
  vitesse <- vmaxmode(mode)
  
  origin  <- tibble(lon=lon, lat=lat, id="1")
  
  origin_sf <- origin %>% st_as_sf(coords=c("lon", "lat"), crs=4326)
  destination_sf <- origin_sf %>%  st_transform(3035) %>% st_buffer(temps_max*vitesse)
  destination <- fasterize(destination_sf, raster_ref(destination_sf, resolution), fun="any")
  xy <- raster::xyFromCell(destination, which(raster::values(destination) == 1)) 
  xy_4326 <- sf_project(pts=xy,from=st_crs(3035), to=st_crs(4326))
  destinations <- data.table(lon=xy_4326[,1], lat=xy_4326[,2],
                             x = xy[,1], y=xy[,2],
                             id=as.character(1:nrow(xy)))
  
  ttm <- travel_time_matrix(core,
                            origins = origin,
                            destinations = destinations,
                            mode=mode,
                            departure_datetime = as.POSIXct(date, format = "%d-%m-%Y %H:%M:%S"),
                            max_walk_dist = max_walk_dist,
                            max_trip_duration = temps_max+1,
                            time_window = as.integer(time_window),
                            percentiles = 50L,
                            walk_speed = 3.6,
                            bike_speed = 12,
                            max_rides = 3,
                            n_threads = nthreads)
  if(nrow(ttm)>0)
  {
    ttm <- merge(ttm, destinations, by.x="toId", by.y="id", all.x=TRUE)
    raster <- rasterize(cbind(ttm$x, ttm$y), destination, field=ttm$travel_time)
  }
  if(plot)
  {
    map <- 
      tm_shape(tmaptools::read_osm(destination_sf, type="osm"))+tm_rgb(saturation = 0)+
      tm_shape(raster)+tm_raster(style="cont", palette=heatvb, alpha=0.5)
    print(map)
  }
  invisible(raster)
}
