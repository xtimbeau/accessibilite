# changement de l'algorithme 
# la sélection des paires s'effectue au coeur du calcul et repose sur la majoration
# par le calcul des distances entre les destinations du paquet

iso_accessibilite <- function(
  quoi,                            # sf avec des variables numériques qui vont être aggrégées
  ou=NULL,                         # positions sur lesquelles sont calculés les accessibilités (si NULL, sur une grille)
  res_quoi=Inf,                    # projection éventuelle des lieux sur une grille
  resolution=ifelse(is.null(ou), 200, Inf),
  var_quoi="individuals",          # si projection fonction d'agrégation
  routing,                         # défini le moteur de routage
  tmax=10L,                        # en minutes
  pdt=1L,
  chunk=5000000,                   # paquet envoyé
  future=TRUE,                  
  out=ifelse(is.finite(resolution), resolution, "raster"),
  ttm_out= FALSE, 
  logs = localdata,
  dir= NULL,
  table2disk=if(!is.null(dir)) TRUE else FALSE)                        # ne recalcule pas les groupes déjà calculés, attention !  
{
  library("logger", quietly=TRUE)
  library("data.table", quietly=TRUE)
  library("magrittr", quietly=TRUE)
  library("glue", quietly=TRUE)
  
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
  
  dir.create("{logs}/logs" %>% glue, showWarnings = FALSE, recursive=TRUE)
  timestamp <- lubridate::stamp("15-01-20 10h08.05", orders = "dmy HMS", quiet=TRUE) (lubridate::now())
  logfile <- glue::glue("{logs}/logs/iso_accessibilite.{routing$type}.{timestamp}.log")
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
  quoi_4326 <- ouetquoi$quoi_4326                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <- ouetquoi$quoi_4326
  
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
  log_success("{length(ou_gr)} carreaux, {k} subsampling")
  
  if(table2disk)
    if(is.null(dir))
      {
      dir <- tempdir()
      purrr::walk(ou_gr, ~file.remove(str_c(dir,"/", .x,".*")))
      }
  else
    if (!dir.exists(dir))
      dir.create(dir)
  
  message("...calcul des temps de parcours")

  pb <- progressr::progressor(steps=sum(groupes$Nous))
  if(routing$future&future)
  {
    pl <- future::plan()
    future::plan(pl)
    pids <- furrr::future_map(
      1:(future::nbrOfWorkers()), 
      ~future:::session_uuid()[[1]])
    lt <- log_threshold()
    access <- furrr::future_map(
      ou_gr,
      function(g) {
        log_threshold(lt)
        log_appender(logger::appender_file(logfile))
        pb(amount=groupes$Nous[[g]])
        rrouting <- get_routing(routing, g)
        access_on_groupe(g, ou_4326, quoi_4326, rrouting, k, tmax, opp_var, ttm_out, pids, dir, t2d=table2disk)
      },
      .options=furrr::furrr_options(seed=TRUE, 
                                      packages=c("data.table", "logger", "stringr", "glue"),
                                      scheduling = 1))
  }
  else
  {
    pids <- future:::session_uuid()[[1]]
    access <- purrr::map(ou_gr, function(g) {
      pb(amount=groupes$Nous[[g]])
      rrouting <- get_routing(routing, g)
      access_on_groupe(g, ou_4326, quoi_4326, rrouting, k, tmax, opp_var, ttm_out, pids, dir, t2d=table2disk)
    })
  }
  
  access <- data.table::rbindlist(access)

  
  if(ttm_out)
  {
    message("...finalisation du routing engine")
    gc()
    pl <- future::plan()
    future::plan(pl) # pour reprendre la mémoire
    access <- purrr::map(access$file,~{
      tt <- qs::qread(.x, nthreads=4)
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
      fromId = ou_4326[, .(id, lon, lat, x, y, gr)],
      toId = quoi_4326[, .(id, lon, lat, x, y)], 
      groupes=ou_gr,
      resolution=groupes$resINS,
      res_ou = resolution,
      res_quoi = res_quoi,
      ancres=FALSE, 
      future=TRUE, 
      mode=routing$mode)
    }
  else
    {
      if(table2disk)
        access <- rbindlist(
          purrr::map(access$file,~qs::qread(.x, nthreads=4)))
      
      npaires <- sum(access[, .(npaires=npep[[1]]), by=fromId][["npaires"]])
      access[, `:=`(npea=NULL, npep=NULL)]
      
      groupes_ok <- unique(access$gr)
      groupes_pasok <- setdiff(ou_gr, groupes_ok)
      ou_pasok <- ou_4326[gr%chin%groupes_pasok, .N]
      
      log_warn("{ou_pasok} ({signif(ou_pasok/nrow(ou_4326)*100, 1)}%) origines non evaluees ")
      access[,gr:=NULL]
      message("...cumul")
      setnames(access, new="temps", old="travel_time")
      timecrossou <- CJ(fromId=ou_4326$id,temps=c(0:tmax), sorted=FALSE)
      access <- merge(timecrossou, access, by=c("fromId", "temps"), all.x=TRUE)
      for (v in opp_var) 
        set(access, i=which(is.na(access[[v]])), j=v, 0)
      setorder(access, fromId, temps)
      access_c <- access[, lapply(.SD, cumsum),
                         by=fromId,
                         .SDcols=opp_var]
      temps_c <- access[, .(temps=temps),by=fromId] 
      access_c[,temps:=temps_c$temps]
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
        sf = access_c %>%
          dplyr::as_tibble() %>%
          sf::st_as_sf(coords=c("x","y"), crs=3035),
        raster = {
          message("...rastérization")
          ttn <- stringr::str_c("iso",tt, "m")
          ids <- idINS3035(ou_4326$x, ou_4326$y, resolution = outr)
          ids <- data.table(fromId=ou_4326$id, idINS200=ids)
          purrr::map(opp_var, function(v) {
            r_xy <- data.table::dcast(access_c, fromId~temps, value.var=v)
            names(r_xy) <- c("fromId", ttn)
            r_xy <- merge(r_xy, ids, by="fromId")[, fromId:=NULL]
            dt2r(r_xy, resolution=outr)
          })})
      dtime <- as.numeric(Sys.time()) - as.numeric(start_time)  
      red <- 100*(npaires_brut-npaires)/npaires_brut
      tmn <- second2str(dtime)
      speed_b <- npaires_brut/dtime
      speed <- npaires/dtime
      mtime <-glue::glue("{tmn} - {f2si2(npaires)} routes - {f2si2(speed_b)} routes(brut)/s - {f2si2(speed)} routes/s - {signif(red,2)}% reduction")
      message(mtime)
      log_success("{routing$string} en {mtime}")
      attr(res, "routing")<- glue::glue("{routing$string} en {mtime}")
    }
  message("...nettoyage")
  plan(plan())
  gc()
  res
  }

# fonctions internes ----------------

# iso_ouetquoi projette sur 4326 les coordonnées et fabrique les grilles nécessaires en donnant en sortie les ou et quoi utilisés pour ttm

iso_ouetquoi_4326 <- function(ou, quoi, res_ou, res_quoi, opp_var, fun_quoi="mean", resolution=res_quoi, rf=5)
{
  library("fasterize", quietly=TRUE)
  library("sf", quietly=TRUE)
  
  # projection éventuelle sur une grille 3035 à la résolution res_quoi ou resolution
  if (!("sfc_POINT" %in% class(st_geometry(quoi)))|is.finite(res_quoi))
  {
    if(!is.finite(res_quoi)) 
      res_quoi <- resolution
    
    if("sfc_POINT" %in% class(st_geometry(quoi))) 
    {
      rf <- 1
      qxy <- quoi %>%
        st_transform(3035) %>% 
        st_coordinates()
      qins <- idINS3035(qxy, resolution=res_quoi, resinst=FALSE)
      qag <- quoi %>% 
        st_drop_geometry() %>% 
        as.data.frame() %>%
        as.data.table()
      qag <- qag[, id:=qins] [, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=id, .SDcols=opp_var]
      qag[, geometry:=idINS2square(qag$id, resolution=res_quoi)]
      quoi <- st_as_sf(qag) 
      }
    
    quoi <- quoi %>% st_transform(3035)
    quoi_surf <- st_area(quoi) %>% as.numeric()
    gc()
    rr_3035 <- 
      raster::brick(
        purrr::map(
          opp_var,
          ~{
            if(!is.na(st_agr(quoi)[[.x]])&st_agr(quoi)[[.x]]=="constant")
              {
              fonction <- mean
              facteur <- 1
              }
            else 
            {
              fonction <- sum
              facteur <- (resolution/rf)^2/quoi_surf
              }
            if(rf>1)
              rref <- raster::disaggregate(raster_ref(quoi, resolution), fact=rf)
            else
              rref <- raster_ref(quoi, resolution)
            un_raster <- 
              fasterize::fasterize(
                quoi %>% dplyr::mutate(field = get(.x)*facteur),
                rref,
                fun="sum",
                background=0,
                field="field")
            if(rf>1) 
              un_raster <- raster::aggregate(un_raster, fact=rf, fun=fonction)
            un_raster 
          }))
    
    xy_3035 <- coordinates(rr_3035)
    quoi_3035 <- data.table(rr_3035 %>% as.data.frame(),
                            x=xy_3035[,1] %>% round(), 
                            y=xy_3035[,2]%>% round())
    quoi_4326 <- quoi_3035 %>%
      as.data.table()
    keep <- purrr::reduce(
      purrr::map(opp_var, ~{
      xx <- quoi_4326[[.x]]
      (!is.na(xx))&(xx!=0)}),
      `|`)
    quoi_4326 <- quoi_4326[keep,]
    xy_3035 <-quoi_4326[, .(x,y)] %>% as.matrix
    xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
    rm(rr_3035)
    gc()
  }
  else # !point ou finite(resquoi)
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
                   y= xy_3035[,2] %>% round())]
  quoi_4326[, id := .I]
  setkey(quoi_4326, id)
  
  if (is.null(ou))
  {
    # pas de ou, on le prend égal à quoi en forme
    ou_3035 <- raster_ref(quoi %>% st_transform(3035), resolution = res_ou)
    ncells <- 1:(ou_3035@ncols * ou_3035@nrows)
    xy_3035 <-  xyFromCell(ou_3035, ncells)
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
      rr_3035 <- fasterize(ou_3035 %>% st_sf(), raster_ref(ou_3035, res_ou), fun = "any")
      xy_3035 <- xyFromCell(rr_3035, which(raster::values(rr_3035) == 1))
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
  library("matrixStats", quietly=TRUE)
  
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
    
    idINS <- idINS3035(ou$x, ou$y, resolution, resinstr = FALSE)
    uidINS <- unique(idINS)
    
    out_ou <- ou
    out_ou[, `:=`(gr=idINS)]
    ou_gr <- set_names(as.character(unique(out_ou$gr)))
  }
  Nous <- out_ou[, .N, by=gr]
  Nous <- set_names(Nous$N, Nous$gr)
  log_success("taille:{f2si2(size)} gr:{f2si2(ngr)} res_gr:{resolution}")
  list(ou=out_ou, ou_gr=ou_gr, resINS=resolution, subsampling=subsampling, Nous=Nous)
}

swap2tmp_routing <- function(routing, qs=TRUE) {
  library("qs", quietly=TRUE)
  library("data.table", quietly=TRUE)
  library("purrr", quietly=TRUE)
  if(is.null(routing$groupes))
    return(routing)
  if(!is.null(routing$tempdir))
    return(routing)
  dir <- tempdir()
  pb <- progressr::progressor(along=routing$groupes)
  routing$time_table <- map_chr(routing$groupes, ~{
    pb()
    file <- if(qs) 
      "{dir}/{.x}.rda" %>% glue
    else 
      "{dir}/{.x}.csv" %>% glue
    if(qs)
      qsave(routing$time_table[[.x]], file=file, preset="fast", nthreads=4)
    else 
      fwrite(routing$time_table[[.x]], file=file)
    file
  })
  routing$tempdir <- dir
  gc()
  routing
  }

get_routing <- function(routing, groupe) {
  library("data.table", quietly=TRUE)
  
  if(is.null(routing$groupes))
    return(routing)
  file <- routing$time_table[[groupe]]
  ext <- stringr::str_extract(file, "(?<=\\.)[:alnum:]*(?!\\.)")
  if (ext=="csv")
    routing$time_table <- data.table::fread(file)
  else
    routing$time_table <- qs::qread(file, nthreads=4)
  routing$groupes=groupe
  routing
}

vmaxmode <- function(mode)
{
  vitesse <- case_when(
    "TRANSIT"%in%mode ~ 40/60*1000,
    "RAIL"%in%mode ~ 40/60*1000,
    "BUS"%in%mode ~ 15/60*1000,
    "CAR"%in%mode ~ 60/60*1000,
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
    null_result <- data.table(fromId=numeric(),
                              toId=numeric(),
                              travel_time=numeric(),
                              npea=numeric(),
                              npep=numeric())
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
          return(null_result)
        }
        else # ttm$error
        {
          log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
          log_warn("erreur {ttm$error}")
          return(null_result)
        }
      }
      else # cibles_ok lenght nulle
      {
        log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
        log_warn("pas de cibles")
        return(null_result)
      }
    }
    else # ss_ou vide
    {
      log_warn("paquet:{grdeou} ppou:{ppou} ttm vide")
      log_warn("pas d'origines")
      return(null_result)
    }
}

get_pid <- function(pids)
{
  library("future", quietly=TRUE)
  library("stringr", quietly=TRUE)
  
  cpid <- future:::session_uuid()[[1]]
  if(cpid%in%pids) 
    stringr::str_c("[",which(pids==cpid),"]")
  else
    ""
}

dt_access_on_groupe <- function(groupe, ou, quoi, routing, tmax, opp_var)
{
  library("purrr", quietly=TRUE)
  library("data.table", quietly=TRUE)
  ttm <- iso_ttm(o=ou, d=quoi, tmax=tmax+1, routing=routing)$result
  ttm_d1 <- merge(ttm, quoi, by.x="toId", by.y="id", all.x=TRUE)
  ttm_d <- ttm_d1[, map(.SD,~sum(., na.rm=TRUE)),
                  by=c("fromId", "travel_time"),
                  .SDcols=(opp_var)]
  ttm_d
}

is.in.dir <- function(groupe, dir)
{
  library("stringr", quietly=TRUE)
  lf <- list.files(dir)
  str_c(groupe, ".rda")%in%lf
}

access_on_groupe <- function(groupe, ou_4326, quoi_4326, routing, k, tmax, opp_var, ttm_out, pids, dir, t2d)
{
  library("data.table", quietly=TRUE)
  library("tictoc", quietly=TRUE)
  library("purrr", quietly=TRUE)
  library("stringr", quietly=TRUE)
  library("qs", quietly=TRUE)
  
  spid <- get_pid(pids)
  
  s_ou <- ou_4326[gr==groupe, .(id, lon, lat, x, y)]
  log_debug("{spid} aog:{groupe} {k} {nrow(s_ou)}")
  
  if(t2d&&is.in.dir(groupe, dir))
    {
    log_success("{spid} carreau:{groupe} dossier:{dir}")
    return(data.table(file = str_c(dir, "/", groupe, ".rda")))
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
        log_warn("{spid} carreau:{groupe} ou_id:{les_ou_s} erreur ttm_0 {ttm_0$error}")
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
          
          speed_log <-stringr::str_c(length(pproches),
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
          log_warn("{spid} carreau:{groupe} ttm vide") 
        }
      } # close nrow(ttm_0)>0
      else # nrow(ttm_0)==0
      {
        time <- toc(quiet=TRUE)
        log_warn("{spid} carreau:{groupe} ou_id:{les_ou_s} ttm_0 vide")
        log_warn("la matrice des distances entre les ancres et les opportunites est vide")
        ttm_d <- NULL
      }
    }
    else  # nrow(ttm_ou)==0
    {
      log_warn("{spid} carreau:{groupe} ou_id:{les_ou_s} ttm_ou vide")
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
  
  if(t2d)
  {
    file <- stringr::str_c(dir,"/", groupe, ".rda")
    qs::qsave(ttm_d, file, preset="fast", nthreads = 4)
    ttm_d <- data.table::data.table(file=file)
  }
 ttm_d
}

minimax_euclid <- function(from, to, dist)
{
  library("matrixStats", quietly=TRUE)
  library("rdist", quietly=TRUE)
  m_from <- from[, .(x,y)] %>% as.matrix()
  m_to <- to[, .(x,y)] %>% as.matrix()
  dfrom2to <- rdist::cdist(m_from, m_to)
  dmin2to <- matrixStats::colMins(dfrom2to)
  dmin2to<=dist
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
  library("tictoc", quietly=TRUE)
  library("assertthat", quietly=TRUE)
  library("r5r", quietly=TRUE)
  library("fasterize", quietly=TRUE)
  library("raster", quietly=TRUE)
  library("sf", quietly=TRUE)
  
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
