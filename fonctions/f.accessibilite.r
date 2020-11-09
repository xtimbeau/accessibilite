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
  chunk=10000,                    # paquet envoyé à r5
  future=TRUE,                  
  out=ifelse(is.finite(resolution), resolution, "raster"),
  ttm_out= FALSE)
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
  
  dir.create("{DVFdata}/logs" %>% glue, showWarnings = FALSE)
  timestamp <- lubridate::stamp("15-01-20 10h08.05", orders = "dmy HMS", quiet=TRUE) (lubridate::now())
  logfile <- "{DVFdata}/logs/iso_accessibilite.{routing$type}.{timestamp}.log" %>% glue
  # logfile <- function() stringr::str_c(logfile_s,future:::session_uuid()[[1]],".log")
  log_appender(appender_file(logfile))
  
  log_info("Calcul accessibilité version 2")
  log_info("{capture.output(show(routing))}")
  log_info("")
  log_info("tmax:{tmax}")
  log_info("pdt:{pdt}")
  log_info("chunk:{chunk}")
  log_info("resolution:{resolution}")
  log_info("future:{future}")
  log_info("out:{out}")
  
  opp_var <- names(quoi %>% as_tibble %>% select(where(is.numeric)))
  if(length(opp_var)==0)
  {
    opp_var <- "c"
    quoi <- quoi %>% mutate(c=1)
  }
  
  names(opp_var) <- opp_var
  
  log_info("les variables sont {c(opp_var)}")
  
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
    mode=routing$mode,
    tmax=tmax)
  
  ou_4326 <- groupes$ou
  ou_gr <- groupes$ou_gr
  
  log_info("{nrow(ou_4326)} pour ou, {nrow(quoi_4326)} pour quoi")
  log_info("{length(ou_gr)} groupes de ou")
  
  message("...calcul des temps de parcours")
  if(routing$future&future)
    map_fun <- function(x,y) furrr::future_map(x,y, .options=furrr_options(seed=TRUE, packages=c("data.table", "glue")))
  else 
    map_fun <- purrr::map
  
  k <- if(!is.null(routing$n_threads)) routing$n_threads else 1
  
  with_progress({
    pb <- progressor(steps=length(ou_gr))
    access <- 
      rbindlist(
        map_fun(
          ou_gr, 
          function(groupe) {
            pb()
            logger::log_appender(appender_file(logfile))
            access_on_groupe(groupe, ou_4326, quoi_4326, routing, k, tmax, opp_var, ttm_out)
          }
        )
      )
  },
  handlers=handler_progress(format=":bar :percent :eta", width=80))
  
  npaires <- sum(access[, .(npaires=npep[[1]]), by=fromId][["npaires"]])
  access[, `:=`(npea=NULL, npep=NULL)]
  
  groupes_ok <- unique(access$gr)
  groupes_pasok <- setdiff(ou_gr, groupes_ok)
  ou_pasok <- ou_4326[gr%chin%groupes_pasok, .N]
  
  log_warn("{ou_pasok} ({signif(ou_pasok/nrow(ou_4326)*100, 1)}%) origines non evaluees ")
  
  if(!ttm_out)
  {
    access[,gr:=NULL]
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
    
    if(is.numeric(out)) {
      outr <- resolution
      out <- "raster"
    }
    
    res <- switch(
      out,
      data.table = access_c,
      sf = access_c %>% as_tibble %>% st_as_sf(coords=c("x","y"), crs=3035),
      raster = {
        message("...rastérization")
        r_xy <- access_c[, .(x=x[[1]], y=y[[1]]), by=fromId] [, fromId:=NULL]
        if (!is.null(ou))
          r <- raster_ref(ou %>% st_transform(3035), outr)
        else
          r <- raster_ref(quoi %>% st_transform(3035), outr)
        map(opp_var, function(v) {
          brick(
            map(tt, ~{
              rz <- rasterize(r_xy, r, field=access_c[temps==.x, .SD, .SDcols=v])
              names(rz) <- str_c("iso",.x, "m")
              rz}))})
      })
  }
  else # ttm_out
  {
    res <- list(
      type = "dt",
      origin = routing$type,
      origin_string = routing$string,
      string = "matrice de time travel {routing$type} precalculee" %>% glue,
      time_table = rbind(access[, .(fromId, toId, travel_time)]),
      fromId = ou_4326[, .(id, lon, lat)],
      toId = quoi_4326[, .(id, lon, lat)], 
      ancres=FALSE, 
      future=TRUE
    )
  }
  
  dtime <- as.numeric(Sys.time()) - as.numeric(start_time)
  red <- 100*(npaires_brut-npaires)/npaires_brut
  tmn <- second2str(dtime)
  speed_b <- npaires_brut/dtime
  speed <- npaires/dtime
  mtime <- "{tmn} - {f2si2(npaires)} routes - {f2si2(speed_b)} routes(brut)/s - {f2si2(speed)} routes/s - {signif(red,2)}% reduction" %>% glue()
  message(mtime)
  log_info("{routing$string} en {mtime}")
  res %@% routing <- ("{routing$string} en {mtime}" %>% glue)
  res
}

# fonctions internes ----------------

# iso_ouetquoi projette sur 4326 les coordonnées et fabrique les grilles nécessaires en donnant en sortie les ou et quoi utilisés pour ttm

iso_ouetquoi_4326 <- function(ou, quoi, res_ou, res_quoi, opp_var, fun_quoi="any", resolution=res_quoi)
{
  # projection éventuelle sur une grille 3035 à la résolution res_quoi ou resolution
  if (!("sfc_POINT" %in% class(st_geometry(quoi)))|is.finite(res_quoi))
  {
    if((!is.finite(res_quoi))) 
      res_quoi <- resolution
    if("sfc_POINT" %in% class(st_geometry(quoi))) 
      quoi <- quoi %>% st_buffer(resolution/5)
    
    quoi <- quoi %>% st_transform(3035)
    rf <- 3
    gc()
    rr_3035 <- 
      brick(
        map(
          opp_var,
          ~(
            fasterize(
              quoi,
              raster_ref(quoi, resolution/rf), 
              fun=fun_quoi,
              background=0L,
              field=.x))))
    rr_3035 <- aggregate(rr_3035, fact=rf, fun=mean)
    xy_3035 <- raster::coordinates(rr_3035)
    quoi_3035 <- data.table(rr_3035 %>% as.data.frame(), x=xy_3035[,1], y=xy_3035[,2])
    quoi_4326 <- quoi_3035 %>% as.data.table
    keep <- purrr::reduce(
      map(opp_var, ~{
      xx <- quoi_4326[[.x]]
      (!is.na(xx))&(xx!=0)}),
      and)
    quoi_4326 <- quoi_4326[keep,]
    xy_3035 <-quoi_4326[, .(x,y)] %>% as.matrix
    xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
    gc()
  }
  else
  {
    xy_3035 <- st_coordinates(quoi %>% st_transform(3035))
    xy_4326 <- st_coordinates(quoi %>% st_transform(4326))
    quoi_4326 <- quoi %>% as_tibble %>% select(all_of(opp_var)) %>% as.data.table
  }
  quoi_4326[, `:=`(lon = xy_4326[, 1], lat = xy_4326[, 2],
                   x= xy_3035[,1], y=xy_3035[,2])]
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
        x = xy_3035[, 1],
        y = xy_3035[, 2]
      )
  }
  else {
    # un ou
    if (is.finite(res_ou)&&!("sfc_POINT" %in% class(st_geometry(ou))))
    {
      # pas de points mais une résolution, on crée la grille
      ou_3035 <- ou %>% st_transform(3035)
      rr_3035 <- fasterize(ou_3035, raster_ref(ou_3035, res_ou), fun = "any")
      xy_3035 <- raster::xyFromCell(rr_3035, which(raster::values(rr_3035) == 1))
      xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
      ou_4326 <- data.table(lon = xy_4326[, 1], lat = xy_4326[, 2],
                            x = xy_3035[,1], y=xy_3035[,2])
    }
    else 
    {
      # des points, une résolution ou pas, on garde les points
      if (!("sfc_POINT" %in% class(st_geometry(ou)))) ou <- st_centroid(ou)
      xy_3035 <- ou %>% st_transform(3035) %>% st_coordinates()
      xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
      ou_4326 <- data.table(lon = xy_4326[, 1], lat = xy_4326[, 2],
                            x = xy_3035[, 1], y = xy_3035[, 2])
    }
  }
  
  ou_4326[, id := .I]
  
  setkey(ou_4326, id)
  list(ou_4326=ou_4326, quoi_4326=quoi_4326)
}

iso_ou_4326 <- function(ou, res_ou)
{
  ou_3035 <- ou %>% st_transform(3035)
  
  if (is.finite(res_ou)&&!("sfc_POINT" %in% class(st_geometry(ou))))
  {
    # pas de points mais une résolution, on crée la grille
    ou_3035 <- ou %>% st_transform(3035)
    rr_3035 <- fasterize(ou_3035, raster_ref(ou_3035, res_ou), fun = "any")
    xy_3035 <- raster::xyFromCell(rr_3035, which(raster::values(rr_3035) == 1))
    xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
    ou_4326 <- data.table(lon = xy_4326[, 1], lat = xy_4326[, 2],
                          x = xy_3035[,1], y=xy_3035[,2])
  }
  else 
  {
    # des points ou pas, une résolution ou pas, on garde les points
    if (!("sfc_POINT" %in% class(st_geometry(ou)))) ou <- st_centroid(ou)
    xy_3035 <- ou %>% st_transform(3035) %>% st_coordinates()
    xy_4326 <- sf_project(xy, from = st_crs(3035), to = st_crs(4326))
    ou_4326 <- data.table(lon = xy_4326[, 1], lat = xy_4326[, 2],
                          x = xy_3035[, 1], y = xy_3035[, 2])
  }
  ou_4326[, id := as.character(.I)]
  setkey(ou_4326, id)
  ou_4326
} 

iso_split_ou <- function(ou, quoi, chunk, mode="CAR", tmax=60) 
{
  n <- 100
  mou <- ou[sample(.N, n), .(x,y)] %>% as.matrix
  mquoi <- quoi[, .(x,y)] %>% as.matrix
  nquoi <- median(rowSums2(rdist::cdist(mou, mquoi) <= vmaxmode(mode)*tmax))

  size <- as.numeric(nrow(quoi))*as.numeric(nquoi)
  bbox <- matrix(c(xmax=max(ou$lon), xmin=min(ou$lon), ymax=max(ou$lat), ymin= min(ou$lat)), nrow=2)
  bbox <- sf_project(pts=bbox, from=st_crs(4326), to=st_crs(3035))
  surf <- (bbox[1,1]-bbox[2,1])*(bbox[1,2]-bbox[2,2])
  
  ngr <- min(max(8, round(size/chunk)), round(size/10)) # au moins 8 groupes, au plus des morceaux de 10
  log_info("{ngr} groupes desires")
  
  resolution <- 12.5*2^floor(max(0,log2(sqrt(surf/ngr)/12.5)))
  
  log_info("résolution des groupes {resolution}")
  idINS <- idINS3035(ou$x, ou$y, resolution)
  uidINS <- unique(idINS)
  
  out_ou <- ou
  out_ou[, `:=`(gr=idINS)]
  ou_gr <- set_names(as.character(unique(out_ou$gr)))
  
  list(ou=out_ou, ou_gr=ou_gr, resINS=resolution)
}

t_prudence <- function(routing, cell=1600)
{
  mode <- routing$mode
  vitesse <- case_when(
    "TRANSIT"%in%mode ~ 120,
    "CAR"%in%mode ~ 120,
    "BIKE"%in%mode ~ 120,
    "WALK"%in%mode ~ 60,
    TRUE ~ 60) # vitesse en metre par minute
  
  round(sqrt(2)*cell/vitesse)
}

vmaxmode <- function(mode)
{
  vitesse <- case_when(
    "TRANSIT"%in%mode ~ 80/60*1000,
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

access_on_groupe <- function(groupe, ou_4326, quoi_4326, routing, k, tmax, opp_var, ttm_out)
{
  s_ou <- ou_4326[gr==groupe, .(id, lon, lat, x, y)]
  
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
      
      # on filtre les cibles qui ne sont pas trop loin selon la distance euclidienne
      
      quoi_f <- minimax_euclid(from=ttm_ou$les_ou, to=quoi_4326, dist=vmaxmode(routing$mode)*(tmax+delay))
      
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
        log_warn("paquet:{groupe} ou_id:{les_ou_s} erreur ttm_0 {ttm_0$error}")
        ttm_0 <- data.table()
      }
      else 
        ttm_0 <- ttm_0$result
      
      if(nrow(ttm_0)>0)
      {
        pproches <- sort(unique(s_ou$closest))
        ttm_0[ , npea:=nrow(quoi_4326)] [, npep:=length(unique(toId)), by=fromId]
        if(!is.null(pproches))
          {
          # boucle sur les ancres
          ttm <- rbindlist( 
            map(pproches, 
                function(close)
                  ttm_on_closest(close, s_ou, quoi_4326, ttm_0, ttm_ou$les_ou, tmax, routing, groupe)
                )
            )
          ttm <- rbind(ttm_0[travel_time<=tmax], ttm)
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
          speed_log <-str_c(length(pproches), "paquets ", f2si2(npea),"@",f2si2(npea/dtime),"p/s demandees, ",f2si2(npep),"@",f2si2(npep/dtime), "p/s retenues")
        
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
            ttm_d <- ttm
            ttm_d <- ttm_d[, `:=`(npea=npea[[1]], npep=npep[[1]]), by=fromId]
          }
          log_info("paquet:{groupe} {speed_log}")
        }
        else 
        {
          ttm_d <- NULL
          log_info("paquet:{groupe} ttm vide") 
        }
      } # close nrow(ttm_0)>0
      else # nrow(ttm_0)==0
      {
        time <- toc(quiet=TRUE)
        log_warn("paquet:{groupe} ou_id:{les_ou_s} ttm_0 vide")
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
    ttm_d <- ttm_d1[, map(.SD,~sum(., na.rm=TRUE)),
                   by=c("fromId", "travel_time"),
                   .SDcols=(opp_var)]
    ttm_d2 <- ttm_d1[, .(npea=.N, npep=.N), by=fromId] [, gr:=groupe]
    ttm_d <- merge(ttm_d, ttm_d2, by="fromId")
  }
  ttm_d
}

minimax_euclid <- function(from, to, dist)
{
  m_from <- from[, .(x,y)] %>% as.matrix
  m_to <- to[, .(x,y)] %>% as.matrix
  dfrom2to <- rdist::cdist(m_from, m_to)
  dmin2to <- colMins(dfrom2to)
  dmin2to<=dist
}



# routing engines ---------------------


iso_ttm <- function(o, d, tmax, routing)
{
  switch(routing$type,
         "r5" = r5_ttm(o, d, tmax, routing),
         "otpv1" = otpv1_ttm(o, d, tmax, routing),
         "osrm" = osrm_ttm(o, d, tmax, routing),
         "dt"= dt_ttm(o, d, tmax, routing))
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
  safe_ttm <- safely(r5r::travel_time_matrix)
  
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
    res$result[, `:=`(fromId=as.integer(fromId), toId=as.integer(toId))]
  else
    log_warn("erreur r5::travel_time_matrix, retourne une matrice vide (après 2 essais)")
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
  temps <- future_map_dbl(1:nrow(paires), ~{
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
    dt[, `:=`(toId = as.integer(toId), travel_time = ceiling(travel_time))]
    table$result <- dt
  }
  table
}

dt_ttm <- function(o, d, tmax, routing)
{
  o_rid <- merge(o[, .(oid=id, lon, lat)], routing$fromId[, .(rid=id, lon, lat)], by=c("lon", "lat"))
  d_rid <- merge(d[, .(did=id, lon, lat)], routing$toId[, .(rid=id, lon, lat)], by=c("lon", "lat"))
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
  path <- glue(path, .envir = env)
  mode_string <- str_c(mode, collapse = "&")
  r5r::stop_r5()
  core <- r5r::setup_r5(data_path = path, verbose=FALSE)
  core$setNumberOfMonteCarloDraws(as.integer(montecarlo))
  list(
    type = "r5",
    string="r5 routing {mode_string} sur {path} à {now()}" %>% glue,
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
