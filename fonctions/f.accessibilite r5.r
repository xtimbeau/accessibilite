# utilise R5 comme calcul de distance et détermine les indicateurs d'accessibilité à partir des ces distance
# opportunite sont les positions que l'on souhaite atteindre (format sf)avec des variables numériques pour lesquelles le calcul sera fait
# lieux est le sf des endroits pour lesquels on calcule l'accessibilité
# res_lieux est la résolution appliquée aux lieux. Ils sont projetté sur une grilel de résolution donnée (3035) si n'est pas Inf
# sinon, lieux est gardé tel quel (une résolution élevée réduit el temps de calcul)
# la sortie est un sf avec les mêmes variables numériques et dans le système de coordonnées des lieux

r5_accessibilite <- function(quoi,                      # sf avec des variables numériques qui vont être aggrégées
                             ou=NULL,                   # positions sur lesquelles sont calculés les accessibilités (si NULL, sur une grille)
                             res_quoi=Inf,                 # projection éventuelle des lieux sur une grille
                             resolution=ifelse(is.null(ou), 200, Inf),
                             fun_quoi=sum,                 # si projection fonction d'agrégation
                             r5_core,                      # pointeur sur le core r5 utilisé ou chemin vers le dosssier
                             mode=c("WALK", "TRANSIT"),    # TRANSIT, CAR, WALK etc...
                             date="17-12-2019 8:00:00",    # au format = "%d-%m-%Y %H:%M:%S"
                             max_walk_dist=2000,           # en mètres
                             temps_max=10L,                # en minutes
                             pas_de_temps=1L,
                             time_window=1L,
                             chunk=10000000,               # paquet envoyé à r5
                             java_mem=16L,                 # en giga
                             montecarlo=1L,                # nombre de tirage effectués par r5
                             nthreads=parallel::detectCores(logical=FALSE),
                             out=ifelse(is.finite(resolution),
                                             resolution,
                                             "data.table")) # ou data.table ou sf ou une résolution et ce sera un raster
{
  tic()
  options(java.parameters = "-Xmx{java_mem}G" %>% glue)
  opp_var <- names(quoi %>% as_tibble %>% select(where(is.numeric)))
  names(opp_var) <- opp_var
  if(is.character(r5_core)) {
    Message("Lecture du schéma géographique")
    core <- setup_r5(data_path =r5_core)
  }
  else core <- r5_core
  assertthat::assert_that("jobjRef"%in% class(core))
  
  r5_core$setNumberOfMonteCarloDraws(as.integer(montecarlo))
  
  ouetquoi <- isor5_ouetquoi_4326(ou=ou, quoi=quoi, res_ou=resolution, res_quoi=res_quoi, opp_var=opp_var)
  ou_4326 <- ouetquoi$ou_4326
  quoi_4326 <- ouetquoi$quoi_4326
  
  vitesse <- isor5_vmax(mode)

  groupes <- isor5_split(ou_4326, quoi_4326, resolution=resolution, chunk=chunk, buffer=temps_max*vitesse)
  ou_4326 <- groupes$ou
  quoi_gr <- groupes$quoi_grps
  grps <- names(quoi_gr)
  
  message("...isochrones")
  
  with_progress({
    pb <- progressor(steps=length(grps))
    access <- rbindlist(
      map(grps, ~{
        pb()
        s_ou <- ou_4326[groupe==.x, .(id, lon, lat) ]
        s_quoi <- quoi_4326[quoi_gr[[.x]], .SD, .SDcols=c("id", "lon", "lat", opp_var)]
        if(nrow(s_quoi)>0)  
          {
          ttm <- travel_time_matrix(core,
                                  origins = s_ou,
                                  destinations = s_quoi,
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
          ttm_d <- merge(ttm, s_quoi[, `:=`(lon=NULL, lat=NULL)], by.x="toId", by.y="id", all.x=TRUE)
          ttm_d <- ttm_d[, map(.SD,~sum(., na.rm=TRUE)),
                         by=c("fromId", "travel_time"),
                         .SDcols=(opp_var)] 
          ttm_d
          }
          else 
          {
            NULL}
          }
        else NULL}))},
  handlers=handler_progress(format=":bar :percent :eta", width=80))
  
  message("...cumul")
  setorder(access, fromId, travel_time)
  setnames(access, new="temps", old="travel_time")
  all_times <- CJ(fromId=unique(access$fromId),temps=c(1:temps_max), sorted=FALSE)
  access <- merge(all_times, access, by=c("fromId", "temps"), all.x=TRUE)
  for (v in opp_var) 
    set(access, i=which(is.na(access[[v]])), j=v, 0)
  access_c <- access[,
                     list.append(map(.SD, cumsum), temps=temps),
                     by=fromId,
                     .SDcols=opp_var]
  tt <- seq(pas_de_temps, temps_max, pas_de_temps)
  access_c <- access_c[temps%in%tt, ]
  access_c <- merge(access_c, ou_4326, by.x="fromId", by.y="id")
  if(is.numeric(out)) {
    outr <- out
    out <- "raster"
  }
  res <- switch(out,
         data.table = access_c,
         sf = access_c %>% as_tibble %>% st_as_sf(coords=c("x","y"), crs=3035),
         raster = {
           message("...rastérization")
           r_xy <- access_c[, .(x=x[[1]], y=y[[1]]), by=fromId] [, fromId:=NULL]
           if (!is.null(ou))
             r <- raster_ref(ou, outr)
           else
             r <- raster_ref(quoi, outr)
           map(opp_var, function(v) {
             brick(
               map(tt, ~{
                rz <- rasterize(r_xy, r, field=access_c[temps==.x, .SD, .SDcols=v])
                names(rz) <- str_c("iso",.x, "m")
                rz}))})})
  
  time <- toc(quiet = TRUE)
  dtime <- (time$toc - time$tic)
  message("{dtime%/%60}m {round(dtime-60*dtime%/%60)}s" %>% glue())
  res
}

r5_isochrones <- function(ou,                           # positions sur lesquelles sont calculés les isochrones 
                          resolution=200,
                          r5_core,                      # pointeur sur le core r5 utilisé ou chemin vers le dosssier
                          mode=c("WALK", "TRANSIT"),    # TRANSIT, CAR, WALK etc...
                          date="17-12-2019 8:00:00",    # au format = "%d-%m-%Y %H:%M:%S"
                          max_walk_dist=2000,           # en mètres
                          temps_max=10L,                # en minutes
                          pas_de_temps=1L,
                          time_window=1L,
                          java_mem=16L,                 # en giga
                          montecarlo=ifelse(time_window==1, 1L, 100L)                 # nombre de tirage effectués par r5
                          ) 
{
  tic()
  options(java.parameters = "-Xmx{java_mem}G" %>% glue)
  
  if(is.character(r5_core)) {
    Message("Lecture du schéma géographique")
    core <- setup_r5(data_path =r5_core)
  }
  else core <- r5_core
  assertthat::assert_that("jobjRef"%in% class(core))
  
  r5_core$setNumberOfMonteCarloDraws(as.integer(montecarlo))
  
  ou_4326 <- isor5_ou_4326(ou=ou, res_ou=resolution)
  ou_4326 <- ouetquoi$ou_4326
  
  vitesse <- isor5_vmax(mode)
  
  groupes <- isor5_split(ou_4326, quoi_4326, resolution=resolution, chunk=chunk, buffer=temps_max*vitesse)
  ou_4326 <- groupes$ou
  quoi_gr <- groupes$quoi_grps
  grps <- names(quoi_gr)
  
  message("...isochrones")
  
  with_progress({
    pb <- progressor(steps=length(grps))
    access <- rbindlist(
      map(grps, ~{
        pb()
        s_ou <- ou_4326[groupe==.x, ]
        s_quoi <- quoi_4326[quoi_gr[[.x]], ]
        if(nrow(s_quoi)>0) 
        {
          ttm <- travel_time_matrix(core,
                                    origins = s_ou,
                                    destinations = s_quoi,
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
            setkey(ttm, toId)
            ttm_d <- ttm[quoi_4326]
            ttm_d <- ttm_d[, map(.SD,~sum(., na.rm=TRUE)),
                           by=c("fromId", "travel_time"),
                           .SDcols=(opp_var)] [s_ou, on=c("fromId"="id")]
            ttm_d
          }
          else 
          {
            NULL}
        }
        else NULL}))},
    handlers=handler_progress(format=":bar :percent :eta", width=80))
 
  time <- toc(quiet = TRUE)
  dtime <- (time$toc - time$tic)
  message("{dtime%/%60}m {round(dtime-60*dtime%/%60)}s" %>% glue())
  list(pairs_dist=access, coords=ou_4326)
}

isor5_ouetquoi_4326 <- function(ou, quoi, res_ou, res_quoi, opp_var)
  {
  if (!("sfc_POINT" %in% class(st_geometry(quoi))))
    quoi <- st_centroid(quoi)
  
  # projection éventuelle sur une grille 3035 à la résolution res_quoi ou resolution
  if (is.finite(res_quoi))
  {
    st_3035 <- quoi %>% st_geometry() %>% st_transform(3035)
    quoi_4326 <-
      data.table(idINS = point_on_idINS(st_3035, resolution = res_quoi))
    quoi_4326[, (opp_var) := map(opp_var, ~ quoi[[.x]])]
    quoi_4326 <-
      quoi_4326[, map(.SD, ~ fun_quoi(., na.rm = TRUE)), by = idINS, .SDcols =
                  (opp_var)]
    xy_3035 <- idINS2point(quoi_4326[["idINS"]], resolution = res_quoi)
    xy_4326 <- sf_project(xy_3035, from = st_crs(3035), to = st_crs(4326))
  }
  else
  {
    xy_3035 <- st_coordinates(quoi %>% st_transform(3035))
    xy_4326 <- st_coordinates(quoi %>% st_transform(4326))
    quoi_4326 <-
      quoi %>% as_tibble %>% select(all_of(opp_var)) %>% as.data.table
    
  }
  quoi_4326[, `:=`(lon = xy_4326[, 1], lat = xy_4326[, 2],
                   x= xy_3035[,1], y=xy_3035[,2])]
  quoi_4326[, id := as.character(.I)]
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
        xy_3035 <- ou %>% st_coordinates()
        xy_4326 <- sf_project(xy, from = st_crs(3035), to = st_crs(4326))
        ou_4326 <- data.table(lon = xy_4326[, 1], lat = xy_4326[, 2],
                              x = xy_3035[, 1], y = xy_3035[, 2])
      }
    }
  
  ou_4326[, id := as.character(.I)]

  setkey(ou_4326, id)
  list(ou_4326=ou_4326, quoi_4326=quoi_4326)
}

isor5_ou_4326 <- function(ou, res_ou)
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
    xy_3035 <- ou %>% st_coordinates()
    xy_4326 <- sf_project(xy, from = st_crs(3035), to = st_crs(4326))
    ou_4326 <- data.table(lon = xy_4326[, 1], lat = xy_4326[, 2],
                          x = xy_3035[, 1], y = xy_3035[, 2])
  }
  ou_4326[, id := as.character(.I)]
  setkey(ou_4326, id)
  ou_4326
  }

isor5_split <- function(ou, quoi, resolution, chunk, buffer) 
  {
  size <- as.numeric(nrow(quoi))*as.numeric(nrow(ou))
  surf <- (max(ou$x)-min(ou$x))*(max(ou$y)-min(ou$y))
  if (log10(size)>10) warn("Attention plus de dix milliards de paires") 
  
  ngr <- min(max(1, round(size/chunk)), round(size/10)) # au moins 1 groupe, au plus des morceaux de 10
  
  if (resolution==Inf)
    resolution <- 12.5*2^round(log2((sqrt(surf/nrow(ou)))/12.5))

  resINS <- (2^round(log2(sqrt(nrow(ou)/ngr))))*resolution
  
  idINS <- idINS3035(ou$x, ou$y, resINS)
  uidINS <- unique(idINS)
  quoi_gr <- idINS2square(uidINS, resINS) %>% 
    st_buffer(buffer) %>%
    st_as_sf() %>% 
    rowid_to_column("groupe") %>% 
    mutate(idINS =uidINS)
  ugr <- set_names(1:nrow(quoi_gr), uidINS)
  sf_quoi <- quoi %>%
    as_tibble() %>%
    st_as_sf(coords=c("x","y"), crs=3035, remove=FALSE)
  out_quoi <- quoi
  grps <- t(st_within(sf_quoi, quoi_gr))
  names(grps) <- ugr
  out_ou <- ou
  out_ou[, groupe:= ugr[idINS]]
  n_ou <- out_ou[, .N, by=groupe]
  n_quoi <- map_int(grps, length)
  paires <- sum(n_ou$N*n_quoi)
  
  message("{f2si2(paires)} paires ({f2si2(size)} paires bruts), {f2si2(length(ugr))} paquets, surrésolution {resINS})" %>% glue)
  
  list(ou=out_ou, quoi_grps=grps, resINS=resINS)
}


isor5_vmax <- function(mode)
{
  case_when(
    "TRANSIT"%in%mode ~ 40000/60,
    "CAR"%in%mode ~ 60000/60,
    "BIKE"%in%mode ~ 300,
    "WALK"%in%mode ~ 80) # vitesse en metre par minute
}
  
r5_isochrone <- function(lon, lat,                         # en coordonnées lon, lat
                      resolution= 50,
                      r5_core,                      # pointeur sur le core r5 utilisé ou chemin vers le dosssier
                      mode=c("WALK", "TRANSIT"),    # TRANSIT, CAR, WALK etc...
                      date="17-12-2019 8:00:00",    # au format = "%d-%m-%Y %H:%M:%S"
                      max_walk_dist=2000,           # en mètres
                      temps_max=10L,                # en minutes
                      time_window=1L,
                      java_mem=16L,                  # en giga
                      montecarlo=1L,
                      plot=FALSE,
                      nthreads=parallel::detectCores(logical=FALSE))
{
  tic()
  options(java.parameters = "-Xmx{java_mem}G" %>% glue)
  
  if(is.character(r5_core)) {
    Message("Lecture du schéma géographique")
    core <- setup_r5(data_path =r5_core)
  }
  
  else core <- r5_core
  assertthat::assert_that("jobjRef"%in% class(core))
  
  r5_core$setNumberOfMonteCarloDraws(as.integer(montecarlo))

  vitesse <- isor5_vmax(mode)
  
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


# calcul de la matrice de temps à partir du moteur r5
# empaquette l'accès au moteur, que l'on suppose initialisé et mis en place

