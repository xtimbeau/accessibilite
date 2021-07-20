# routing engines ---------------------


  iso_ttm <- function(o, d, tmax, routing)
{
  log_debug("iso_ttm:{tmax} {nrow(o)} {nrow(d)}")
  r <- switch(routing$type,
              "r5" = r5_ttm(o, d, tmax, routing),
              "otpv1" = otpv1_ttm(o, d, tmax, routing),
              "osrm" = osrm_ttm(o, d, tmax, routing),
              "dt"= dt_ttm(o, d, tmax, routing),
              "euclidean" = euc_ttm(o, d, tmax, routing))
  log_debug("result:{nrow(r$result)}")
  r
}

safe_ttm <- function(routing)
{
  switch(routing$type,
         "r5" = r5_ttm,
         "otpv1" = otpv1_ttm,         
         "osrm" = osrm_ttm,
         "dt"= dt_ttm,
         "euclidean" = euc_ttm)
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
  library("r5r", quietly=TRUE)
  o <- o[, .(id=as.character(id),lon,lat)]
  d <- d[, .(id=as.character(id),lon,lat)]
  safe_ttm <- purrr::safely(travel_time_matrix)
  
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
    {
      log_warn("erreur r5::travel_time_matrix, retourne une matrice vide après 2 essais")
      res$result <- data.table(fromId=numeric(), toId=numeric(), travel_time=numeric())
    }
  res
}

euc_ttm <- function(o, d, tmax, routing)
{
  library(rdist)
  mode <- routing$mode
  vitesse <- routing$speed
  
  o <- o[, .(id=as.character(id),lon,lat)]
  d <- d[, .(id=as.character(id),lon,lat)]
  
  o_3035 <- sf_project(from=st_crs(4326), to=st_crs(3035), o[, .(lon, lat)])
  d_3035 <- sf_project(from=st_crs(4326), to=st_crs(3035), d[, .(lon, lat)])
  dist <- rdist::cdist(X=o_3035, Y=d_3035, metric="euclidean", p=2)
  dist <- dist/(vitesse*1000/60)
  colnames(dist) <- d$id
  rownames(dist) <- o$id
  dt <- data.table(dist, keep.rownames=TRUE)
  setnames(dt, "rn", "fromId")
  dt[, fromId:=as.integer(fromId)]
  dt <- melt(dt, id.vars="fromId", variable.name="toId", value.name = "travel_time", variable.factor = FALSE)
  dt <- dt[travel_time<tmax,]
  dt[, `:=`(toId = as.integer(toId), travel_time = as.integer(ceiling(travel_time)))]
  list(error=NULL,
       result=dt)
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
  library("osrm", quietly=TRUE)
  options(osrm.server = routing$osrm.server, 
          osrm.profile = routing$osrm.profile)
  safe_table <- safely(osrm::osrmTable)
  l_o <- o[, .(id, lon, lat)]
  l_d <- d[, .(id, lon, lat)]
  tt <- safe_table(
    src = l_o,
    dst= l_d,
    exclude=NULL,
    gepaf=FALSE,
    measure="duration")
  if(!is.null(tt$error))
  {
    gc()
    log_warn("deuxieme essai osrm")
    tt <- safe_table(
      src = l_o,
      dst= l_d,
      exclude=NULL,
      gepaf=FALSE,
      measure="duration")
  }
  
  if(is.null(tt$error))
  {
    dt <- data.table(tt$result$duration, keep.rownames = TRUE)
    dt[, fromId:=rn %>% as.integer] [, rn:=NULL]
    dt <- melt(dt, id.vars="fromId", variable.name="toId", value.name = "travel_time", variable.factor = FALSE)
    dt <- dt[travel_time<tmax,]
    dt[, `:=`(toId = as.integer(toId), travel_time = as.integer(ceiling(travel_time)))]
    tt$result <- dt
  }
  else
    log_warn("erreur osrm::osrmTable, retourne une matrice vide après 2 essais")
  tt
}

dt_ttm <- function(o, d, tmax, routing)
{
  library("data.table", quietly=TRUE)
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

routing_setup_dt <- function(file,
                             rep="rda",
                             local="FALSE",
                             date=NULL,
                             mode=NULL,
                             future=NULL)
{
  message("...loading")
  fdt <- load_DVF(file=file, rep=rep, local=local) 
  message("...swapping")
  fdt <- swap2tmp_routing(fdt, qs=TRUE)
  os <- fdt$origin_string
  list(
    type = "dt",
    string=glue::glue("précalculé à partir de {os}"),
    mode=if(is.null(mode)) fdt$mode else mode,
    future=if(is.null(future)) fdt$future else future,
    time_table=fdt$time_table,
    ancres=fdt$ancres,
    resolution=fdt$resolution,
    res_ou=fdt$res_ou,
    res_quoi=fdt$res_quoi,
    toId = fdt$toId,
    fromId=fdt$fromId,
    groupes=fdt$groupes
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
  library("r5r", quietly=TRUE)
  env <- parent.frame()
  path <- glue::glue(path, .envir = env)
  mode_string <- stringr::str_c(mode, collapse = "&")
  r5r::stop_r5()
  rJava::.jgc(R.gc = TRUE)
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
  library("r5r", quietly=TRUE)
  library("AzureStor", quietly=TRUE)
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
  s_now <- lubridate::now()
  mode_string <- str_c(mode, collapse = "&")
  list(
    type = "otpv1",
    string=glue::glue("otpv1 routing {mode_string} sur {router}(:{port}) à {s_now}"),
    otpcon = OTP_server(router=router, port=port, memory = memory, rep=rep),
    date = unlist(stringr::str_split(date, " "))[[1]],
    time= unlist(stringr::str_split(date, " "))[[2]],
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
  s_now <- lubridate::now()
  list(
    type = "osrm",
    string=glue::glue("osrm routing localhost:{server} profile {profile} à {s_now}"),
    osrm.server =glue::glue("http://localhost:{server}/"),
    osrm.profile=profile,
    future=TRUE,
    mode=switch(profile,
                driving="CAR",
                walk="WALK",
                bike="BIKE"))
}

routing_setup_euc <- function(
  mode="WALK", speed=5)
  
{
  s_now <- lubridate::now()
  list(
    type = "euclidean",
    string=glue::glue("euclidien à {s_now}"),
    future=TRUE,
    mode=mode,
    speed=speed)
}
