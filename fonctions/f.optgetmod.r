# fonction copiée d'otpr, qui permet de se faire une requête sur un serveur otp
# elle ne requiert aucune dépendance, donc elle marche bien avec future
# pour la doc, voir soir OTP soit otpr
make_url <- function(x)
{
  url <- paste0(
    ifelse(isTRUE(x$ssl), 'https://', 'http://'),
    x$hostname,
    ':',
    x$port,
    '/otp/routers/',
    x$router
  )
  return(url)
}

otp_get_iso_mod <- function (otpcon, location, fromLocation = TRUE, format = "JSON", 
                             mode = "TRANSIT", date, time, cutoffs, batch = TRUE, arriveBy = FALSE,
                             maxWalkDistance = 800, walkReluctance = 2, transferPenalty = 0,
                             minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=100, precisionMeters=100, bannedRoutes="")
{
  
  
  
  format <- toupper(format)
  mode <- toupper(mode)
  if (identical(mode, "TRANSIT") | identical(mode, "BUS")| identical(mode, "CAR") |
      identical(mode, "RAIL")) {
  mode <- append(mode, "WALK")
  }
  mode <- paste(mode, collapse = ",")
  routerUrl <- paste0(make_url(otpcon), "/isochrone")
  cutoffs <- as.list(cutoffs)
  names(cutoffs) <- rep("cutoffSec", length(cutoffs))
  query <- append(list(fromPlace = paste(location,
                                           collapse = ","), mode = mode, batch = batch, date = date,
                         time = time, maxWalkDistance = maxWalkDistance,
                         offRoadDistanceMeters=offRoadDistanceMeters, # ajouter pour ne pas être bloqué dans un champ
                         clampInitialWait=clampInitialWait, # est ce que l'on compte le temps d'attente
                         precisionMeters=precisionMeters, # taile de la grille utilisée pour les isochrones
                         walkReluctance = walkReluctance, arriveBy = arriveBy,
                         transferPenalty = transferPenalty, minTransferTime = minTransferTime, bannedRoutes=bannedRoutes),
                    cutoffs)
  req <- httr::GET(routerUrl, query = query )
  req <- httr::content(req, as = "text", encoding = "UTF-8")
  if (grepl("\"type\":\"FeatureCollection\"", req)) {
    errorId <- "OK"
    if (format == "SF") {
      req <- geojsonsf::geojson_sf(req)
    }
  }
  else {
    errorId <- "ERROR"
  }
  response <- list(errorId = errorId, response = req)
  return(response)
}


# lance un serveur java. Il faut qu'il existe un répertoire correctement structuré dans {DVFdata}/otp_idf
# le paramètre router correspond à un sous dossier dasn lequel le graphe OTP doit être construit
# la fonction attend que le serveur soit prêt, elle peut donc prendre quelques secondes à exécuter

OTP_server <- function(router="IDF1", port=8008, memory="8G", rep=DVFdata)
{
  safe_otp_connect <- purrr::safely(otpr::otp_connect)
  connected <- FALSE
  connection <- safe_otp_connect(router=router, port=port)
  if (!is.null(connection$error))
  {
    secureport <- port+1
    current.wd <- getwd()
    setwd("{rep}/otp_idf" %>% glue)
    shell("start java -Xmx{memory} -jar otp-1.4.0-shaded.jar --router {router} --graphs graphs --server --port {port} --securePort {secureport}" %>% glue(),
          translate = TRUE, wait = FALSE, mustWork = TRUE)
    setwd(current.wd)
    safe_otp_connect <- safely(otpr::otp_connect)
    connected <- FALSE
    while(!connected) {
      connection <- safe_otp_connect(router=router, port=port)
      connected <- is.null(connection$error)}
  }
  connection$result
}

otp_get_dist <- function (otpcon, fromPlace, toPlace, mode = "CAR") 
{
  mode <- toupper(mode)
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_class(otpcon, "otpconnect", add = coll)
  checkmate::assert_numeric(fromPlace, lower = -180, upper = 180, 
                            len = 2, add = coll)
  checkmate::assert_numeric(toPlace, lower = -180, upper = 180, 
                            len = 2, add = coll)
  checkmate::assert_choice(mode, choices = c("WALK", 
                                             "BICYCLE", "CAR"), null.ok = F, add = coll)
  checkmate::reportAssertions(coll)
  fromPlace <- paste(fromPlace, collapse = ",")
  toPlace <- paste(toPlace, collapse = ",")
  mode <- paste(mode, collapse = ",")
  routerUrl <- paste0(make_url(otpcon)$router, "/plan")
  req <- httr::GET(routerUrl, query = list(fromPlace = fromPlace, 
                                           toPlace = toPlace, mode = mode))
  url <- urltools::url_decode(req$url)
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  asjson <- jsonlite::fromJSON(text)
  if (!is.null(asjson$error$id)) {
    response <- list(errorId = asjson$error$id, errorMessage = asjson$error$msg, 
                     query = url)
    return(response)
  }
  else {
    error.id <- "OK"
  }
  if (length(asjson$plan$itineraries) == 0) {
    response <- list(errorId = -9999, errorMessage = "No itinerary returned.", 
                     query = url)
    return(response)
  }
  if (mode == "CAR") {
    response <- list(errorId = error.id, distance = asjson$plan$itineraries$legs[[1]]$distance, 
                     query = url)
    return(response)
  }
  else {
    response <- list(errorId = error.id, distance = asjson$plan$itineraries$walkDistance, 
                     query = url)
    return(response)
  }
}

