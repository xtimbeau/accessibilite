# devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

travel_time_matrix_c <- function(r5r_core, origins, destinations, mode = "WALK",
                                 mode_egress = "WALK", departure_datetime = Sys.time(),
                                 time_window = 1L, percentiles = 50L, max_walk_dist = Inf,
                                 max_trip_duration = 120L, walk_speed = 3.6, bike_speed = 12,
                                 max_rides = 3, n_threads = Inf, verbose = TRUE, draws = 5 * time_window) {
  old_options <- options()
  old_dt_threads <- data.table::getDTthreads()
  on.exit({
    options(old_options)
    data.table::setDTthreads(old_dt_threads)
  })
  options(datatable.optimize = Inf)
  checkmate::assert_class(r5r_core, "jobjRef")
  mode_list <- r5r:::select_mode(mode, mode_egress)
  departure <- r5r:::posix_to_string(departure_datetime)
  checkmate::assert_numeric(max_trip_duration)
  max_trip_duration <- as.integer(max_trip_duration)
  max_street_time <- r5r:::set_max_street_time(
    max_walk_dist, walk_speed,
    max_trip_duration
  )
  origins <- r5r:::assert_points_input(origins, "origins")
  destinations <- r5r:::assert_points_input(destinations, "destinations")
  checkmate::assert_numeric(time_window)
  time_window <- as.integer(time_window)
  draws <- as.integer(draws)
  percentiles <- percentiles[1:5]
  percentiles <- percentiles[!is.na(percentiles)]
  checkmate::assert_numeric(percentiles)
  percentiles <- as.integer(percentiles)
  r5r_core$setTimeWindowSize(time_window)
  r5r_core$setPercentiles(percentiles)
  r5r_core$setNumberOfMonteCarloDraws(draws)
  r5r:::set_speed(r5r_core, walk_speed, "walk")
  r5r:::set_speed(r5r_core, bike_speed, "bike")
  r5r:::set_max_rides(r5r_core, max_rides)
  r5r:::set_n_threads(r5r_core, n_threads)
  r5r:::set_verbose(r5r_core, verbose)
  travel_times <- r5r_core$travelTimeMatrixParallel(
    origins$id,
    origins$lat, origins$lon, destinations$id, destinations$lat,
    destinations$lon, mode_list$direct_modes, mode_list$transit_mode,
    mode_list$access_mode, mode_list$egress_mode, departure$date,
    departure$time, max_street_time, max_trip_duration
  )
  travel_times <- jdx::convertToR(travel_times)
  travel_times <- data.table::rbindlist(travel_times)
  for (j1 in seq_along(travel_times)) {
    cl1 <- class(travel_times[[j1]])
    if (cl1 == "list") {
      data.table::set(travel_times, i = NULL, j = j1, value = unlist(travel_times[[j1]]))
    }
  }
  for (j in seq(from = 3, to = length(travel_times))) {
    data.table::set(travel_times, i = which(travel_times[[j]] >
      max_trip_duration), j = j, value = NA_integer_)
  }
  return(travel_times)
}
