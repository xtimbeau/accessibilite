
source("init.r")
library("tidytransit")
library("lubridate")

gtfs <- "lyon/r5" 

gtfss <- list.files("{DVFdata}/r5r_data/{gtfs}" %>% glue, pattern = "*gtfs*|*GTFS*")

mars <-map(gtfss, ~read_gtfs("{DVFdata}/r5r_data/{gtfs}/{.x}" %>% glue))
do.call(max, map(map(mars, ~c(min(.x$calendar$start_date), max(.x$calendar$end_date))), 1))
do.call(min, map(map(mars, ~c(min(.x$calendar$start_date), max(.x$calendar$end_date))), 2))

