# allocate RAM memory to Java
options(java.parameters = "-Xmx16G")

# 1) build transport network, pointing to the path where OSM and GTFS data are stored

r5r_GPE <- setup_r5(data_path = "{DVFdata}/r5r_data/IDFMGPE" %>% glue, verbose = TRUE)
r5r_noGPE <- setup_r5(data_path = "{DVFdata}/r5r_data/IDFM" %>% glue, verbose = TRUE)

# 2) load origin/destination points and set arguments
c200 <- load_DVF("c200idf")
points <- c200 %>%
  filter(Ind>0) %>%
  st_transform(4326) %>%
  st_centroid %>%
  st_coordinates %>%
  as_tibble() %>%
  rowid_to_column("id") %>% 
  rename(lon=X, lat=Y)

points_max <- c200 %>%
  filter(min_rank(-Ind)<11) %>%
  st_transform(4326) %>%
  st_centroid %>%
  st_coordinates %>%
  as_tibble() %>%
  rowid_to_column("id") %>% 
  rename(lon=X, lat=Y)

dv3f <- load_DVF("dv3fv4")
points_50 <- idINS2point(dv3f$IdINS_50 %>% unique) %>%
  sf_project(from=st_crs(3035), to=st_crs(4326)) %>% 
  as_tibble %>% 
  rowid_to_column("id") %>% 
  rename(lon=V1, lat=V2)

mode <- c("WALK","TRANSIT")
max_walk_dist <- 3000   # meters
max_trip_duration <- 100 # minutes
departure_datetime <- as.POSIXct("17-12-2019 8:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# 3.1) calculate a travel time matrix
tic();ttm <- travel_time_matrix(r5r_noGPE,
                                origins = points_max,
                                destinations = points,
                                mode,
                                departure_datetime,
                                max_walk_dist,
                                max_trip_duration); toc()

tic();ttm_GPE <- travel_time_matrix(r5r_GPE,
                                    origins = points_max,
                                    destinations = points,
                                    mode,
                                    departure_datetime,
                                    max_walk_dist,
                                    max_trip_duration); toc()


ttm_GPE <- ttm_GPE %>%
  as_tibble %>% 
  mutate(across(everything(), as.numeric)) %>%
  left_join(points, by =c("toId" = "id")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3035) %>%
  filter(fromId ==1)

ttm <- ttm %>%
  as_tibble %>% 
  mutate(across(everything(), as.numeric)) %>%
  left_join(points, by =c("toId" = "id")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3035) %>%
  filter(fromId ==1)

ttmr <- rasterize(ttm, raster_ref(ttm, resolution = 200), field="travel_time", background=NA)
ttm_GPEr <- rasterize(ttm_GPE, raster_ref(ttm, resolution = 200), field="travel_time", background=NA)

tmap_mode("plot")
a <- uu851.fdc+tm_shape(ttmr)+tm_raster(style="cont",palette=rev(heatvb))+uu851.hdc
b <- uu851.fdc+tm_shape(ttmr-ttm_GPEr)+tm_raster(style="cont",palette=green2gray)+uu851.hdc
tmap_arrange(a,b)