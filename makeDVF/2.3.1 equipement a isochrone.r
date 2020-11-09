source("DVF.r")

# on traite quelques ?quipements avec une isochrone ? pied
# dans un premier temps on regarde surtout les cr?ches (D502)


load_DVF("c200idf")
c200idf <- c200idf %>% st_transform(3035)
load_DVF("iris")
iris <- iris %>% st_transform(3035)
bbPC <- st_bbox(st_union(iris %>% filter(DEP %in% c("75"))))
uu <- filter(iris, UU2010==851) %>% pull(INSEE_COM)

load_DVF("iris15")
didf.sf <- iris15 %>% filter(DEP%in%depIdf) %>% group_by(DEP) %>% summarize(P15_POP=sum(P15_POP), EMP09=sum(EMP09, na.rm=TRUE), UU2010=list(UU2010))
uu851 <- st_union(filter(iris15, UU2010=="00851")) %>% st_sf
uu851.e <- xt_as_extent(uu851)

# équipements --------------------------------------------------


otpc <- OTP_server(router="IDF1")

param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=c(300, 600, 900),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=200,
              mode="WALK")

options(future.globals.maxSize= 16*1024^3)
plan("multiprocess", workers=8)

load_DVF("equipements") #IDF est d?j? s?lectionn? !

codes_equipements <- read_excel("{DVFdata}/equipements/codes equiments.xlsx" %>% glue)
equipements <- left_join(equipements, codes_equipements, by=c("typequ"="code_equipement"))

eq.sf <- st_as_sf(equipements %>% filter(qualite_xy%in%c("Bonne", "Acceptable", "Mauvaise")),
                  coords=c("lambert_x", "lambert_y"), crs=2154)
eq.sf <- st_transform(eq.sf, 3035)

creches <- eq.sf %>% filter(typequ=="D502") # 3 178 cr?ches
restaurants <- eq.sf %>% filter(typequ=="A504") # 46 644 restaurants

creches_pos <- creches %>%
  st_transform(4326) %>%
  as_tibble %>%
  transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
            Y=st_coordinates(st_centroid(geometry))[, "Y"])

restaurants_pos <- restaurants %>%
  st_transform(4326) %>%
  as_tibble %>%
  transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
            Y=st_coordinates(st_centroid(geometry))[, "Y"],
            cste=1L)

creches_pos <- future_kernel_isochronique(sf=c200idf %>% select(Ind),
                                          positions=creches_pos,
                                          param=param,
                                          res_fac=10,
                                          n_split=2*nbrOfWorkers())
save_DVF(creches_pos)

load_DVF("creches_pos")

creches_pos_n <- creches_pos %>%
  filter(Ind_300>500) %>%
  transmute(X,Y, cste=1L,
            pop_n_10mn=round(min(Ind_600)/Ind_600*2^16))

plan("multiprocess", workers=2)
creches_isoac_10 <- future_aggr_isochrone_DT(creches_pos_n,
                                  param,
                                  raster_ref(c200idf, resolution=10),
                                  progress=TRUE, timing=TRUE, todisk=TRUE)

save_DVF(creches_isoac_10)

plan("multiprocess", workers=2)
rest_isoac_10 <- future_aggr_isochrone_DT(restaurants_pos ,
                                             param,
                                             raster_ref(c200idf, resolution=10),
                                             progress=TRUE, n_split = nbrOfWorkers(), todisk=TRUE)
save_DVF(rest_isoac_20)
load_DVF("rest_isoac_20")


tmap_options(max.raster = c(plot = 1e+9, view = 1e+9))
tmap_mode("view")

tm_shape(creches_isoac_20$bricks$cste[["iso10mn"]])+tm_raster(style="cont", palette=green2gray)

# on enregistre dans dvfplus

load_DVF("dvfplus")

dvfplus.co <- dvfplus %>% st_as_sf %>% st_transform(3035) %>% st_coordinates
dvfplus <- mutate(dvfplus, eq_CR_iso10pop = raster::extract(creches_isoac_10$bricks$pop_n_10mn[["iso10mn"]], dvfplus.co))
dvfplus <- mutate(dvfplus, eq_CR_iso10nbr = raster::extract(creches_isoac_10$bricks$cste[["iso10mn"]], dvfplus.co))
dvfplus <- mutate(dvfplus, eq_REST_iso10nbr = raster::extract(rest_isoac_10$bricks$cste[["iso10mn"]], dvfplus.co))

save_DVF(dvfplus)

# population -> POP et PAUV ------------------------------------

otpcs <- future_map(1:1, ~OTP_server(router=str_c("IDF",.), port=8000+10*., memory = "3G", rep=DVFdata))
otpc <- otpcs[[1]]

load_DVF("c200idf")
plan("multiprocess", workers=8)
param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=c(300, 600, 900),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=200,
              mode="WALK")

c200idf <- c200idf %>% st_transform(3035)

load_DVF("g200idf")
g200uu851 <- st_filter(g200idf %>% st_transform(3035), uu851)

g_pos <- c200idf %>% 
  filter(Ind>0) %>%
  st_transform(4326) %>% 
  as_tibble %>%
  transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
            Y=st_coordinates(st_centroid(geometry))[, "Y"])

pop <- future_kernel_isochronique(sf=c200idf %>% select(Ind, Men, Men_pauv),
                                          positions=g_pos,
                                          param=param,
                                          res_fac=4,
                                          progress = TRUE,
                                          n_split=2*nbrOfWorkers(),
                                          otpserver=otpcs)

pop.sf <- pop %>% st_as_sf(crs=4326, coords=c("X","Y")) %>% st_transform(3035)
pop.sf <- pop.sf %>% mutate(PAUV15_5 = ifelse(Men_300>0, Men_pauv_300/Men_300, 0),
                            PAUV15_10 = ifelse(Men_600>0, Men_pauv_600/Men_600, 0),
                            PAUV15_15 = ifelse(Men_900>0, Men_pauv_900/Men_900, 0),
                            POP15_5 = Ind_300/max(Ind_300, na.rm=TRUE),
                            POP15_10 = Ind_600/max(Ind_600, na.rm=TRUE),
                            POP15_15 = Ind_900/max(Ind_900), na.rm=TRUE)

popisokernel <- st_join(c200idf %>% select(IdINSPIRE), pop.sf, join=st_contains)
save_DVF(popisokernel)

# load_DVF("dvfplus")
# dvfplus.co <- dvfplus %>% st_as_sf %>% st_transform(3035) %>% st_coordinates
# walk( names(pop %>% select(-X, -Y)), ~{
#   varname <- str_c("pop_w_", .)
#     dvfplus <<- mutate(dvfplus,  !!varname := raster::extract(fasterize(c200idf.1, raster_ref(c200idf, resolution=200), field=.x), dvfplus.co))
# })
# save_DVF(dvfplus)

tmap_mode("plot")
tm_shape(fasterize(c200idf.1, raster_ref(c200idf, resolution=200), field="Ind_600"), bbox=bbPC)+tm_raster()


# GPE (gares XY) ------------------------------------------

otpcs <- future_map(1:1, ~OTP_server(router=str_c("IDF",.), port=8000+10*., memory = "3G", rep=DVFdata))
otpc <- otpcs[[1]]
plan("multiprocess", workers=2)

load_DVF("g200idf")
g200uu851 <- st_filter(g200idf %>% st_transform(3035), uu851)

param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=seq(from=300, to=900, by=300),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=150,
              mode="WALK")

load("{DVFdata}/GPE/Gares_GPE.Rda" %>% glue) # load stops
# fwrite(stops, file="{DVFdata}/GPE/Gares_GPE.csv" %>% glue ) (?dit? dans excel)
stops <- data.table::fread(file="{DVFdata}/GPE/Gares_GPE.csv" %>% glue )

stops_GPE <- stops %>%
  filter(GPE_test==1) %>% 
  transmute(X=stop_lon,
            Y=stop_lat,
            cste=1L,
            date_exploitation)

stops_GPE.xy <- stops %>% 
  filter(GPE_test==1) %>% 
  st_as_sf(crs=4326, coords=c("stop_lon", "stop_lat")) %>%
  st_transform(3035)  

GPE_21 <- stops_GPE %>% filter(date_exploitation==2021) %>% select(-date_exploitation)
GPE_25 <- stops_GPE %>% filter(date_exploitation<=2025) %>% select(-date_exploitation)
GPE_27 <- stops_GPE %>% filter(date_exploitation<=2027) %>% select(-date_exploitation)
GPE_30 <- stops_GPE %>% filter(date_exploitation<=2030) %>% select(-date_exploitation)

plan("sequential")
GPE_kiso <- map(list(GPE_21,GPE_25,GPE_27,GPE_30), ~aggr_isochrone_DT(positions=.x,
                                  grid=raster_ref(g200uu851, resolution=10),
                                  param=param,
                                  return = "raster",
                                  progress = TRUE))

names(GPE_kiso) <- c("GPE_21","GPE_25","GPE_27","GPE_30")

walk(GPE_kiso, function(iso) walk(iso$bricks, readAll) )

tmap_options(max.raster = c(plot = 1e+8, view = 1e+8))
tmap_mode("plot")
tm_shape(GPE_kiso$GPE_30$bricks$cste[["iso15mn"]])+tm_raster(style="cat")

pp <- colorspace::sequential_hcl(n = 5, h = c(15, 15), c = c(0, 180, 180), l = c(100, 25), power = c(1.2, 1.2), rev=FALSE)

tm_shape(didf.sf, bbox=uu851)+
  tm_fill("gray97")+
  tm_shape(mask(crop(GPE_kiso$GPE_30$bricks$cste[["iso15mn"]], uu851.e), uu851), bbox=uu851)+
  tm_raster(style="cat", title = "GPE", palette=pp)+
  tm_shape(didf.sf, bbox=uu851)+
  tm_borders(lwd=0.25)+
  tm_shape(uu851, bbox=uu851)+tm_borders("black",lwd=1)+
  tm_facets(free.scales.raster = FALSE)+
  tm_layout(legend.outside = TRUE)

load_DVF("dvfplus")
dvfplus.co <- dvfplus %>% st_as_sf %>% st_transform(3035) %>% st_coordinates

dvfplus <- dvfplus %>% select(-contains("GPE"))
walk(c("GPE_21","GPE_25","GPE_27","GPE_30"), function(gpe) {
  walk(names(GPE_kiso[[gpe]]$bricks$cste), ~{
  varname <- str_c(gpe, "_", .x)
  dvfplus <<- mutate(dvfplus,  !!varname := raster::extract(GPE_kiso[[gpe]]$bricks$cste[[.x]], dvfplus.co))})})

save_DVF(dvfplus)

load_DVF("dv3f")
dv3f.coord <- dv3f %>% st_centroid %>% st_coordinates

dv3f <- dv3f %>% select(-contains("GPE"))

walk(c("GPE_21","GPE_25","GPE_27","GPE_30"), function(gpe) {
  walk(names(GPE_kiso[[gpe]]$bricks$cste), ~{
    varname <- str_c(gpe, "_", .x)
    dv3f <<- mutate(dv3f,  !!varname := raster::extract(GPE_kiso[[gpe]]$bricks$cste[[.x]], dv3f.coord))})})

old <- keep(names(dv3f), ~str_detect(., "^GPE"))
new <- str_replace(old, "_iso", "wa") %>% str_replace("mn", "m") %>% str_replace("GPE_", "GPE")
names(new) <- old
dv3f <- dv3f %>% rename_at(vars(old), ~new)

save_DVF("dv3f")

# Construction d'un contrôle ----------------------------------------------

idfm <- read_gtfs(path="{DVFdata}/otp_idf/IDFM gtfs/IDFM_gtfs.zip" %>% glue)

idfm <- idfm %>% 
  set_hms_times() %>% 
  set_date_service_table()

metro_routes <- idfm$routes %>% filter(route_type %in% c(1,2))
metro_trips <- left_join(idfm$trips, idfm$routes, by="route_id")
metro_stops <- left_join(idfm$stop_times, metro_trips, by="trip_id")
stops_m <- metro_stops %>%
  filter(route_type%in%c(1,2)) %>%
  select(stop_id, route_id, route_short_name, route_long_name) %>% 
  distinct(stop_id, .keep_all= TRUE) %>%
  left_join(idfm$stops, by="stop_id")
rm(metro_stops, metro_routes, metro_trips, idfm)
stops_m.sf <- stops_m %>% st_as_sf(crs=4326, coords=c("stop_lon", "stop_lat")) %>% st_transform(3035)
tmap_mode("view")

stops_commun <- st_join(stops_GPE, stops_m.sf %>% select(stop_name, stop_id), join=function(x,y) st_is_within_distance(x,y, dist=300), suffix=c("",".y"))
stops_GPE_seul <- stops_commun %>% filter(is.na(stop_id.y))
tm_shape(stops_m.sf)+tm_dots(col="black", size=0.05, popup.var=c("stop_name"))+
  tm_shape(stops_GPE_seul)+tm_dots(col="blue", size=0.05)

stops_g <- stops_GPE_seul %>%
  st_transform(4326) %>%
  st_coordinates() %>% 
  as_tibble %>% 
  mutate(cste=1L)

stops_m <- stops_m %>%
  transmute(X=stop_lon,
            Y=stop_lat,
            cste=1L)

GPE_kiso <- aggr_isochrone_DT(positions=stops_g,
                                 grid=raster_ref(g200uu851, resolution=10),
                                 param=param,
                                 return="raster",
                                 progress = TRUE)

nonGPE_kiso <- aggr_isochrone_DT(positions=stops_m,
                              grid=raster_ref(g200uu851, resolution=10),
                              param=param,
                              return="raster",
                              progress = TRUE)

load_DVF("dvfplus")
dvfplus.co <- dvfplus %>% st_as_sf %>% st_transform(3035) %>% st_coordinates

walk( names(nonGPE_kiso$bricks$cste), ~{
  varname <- str_c("nonGPE_", .x)
  dvfplus <<- mutate(dvfplus,  !!varname := raster::extract(nonGPE_kiso$bricks$cste[[.x]], dvfplus.co))})

save_DVF(dvfplus)
