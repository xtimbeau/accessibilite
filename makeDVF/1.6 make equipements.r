source("DVF.r")

# on traite quelques ?quipements avec une isochrone ? pied
# dans un premier temps on regarde surtout les cr?ches (D502)

c200idf <- load_DVF("c200idf") %>% st_transform(3035)
iris <- load_DVF("iris") %>% st_transform(3035)

bbPC <- st_bbox(st_union(iris %>% filter(DEP %in% c("75"))))
uu <- filter(iris, UU2010==851) %>% pull(INSEE_COM)

didf.sf <- iris %>% filter(DEP%in%depIdf) %>% group_by(DEP) %>% summarize(P15_POP=sum(P15_POP), EMP09=sum(EMP09, na.rm=TRUE), UU2010=list(UU2010))
uu851 <- st_union(filter(iris, UU2010=="851")) %>% st_sf
uu851.e <- xt_as_extent(uu851)

# équipements --------------------------------------------------

otpc <- OTP_server(router="IDF1", port=8090, memory="8G")

param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=c(300, 600, 900),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=200,
              mode="WALK")

options(future.globals.maxSize= 16*1024^3)
plan("multiprocess", workers=4)

equipements <- foreign::read.dbf(file="{DVFdata}/Equipements/bpe_ensemble_xy.dbf" %>% glue)
equipements <- filter(equipements, dep%in%depIdf)

codes_equipements <- readxl::read_excel("{DVFdata}/equipements/codes equiments.xlsx" %>% glue)
equipements <- left_join(equipements, codes_equipements, by=c("typequ"="code_equipement"))

eq.sf <- st_as_sf(equipements %>% filter(qualite_xy%in%c("Bonne", "Acceptable", "Mauvaise")),
                  coords=c("lambert_x", "lambert_y"), crs=2154)
eq.sf <- st_transform(eq.sf, 3035)

creches <- eq.sf %>% filter(typequ=="D502") # 3 178 créches
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
                                          res_fac=4,
                                          n_split=2*nbrOfWorkers())

creches_pos_n <- creches_pos %>%
  filter(Ind_300>500) %>%
  transmute(X,Y, cste=1L,
            pop_n_10mn=round(min(Ind_600)/Ind_600*2^16))

plan("multiprocess", workers=4)
creches_isoac_10 <- future_aggr_isochrone_DT(creches_pos_n,
                                             param,
                                             raster_ref(c200idf, resolution=10),
                                             progress=TRUE, timing=TRUE, todisk=TRUE)

save_DVF(creches_isoac_10)

rm(creches_pos_n, creches_isoac_10)

plan("multiprocess", workers=4)
rest_isoac_10 <- future_aggr_isochrone_DT(restaurants_pos ,
                                          param,
                                          raster_ref(c200idf, resolution=10),
                                          progress=TRUE, n_split = nbrOfWorkers(), todisk=TRUE)
save_DVF(rest_isoac_10)

tmap_options(max.raster = c(plot = 1e+9, view = 1e+9))
tmap_mode("view")

tm_shape(rest_isoac_10$bricks$cste[["iso10mn"]])+tm_raster(style="cont", palette=green2gray)

