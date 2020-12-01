source("DVF.r")

#grille carreau 200m projection 3035
#g200idf est la grille simple, c200idf la grille plus les infos d?mographiques
#dans g200idf il y a une colonne iris, qui permet de sélectionner les 65000 carreaux de l'aire urbaine

c200idf <- load_DVF("c200idf")
c200idf %<>% st_transform(3035)
iris15 <- load_DVF("iris15")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union
iris_idf <- iris15 %>% filter(st_within(.,idf, sparse=FALSE))

# définition des points d'arrivée à partir des centres des iris
iris_pos <- iris_idf %>% 
  st_transform(4326) %>%
  as_tibble %>%
  transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
            Y=st_coordinates(st_centroid(geometry))[, "Y"],
            EMP09,
            P15_POP,
            cste=1)

otpcs <- future_map(3:4, ~OTP_server(router=str_c("IDF",.), port=8100+10*., memory = "8G", rep=DVFdata))
otpc <- otpcs[[1]]

# Calcul de la carte transport en commun ------------------------------------------

plan(multiprocess, workers=4)
param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs = seq(600, 6000, by=300),
              batch = FALSE, arriveBy = FALSE, walkReluctance= 2, maxWalkDistance= 500,
              transferPenalty = 0, minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=50,
              precisionMeters=200, mode="TRANSIT")

iso_transit_50 <- future_aggr_isochrone_DT(iris_pos,
                                           param,
                                           raster_ref(c200idf, resolution=50),
                                           progress=TRUE,
                                           todisk=TRUE,
                                           otpserver=otpcs)

save_DVF(iso_transit_50)

# Calcul de la carte voiture ------------------------------------------------------

plan("multiprocess", workers=2)
param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs = seq(600, 1200, by=300),
              batch = FALSE, arriveBy = FALSE, walkReluctance= 2, maxWalkDistance= 500,
              transferPenalty = 0, minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=250,
              precisionMeters=50, mode="CAR")

iso_car_50 <- future_aggr_isochrone_DT(iris_pos,
                                       param,
                                       raster_ref(c200idf, resolution=50),
                                       progress=TRUE,
                                       todisk=TRUE,
                                       otpserver=otpcs)

  save_DVF(iso_car_50)

# Calcul de GPE transport en commun ------------------------------------------------------

otpcs <- map(1:1, ~OTP_server(router="IDFG1", port=8080+10*., memory = "6G", rep=localdata))
otpc <- otpcs[[1]]
plan("multiprocess", workers=4)
param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs = seq(600, 5400, by=300),
              batch = FALSE, arriveBy = FALSE, walkReluctance= 2, maxWalkDistance= 500,
              transferPenalty = 0, minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=250,
              precisionMeters=50, mode="TRANSIT")

gpe_tr50 <- future_aggr_isochrone_DT(iris_pos %>% select(X, Y, EMP09),
                                     param,
                                     raster_ref(c200idf, resolution=50),
                                     progress=TRUE,
                                     todisk=TRUE,
                                     otpserver = otpcs)

save_DVF(gpe_tr50)
