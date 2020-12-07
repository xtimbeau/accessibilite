source("DVF.r")

#grille carreau 200m projection 3035
#g200idf est la grille simple, c200idf la grille plus les infos d?mographiques
#dans g200idf il y a une colonne iris, qui permet de s?lectionner les 65000 carreaux de l'aire urbaine

c200idf <- load_DVF("c200idf") %>% st_transform(3035)
iris <- load_DVF("iris")

uu <- filter(iris, UU2010==851) %>% pull(INSEE_COM)
didf.sf <- iris %>% filter(DEP%in%depIdf) %>% group_by(DEP) %>% summarize(P15_POP=sum(P15_POP), EMP09=sum(EMP09, na.rm=TRUE), UU2010=list(UU2010))
uu851 <- st_union(filter(iris, UU2010=="851")) %>% st_sf
uu851.e <- xt_as_extent(uu851)

grid200 <- raster(xt_as_extent(c200idf), crs=st_crs(c200idf)$proj4string,  resolution=200)
grid.r <- raster_ref(c200idf, 50)

# définition des points d'arrivée à partir des centres des iris
iris_pos <- iris %>%
  st_transform(4326) %>%
  as_tibble %>%
  transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
            Y=st_coordinates(st_centroid(geometry))[, "Y"],
            EMP09,
            P15_POP,
            cste=1)

options(future.globals.maxSize= 8*1024^3)

# GPE ------------------------------------------------------

otpcs <- map(1:1, ~OTP_server(router="IDFGPE", port=8000+10*., memory = "6G", rep=DVFdata))
otpc <- otpcs[[1]]

param <- list(otpcon=otpc,
              date = "12-19-2019", time = "08:00:00",
              cutoffs = seq(600, 3600, by=600),
              batch = FALSE, arriveBy = FALSE, walkReluctance= 2,maxWalkDistance= 800,
              transferPenalty = 0, minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=250,
              precisionMeters=50, mode="TRANSIT")

plan("multiprocess", workers=4)
gpe_tr50 <- future_aggr_isochrone_DT(iris_pos %>% select(X, Y, EMP09),
                                     param,
                                     raster_ref(c200idf, resolution=50),
                                     progress=TRUE,
                                     todisk=TRUE,
                                     otpserver = otpcs)

save_DVF(gpe_tr50)