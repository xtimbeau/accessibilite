source("DVF.r")

#grille carreau 200m projection 3035
#g200idf est la grille simple, c200idf la grille plus les infos d?mographiques
#dans g200idf il y a une colonne iris, qui permet de sélectionner les 65000 carreaux de l'aire urbaine

c200Lgdrous <- load_DVF("c200Lgdrous")
c200Lgdrous %<>% st_transform(3035)
iris15 <- load_DVF("iris15")
uu <- filter(iris15, UU2010=="31701") %>% pull(COM) # attention aux guillemets
depPaca <- c("82","81","32","31")

# j'ajoute ce bout de code pour filter une zone moins grande
lgdrous <- iris15 %>% filter(UU2010=="31701") %>% st_buffer(5000) %>% st_union # prend l'aire urbaine plus 5km

# on vérifie qu'on est bon
tmap_mode("view")
tm_shape(lgdrous)+tm_borders()

# c'est assez puissant : on sélectionne sur une base géoghraphique
# . veut dire l'argument qui précède %>% (iris15 dans ce cas)
iris_lgdrous <- iris15 %>% filter(st_within(., lgdrous, sparse=FALSE)) # 678 iris
lgdrous_box <- st_bbox(lgdrous %>% st_transform(4326))

# idem pour la grille (23000 carreaux)
c200_lgdrous <- c200Lgdrous %>% filter(st_within(., lgdrous, sparse=FALSE)) 
dLgdrous <- iris15 %>% filter(DEP%in%depLgdrous) %>% group_by(DEP) %>% summarize(P15_POP=sum(P15_POP), UU2010=list(UU2010))

grid200 <- raster(xt_as_extent(c200Lgdrous), crs=st_crs(c200Lgdrous)$proj4string, resolution=200)
grid.r <- raster_ref(c200Lgdrous, 50)

# définition des points d'arrivée à partir des centres des iris
# il faut se limiter à rha, moins de carreaux, moins d'iris, plus de chance que ça marche

iris_pos <- iris_lgdrous %>%
  st_transform(4326) %>%
  as_tibble %>%
  transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
            Y=st_coordinates(st_centroid(geometry))[, "Y"],
            EMP09,
            P15_POP,
            cste=1)

otpc<- OTP_server(router="Toulouse", port=8110, memory = "8G", rep=DVFdata)

# Calcul de la carte transport en commun ------------------------------------------

plan(multiprocess, workers=4)
param <- list(otpcon=otpc,
              date = "10-19-2020", time = "08:00:00",
              cutoffs = seq(600, 3600, by=300), # les temps sont en seconde
              batch = FALSE, arriveBy = FALSE, walkReluctance= 2, maxWalkDistance= 10000,
              transferPenalty = 0, minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=50,
              precisionMeters=50, mode="TRANSIT")

iso_transit_50_Toulouse <- future_aggr_isochrone_DT(iris_pos,
                                           param,
                                           raster_ref(c200_lgdrous, resolution=50),
                                           progress=TRUE,
                                           todisk=TRUE)

tmap_mode("plot")
tm_shape(iso_transit_50_Toulouse$bricks$EMP09)+tm_raster()

save_DVF(iso_transit_50_Toulouse)

# Calcul de la carte voiture ------------------------------------------------------

plan("multiprocess", workers=4)
param <- list(otpcon=otpc,
              date = "09-10-2020", time = "08:00:00",
              cutoffs = seq(600, 1800, by=300),
              batch = FALSE, arriveBy = FALSE, walkReluctance= 2, maxWalkDistance= 10000,
              transferPenalty = 0, minTransferTime = 0, clampInitialWait=0, offRoadDistanceMeters=250,
              precisionMeters=50, mode="CAR")

iso_car_50_Toulouse <- future_aggr_isochrone_DT(iris_pos,
                                       param,
                                       raster_ref(c200_lgdrous, resolution=50),
                                       progress=TRUE,
                                       todisk=TRUE)

save_DVF(iso_car_50_Toulouse)

