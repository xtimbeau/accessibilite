source("DVF.r")

# on traite quelques ?quipements avec une isochrone à pied
# dans un premier temps on regarde surtout les crèches (D502)

c200Ra <- load_DVF("c200Ra") %>% st_transform(3035)

# équipements --------------------------------------------------

otpc<- OTP_server(router="Lyon", port=8110, memory = "8G", rep=DVFdata)

param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=c(300, 600, 900),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=200,
              mode="WALK")

plan("multiprocess", workers=8)

# résolution 200 sur les carreaux habités

jardins <- st_read("{DVFdata}//fdCartes//espaces verts//Parcs_Lyon.shp" %>%  glue) %>% st_transform(3035)

grille_xy <- c200Ra %>%
  filter(Ind>0) %>%
  st_transform(4326) %>% 
  st_centroid %>% 
  st_coordinates %>% 
  as_tibble # renvoie 70k+ cellules (X,Y)

k_jardins_200_Lyon <- future_kernel_isochronique(sf=jardins %>% transmute(cste=1),
                                 positions=grille_xy,
                                 param=param,
                                 res_fac=4,
                                 resolution = 200,
                                 n_split=2*nbrOfWorkers())

k_jardins_200_Lyon %<>% mutate(petj_15 = cste_900/max(cste_900, na.rm=TRUE),
                          petj_10 = cste_600/max(cste_600, na.rm=TRUE),
                          petj_5 = cste_300/max(cste_300, na.rm=TRUE)) %>%   
  st_as_sf(coords=c("X", "Y"), crs=4326) %>% 
  st_transform(3035)

raster_parcsetjardins_200_Lyon <- rastervar(k_jardins_200_Lyon, petj_5, petj_10, petj_15, resolution=200)
save_DVF(raster_parcsetjardins_200_Lyon)

# résolution 50 sur les carreaux avec transaction

dv3fv4 <- load_DVF("dv3fv4")
h_pos <- dv3fv4 %>%
  pull(IdINS_50) %>% 
  unique() %>% 
  idINS2point(resolution = 50) %>% 
  sf_project(from=st_crs(3035), to=st_crs(4326), .)
colnames(h_pos) <- c("X", "Y")
h_pos %<>% as_tibble

k_jardins_50_Lyon <- future_kernel_isochronique(sf= jardins %>% transmute(cste=1),
                                       positions=h_pos,
                                       param=param,
                                       res_fac=1,
                                       otpserver=otpcs, resolution=50)

k_jardins_50_Lyon %<>% mutate(petj_15 = cste_900/max(cste_900, na.rm=TRUE),
                      petj_10 = cste_600/max(cste_600, na.rm=TRUE),
                      petj_5 = cste_300/max(cste_300, na.rm=TRUE)) %>% 
  st_as_sf(coords=c("X", "Y"), crs=4326) %>% 
  st_transform(3035)
raster_parcsetjardins_50_Lyon <- rastervar(k_jardins_50, petj_5, petj_10, petj_15)
save_DVF(raster_parcsetjardins_50_Lyon)

# equipements <- foreign::read.dbf(file="{DVFdata}/Equipements/bpe_ensemble_xy.dbf" %>% glue)
# equipements <- filter(equipements, dep%in%depIdf)
# 
# codes_equipements <- readxl::read_excel("{DVFdata}/equipements/codes equiments.xlsx" %>% glue)
# equipements <- left_join(equipements, codes_equipements, by=c("typequ"="code_equipement"))
# 
# eq.sf <- st_as_sf(equipements %>% filter(qualite_xy%in%c("Bonne", "Acceptable", "Mauvaise")),
#                   coords=c("lambert_x", "lambert_y"), crs=2154)
# eq.sf <- st_transform(eq.sf, 3035)
# 
# creches <- eq.sf %>% filter(typequ=="D502") # 3 178 créches
# restaurants <- eq.sf %>% filter(typequ=="A504") # 46 644 restaurants
# 
# creches_pos <- creches %>%
#   st_transform(4326) %>%
#   as_tibble %>%
#   transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
#             Y=st_coordinates(st_centroid(geometry))[, "Y"])
# 
# restaurants_pos <- restaurants %>%
#   st_transform(4326) %>%
#   as_tibble %>%
#   transmute(X=st_coordinates(st_centroid(geometry))[, "X"],
#             Y=st_coordinates(st_centroid(geometry))[, "Y"],
#             cste=1L)
# 
# creches_pos <- future_kernel_isochronique(sf=c200idf %>% select(Ind),
#                                           positions=creches_pos,
#                                           param=param,
#                                           res_fac=4,
#                                           n_split=2*nbrOfWorkers())
# 
# creches_pos_n <- creches_pos %>%
#   filter(Ind_300>500) %>%
#   transmute(X,Y, cste=1L,
#             pop_n_10mn=round(min(Ind_600)/Ind_600*2^16))
# 
# plan("multiprocess", workers=4)
# creches_isoac_10 <- future_aggr_isochrone_DT(creches_pos_n,
#                                              param,
#                                              raster_ref(c200idf, resolution=10),
#                                              progress=TRUE, timing=TRUE, todisk=TRUE)
# 
# save_DVF(creches_isoac_10)
# 
# rm(creches_pos_n, creches_isoac_10)
# 
# plan("multiprocess", workers=4)
# rest_isoac_10 <- future_aggr_isochrone_DT(restaurants_pos ,
#                                           param,
#                                           raster_ref(c200idf, resolution=10),
#                                           progress=TRUE, n_split = nbrOfWorkers(), todisk=TRUE)
# save_DVF(rest_isoac_10)
# 
# tmap_options(max.raster = c(plot = 1e+9, view = 1e+9))
# tmap_mode("view")
# 
# tm_shape(rest_isoac_10$bricks$cste[["iso10mn"]])+tm_raster(style="cont", palette=green2gray)

