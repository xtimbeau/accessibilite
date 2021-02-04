source("access.r")

# on traite quelques équipements avec une isochrone à pied
# Crèches et restaurants de la base équipement

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% 
  filter(UU2010=="00851") %>% 
  st_union() %>%
  st_buffer(4000)
idf <- iris15 %>%
  filter(UU2010=="00851") %>% 
  st_union()
c200_idf <- c200 %>%
  filter(st_within(., idf, sparse=FALSE))
rm(c200)

fdt_idf_50 <- load_DVF("fdt_idf_50") %>% swap2tmp_routing()

plan("multiprocess", workers=8)

# creches et restaurants

equipements <- foreign::read.dbf(file="{DVFdata}/Equipements/bpe_ensemble_xy.dbf" %>% glue)
equipements <- filter(equipements, dep%in%depIdf)

codes_equipements <- readxl::read_excel("{DVFdata}/equipements/codes equiments.xlsx" %>% glue)
equipements <- left_join(equipements, codes_equipements, by=c("typequ"="code_equipement"))

eq.sf <- st_as_sf(equipements %>% filter(qualite_xy%in%c("Bonne", "Acceptable", "Mauvaise")),
                  coords=c("lambert_x", "lambert_y"), crs=2154)
eq.sf <- st_transform(eq.sf, 3035)

creches <- eq.sf %>% filter(typequ=="D502") # 3 178 créches
restaurants <- eq.sf %>% filter(typequ=="A504") # 46 644 restaurants

kreches <- iso_accessibilite(
  quoi = creches %>% transmute(c=1),
  res_quoi=50,
  ou=c200_idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

save_DVF(kreches)

kestaurants <- iso_accessibilite(
  quoi = restaurants %>% transmute(c=1),
  res_quoi = 50,
  ou=c200_idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

save_DVF(kestaurants)


# # équipements (ancienne méthode) --------------------------------------------------
# 
# otpc <- OTP_server(router="IDF1", port=8090, memory="8G")
# 
# param <- list(otpcon=otpc,
#               date = "12-17-2019", time = "08:00:00",
#               cutoffs=c(300, 600, 900),
#               walkReluctance=2,
#               maxWalkDistance=10000,
#               precisionMeters=10,
#               offRoadDistanceMeters=200,
#               mode="WALK")
# 
# options(future.globals.maxSize= 16*1024^3)
# plan("multiprocess", workers=4)
# 
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
# 
