source("access.r")

# on traite quelques ?quipements avec une isochrone à pied
# dans un premier temps on regarde surtout les crèches (D502)

c200 <- load_DVF("c200") 
iris15 <- load_DVF("iris15")
uu33701 <- iris15 %>% filter(UU2010=="33701") %>% st_union
c200_33701 <- c200 %>% filter(st_within(., uu33701, sparse=FALSE))


ecomos <- st_read("{DVFdata}/fdcartes/ecomos/ecomos-idf.shp" %>% glue) %>% st_transform(3035)
ecomos_aqui <- ecomos %>% filter(!clc6%in%c(231114, 332202 , 0)) %>% filter(st_within(., uu33701$iris %>% st_union %>% st_buffer(2000), sparse=FALSE))


# OSRM ecomos pour l'idf

foot_osrm_Bordeaux <- routing_setup_osrm(server="5002", profile="walk")

iso_f_ecomos_50_osrm_Bordeaux <- iso_accessibilite(
  quoi=ecomos_aqui %>% transmute(c=1),
  ou=c200_33701,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=foot_osrm_Bordeaux)

save_DVF(iso_f_ecomos_50_osrm_Bordeaux)




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

