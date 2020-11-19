source("access.r")

als.mbr <- load_DVF("als.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Strasbourg <- load_DVF("ttr_r5_emp09_Strasbourg")
tcar_osrm_emp09_Strasbourg <- load_DVF("tcar_osrm_emp09_Strasbourg")

# Construction bb33701

uu67701 <- iris15 %>% filter(UU2010=="67701") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu67701 <- uu67701 %>% st_centroid
bb67701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu67701[[1]]), crs=3035)

# construction uu33701$bbox

dals <- load_DVF("dals")
cals <- load_DVF("cals")

uu67701 <- NULL
iris15 <- load_DVF("iris15")
uu67701$iris <- iris15 %>% filter(UU2010=="67701") %>% st_union 
cals <- load_DVF("cals") %>% st_transform(3035)

uu67701$depsf <- dals

uu67701$border <- cals %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="67701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu67701$bbox <- st_bbox(uu67701$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu67701$border) %>% st_crop(uu67701$border)

# Cartes isotime sur fond de carte

# transport en commun --------- EMP09
ttr_r5_emp09_isotime_Strasbourg <- tm_shape(als.mbr,bbox=bb67701)+tm_rgb()+
                                   tm_shape(ttr_r5_emp09_Strasbourg)+tm_raster(style="cont",palette=heatrg)+
                                   tm_shape(riv,bbox=uu67701$bbox)+tm_fill("dodgerblue",alpha=1)

# voiture --------- EMP09

tcar_osrm_emp09_isotime_Strasbourg <- tm_shape(als.mbr,bbox=bb67701)+tm_rgb()+
                                      tm_shape(tcar_osrm_emp09_Strasbourg)+tm_raster(style="cont",palette=heatrg)+
                                      tm_shape(riv,bbox=uu67701$bbox)+tm_fill("dodgerblue",alpha=1)