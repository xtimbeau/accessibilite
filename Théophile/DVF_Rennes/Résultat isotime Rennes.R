source("access.r")

bre.mbr <- load_DVF("bre.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Rennes <- load_DVF("ttr_r5_emp09_Rennes")

# Construction bb33701

uu35701 <- iris15 %>% filter(UU2010=="35701") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu35701 <- uu35701 %>% st_centroid
bb35701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu35701[[1]]), crs=3035)

# construction uu33701$bbox

dbre <- load_DVF("dbre")
cbre <- load_DVF("cbre")

uu35701 <- NULL
iris15 <- load_DVF("iris15")
uu35701$iris <- iris15 %>% filter(UU2010=="35701") %>% st_union 
cbre <- load_DVF("cbre") %>% st_transform(3035)

uu35701$depsf <- dbre

uu35701$border <- cbre %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="35701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu35701$bbox <- st_bbox(uu35701$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu35701$border) %>% st_crop(uu35701$border)

# Cartes isotime sur fond de carte

isotime_Rennes <- tm_shape(bre.mbr,bbox=bb35701)+tm_rgb()+
                    tm_shape(ttr_r5_emp09_Rennes)+tm_raster(style="cont",palette=heatrg)+
                    tm_shape(riv,bbox=uu35701$bbox)+tm_fill("dodgerblue",alpha=1)
