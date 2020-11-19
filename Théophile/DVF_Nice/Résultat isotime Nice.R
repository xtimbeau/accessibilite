source("access.r")

paca_nice.mbr <- load_DVF("paca_nice.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Nice <- load_DVF("ttr_r5_emp09_Nice")
tcar_osrm_emp09_Nice <- load_DVF("tcar_osrm_emp09_Nice")

# Construction bb33701

uu06701 <- iris15 %>% filter(UU2010=="06701") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu06701 <- uu06701 %>% st_centroid
bb06701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu06701[[1]]), crs=3035)

# construction uu33701$bbox

dpaca_nice <- load_DVF("dpaca_nice")
cpaca_nice <- load_DVF("cpaca_nice")

uu06701 <- NULL
iris15 <- load_DVF("iris15")
uu06701$iris <- iris15 %>% filter(UU2010=="06701") %>% st_union 
cpaca_nice <- load_DVF("cpaca_nice") %>% st_transform(3035)

uu06701$depsf <- dpaca_nice

uu06701$border <- cpaca_nice %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="06701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu06701$bbox <- st_bbox(uu06701$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu06701$border) %>% st_crop(uu06701$border)

# Cartes isotime sur fond de carte

ttr_r5_emp09_isotime_Nice <- tm_shape(paca_nice.mbr,bbox=bb06701)+tm_rgb()+
                             tm_shape(ttr_r5_emp09_Nice)+tm_raster(style="cont",palette=heatrg)+
                             tm_shape(riv,bbox=uu06701$bbox)+tm_fill("dodgerblue",alpha=1)

tcar_osrm_emp09_isotime_Nice <- tm_shape(paca_nice.mbr,bbox=bb06701)+tm_rgb()+
                                 tm_shape(tcar_osrm_emp09_Nice)+tm_raster(style="cont",palette=heatrg)+
                                 tm_shape(riv,bbox=uu06701$bbox)+tm_fill("dodgerblue",alpha=1)
