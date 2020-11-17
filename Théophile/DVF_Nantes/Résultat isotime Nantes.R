source("access.r")

pdloire.mbr <- load_DVF("pdloire.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Nantes <- load_DVF("ttr_r5_emp09_Nantes")
tcar_osrm_emp09_Nantes <- load_DVF("tcar_osrm_emp09_Nantes")

# Construction bb33701

uu44701 <- iris15 %>% filter(UU2010=="44701") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu44701 <- uu44701 %>% st_centroid
bb44701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu44701[[1]]), crs=3035)

# construction uu33701$bbox

dpdloire <- load_DVF("dpdloire")
cpdloire <- load_DVF("cpdloire")

uu44701 <- NULL
iris15 <- load_DVF("iris15")
uu44701$iris <- iris15 %>% filter(UU2010=="44701") %>% st_union 
cpdloire <- load_DVF("cpdloire") %>% st_transform(3035)

uu44701$depsf <- dpdloire

uu44701$border <- cpdloire %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="44701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu44701$bbox <- st_bbox(uu44701$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu44701$border) %>% st_crop(uu44701$border)

# Cartes isotime sur fond de carte

ttr_r5_emp09_isotime_Nantes <- tm_shape(pdloire.mbr,bbox=bb44701)+tm_rgb()+
                               tm_shape(ttr_r5_emp09_Nantes)+tm_raster(style="cont",palette=heatrg)+
                               tm_shape(riv,bbox=uu44701$bbox)+tm_fill("dodgerblue",alpha=1)
 
tcar_osrm_emp09_isotime_Nantes <- tm_shape(pdloire.mbr,bbox=bb44701)+tm_rgb()+
                                  tm_shape(tcar_osrm_emp09_Nantes)+tm_raster(style="cont",palette=heatrg)+
                                  tm_shape(riv,bbox=uu44701$bbox)+tm_fill("dodgerblue",alpha=1)
