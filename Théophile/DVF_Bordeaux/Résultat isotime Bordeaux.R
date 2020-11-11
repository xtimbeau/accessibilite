source("access.r")

aqui.mbr <- load_DVF("aqui.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Bordeaux <- load_DVF("ttr_r5_emp09_Bordeaux")

# Construction bb33701

uu33701 <- iris15 %>% filter(UU2010=="33701") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu33701 <- uu33701 %>% st_centroid
bb33701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu33701[[1]]), crs=3035)

# construction uu33701$bbox

daqui <- load_DVF("daqui")
caqui <- load_DVF("caqui")

uu33701 <- NULL
iris15 <- load_DVF("iris15")
uu33701$iris <- iris15 %>% filter(UU2010=="33701") %>% st_union 
caqui <- load_DVF("caqui") %>% st_transform(3035)

uu33701$depsf <- daqui

uu33701$border <- caqui %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="33701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu33701$bbox <- st_bbox(uu33701$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu33701$border) %>% st_crop(uu33701$border)

# Cartes isotime sur fond de carte

isotime_Bordeaux <- tm_shape(aqui.mbr,bbox=bb33701)+tm_rgb()+
                    tm_shape(ttr_r5_emp09_Bordeaux$to100k)+tm_raster(style="cont",palette=heatrg)+
                    tm_shape(riv,bbox=uu33701$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(isotime_Bordeaux, file="{DVFdata}/presentation/vv/isotime_Bordeaux" %>% glue)

isotime_Bordeaux <- tm_shape(aqui.mbr,bbox=bb33701)+tm_rgb()+
  tm_shape(ttr_r5_emp09_Bordeaux$to25k)+tm_raster(style="cont",palette=heatrg)+
  tm_shape(riv,bbox=uu33701$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(isotime_Bordeaux, file="{DVFdata}/presentation/vv/isotime_Bordeaux 25k" %>% glue)
