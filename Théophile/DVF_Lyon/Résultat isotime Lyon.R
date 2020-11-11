source("access.r")

rha.mbr <- load_DVF("rha.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Lyon <- load_DVF("ttr_r5_emp09_Lyon")

# Construction bb33701

uu758 <- iris15 %>% filter(UU2010=="00758") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu758 <- uu758 %>% st_centroid
bb758 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu758[[1]]), crs=3035)

# construction uu33701$bbox

drha <- load_DVF("drha")
crha <- load_DVF("crha")

uu758 <- NULL
iris15 <- load_DVF("iris15")
uu758$iris <- iris15 %>% filter(UU2010=="00758") %>% st_union 
crha <- load_DVF("crha") %>% st_transform(3035)

uu758$depsf <- drha

uu758$border <- crha %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="00758") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu758$bbox <- st_bbox(uu758$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu758$border) %>% st_crop(uu758$border)

# Cartes isotime sur fond de carte

isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
                    tm_shape(ttr_r5_emp09_Lyon)+tm_raster(style="cont",palette=heatrg)+
                    tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)
