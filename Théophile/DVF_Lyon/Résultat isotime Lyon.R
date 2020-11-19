source("access.r")

rha.mbr <- load_DVF("rha.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Lyon <- load_DVF("ttr_r5_emp09_Lyon")
tcar_osrm_emp09_Lyon <- load_DVF("tcar_osrm_emp09_Lyon")
ttr_r5_pop15_Lyon <- load_DVF("ttr_r5_pop15_Lyon")
tcar_osrm_pop15_Lyon <- load_DVF("tcar_osrm_pop15_Lyon")

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

# transport en commun --------- EMP09
ttr_r5_emp09_isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
                    tm_shape(ttr_r5_emp09_Lyon$to100k)+tm_raster(style="cont",palette=heatrg)+
                    tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(isotime_Lyon, file="{DVFdata}/presentation/vv/isotime_Lyon" %>% glue)

isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
  tm_shape(ttr_r5_emp09_Lyon$to25k)+tm_raster(style="cont",palette=heatrg)+
  tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(isotime_Lyon, file="{DVFdata}/presentation/vv/isotime_Lyon 25k" %>% glue)

# transports en commun ---------- pop15

ttr_r5_pop_isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
  tm_shape(ttr_r5_pop15_Lyon)+tm_raster(style="cont",palette=heatrg)+
  tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)

# transports en commun --------- EMP09

ttr_r5_emp09_isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
  tm_shape(ttr_r5_emp09_Lyon)+tm_raster(style="cont",palette=heatrg)+
  tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)

# voiture --------- EMP09

tcar_osrm_emp09_isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
                          tm_shape(tcar_osrm_emp09_Lyon)+tm_raster(style="cont",palette=heatrg)+
                          tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)

# voiture ---------- pop15

tcar_osrm_pop15_isotime_Lyon <- tm_shape(rha.mbr,bbox=bb758)+tm_rgb()+
  tm_shape(tcar_osrm_pop15_Lyon)+tm_raster(style="cont",palette=heatrg)+
  tm_shape(riv,bbox=uu758$bbox)+tm_fill("dodgerblue",alpha=1)