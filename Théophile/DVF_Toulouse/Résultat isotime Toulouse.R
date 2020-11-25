source("access.r")

lgdrous.mbr <- load_DVF("lgdrous.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Toulouse <- load_DVF("ttr_r5_emp09_Toulouse")
tcar_osrm_emp09_Toulouse <- load_DVF("tcar_osrm_emp09_Toulouse")
ttr_r5_pop15_Toulouse <- load_DVF("ttr_r5_pop15_Toulouse")
tcar_osrm_pop15_Toulouse <- load_DVF("tcar_osrm_pop15_Toulouse")

# Construction bb33701

uu31701 <- iris15 %>% filter(UU2010=="31701") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu31701 <- uu31701 %>% st_centroid
bb31701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu31701[[1]]), crs=3035)

# construction uu33701$bbox

dlgdrous <- load_DVF("dlgdrous")
clgdrous <- load_DVF("clgdrous")

uu31701 <- NULL
iris15 <- load_DVF("iris15")
uu31701$iris <- iris15 %>% filter(UU2010=="31701") %>% st_union 
clgdrous <- load_DVF("clgdrous") %>% st_transform(3035)

uu31701$depsf <- dlgdrous

uu31701$border <- clgdrous %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="31701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu31701$bbox <- st_bbox(uu31701$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu31701$border) %>% st_crop(uu31701$border)

# Cartes isotime sur fond de carte

# transport en commun --------- EMP09
ttr_r5_emp09_isotime_Toulouse <- tm_shape(lgdrous.mbr,bbox=bb31701)+tm_rgb()+
                                 tm_shape(ttr_r5_emp09_Toulouse)+tm_raster(style="cont",palette=heatrg)+
                                 tm_shape(riv,bbox=uu31701$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(ttr_r5_emp09_isotime_Toulouse, file="{DVFdata}/presentation/theophile/transit_emp09/cartes/toulouse" %>% glue)

 
# transports en communs ----------- P15_POP
 
ttr_r5_pop15_isotime_Toulouse <- tm_shape(lgdrous.mbr,bbox=bb31701)+tm_rgb()+
                                 tm_shape(ttr_r5_pop15_Toulouse)+tm_raster(style="cont",palette=heatrg)+
                                 tm_shape(riv,bbox=uu31701$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(ttr_r5_pop15_isotime_Toulouse, file="{DVFdata}/presentation/theophile/transit_P15pop/cartes/toulouse" %>% glue)


# voiture --------- EMP09

tcar_osrm_emp09_isotime_Toulouse <- tm_shape(lgdrous.mbr,bbox=bb31701)+tm_rgb()+
                              tm_shape(tcar_osrm_emp09_Toulouse)+tm_raster(style="cont",palette=heatrg)+
                              tm_shape(riv,bbox=uu31701$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(tcar_osrm_emp09_isotime_Toulouse, file="{DVFdata}/presentation/theophile/car_emp09/cartes/toulouse" %>% glue)

# voiture ----------- P15_POP

tcar_osrm_pop15_isotime_Toulouse <- tm_shape(lgdrous.mbr,bbox=bb31701)+tm_rgb()+
                                    tm_shape(tcar_osrm_pop15_Toulouse)+tm_raster(style="cont",palette=heatrg)+
                                    tm_shape(riv,bbox=uu31701$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(tcar_osrm_pop15_isotime_Toulouse, file="{DVFdata}/presentation/theophile/car_P15pop/cartes/toulouse" %>% glue)
