source("access.r")

nrdcal.mbr <- load_DVF("nrdcal.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Lille <- load_DVF("ttr_r5_emp09_Lille")
tcar_osrm_emp09_Lille <- load_DVF("tcar_osrm_emp09_Lille")
ttr_r5_pop15_Lille <- load_DVF("ttr_r5_pop15_Lille")
tcar_osrm_pop15_Lille <- load_DVF("tcar_osrm_pop15_Lille")


# Construction bb33701

uu59702 <- iris15 %>% filter(UU2010=="59702") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu59702 <- uu59702 %>% st_centroid
bb59702 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu59702[[1]]), crs=3035)

# construction uu33701$bbox

dnrdcal <- load_DVF("dnrdcal")
cnrdcal <- load_DVF("cnrdcal")

uu59702 <- NULL
iris15 <- load_DVF("iris15")
uu59702$iris <- iris15 %>% filter(UU2010=="59702") %>% st_union 
cnrdcal <- load_DVF("cnrdcal") %>% st_transform(3035)

uu59702$depsf <- dnrdcal

uu59702$border <- cnrdcal %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="59702") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu59702$bbox <- st_bbox(uu59702$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu59702$border) %>% st_crop(uu59702$border)

# Cartes isotime sur fond de carte

ttr_r5_emp09_isotime_Lille <- tm_shape(nrdcal.mbr,bbox=bb59702)+tm_rgb()+
                              tm_shape(ttr_r5_emp09_Lille)+tm_raster(style="cont",palette=heatrg)+
                              tm_shape(riv,bbox=uu59702$bbox)+tm_fill("dodgerblue",alpha=1)+
                              tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(ttr_r5_emp09_isotime_Lille, file="{DVFdata}/presentation/theophile/transit_emp09/cartes/lille" %>% glue)

ttr_r5_pop15_isotime_Lille <- tm_shape(nrdcal.mbr,bbox=bb59702)+tm_rgb()+
                              tm_shape(ttr_r5_pop15_Lille)+tm_raster(style="cont",palette=heatrg)+
                              tm_shape(riv,bbox=uu59702$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(ttr_r5_emp09_isotime_Lille, file="{DVFdata}/presentation/theophile/transit_P15pop/cartes/lille" %>% glue)


tcar_osrm_emp09_isotime_Lille <- tm_shape(nrdcal.mbr,bbox=bb59702)+tm_rgb()+
                                 tm_shape(tcar_osrm_emp09_Lille)+tm_raster(style="cont",palette=heatrg)+
                                 tm_shape(riv,bbox=uu59702$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(tcar_osrm_emp09_isotime_Lille, file="{DVFdata}/presentation/theophile/car_emp09/cartes/lille" %>% glue)

tcar_osrm_pop15_isotime_Lille <- tm_shape(nrdcal.mbr,bbox=bb59702)+tm_rgb()+
                                 tm_shape(tcar_osrm_pop15_Lille)+tm_raster(style="cont",palette=heatrg)+
                                 tm_shape(riv,bbox=uu59702$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(tcar_osrm_pop15_isotime_Lille, file="{DVFdata}/presentation/theophile/car_P15pop/cartes/lille" %>% glue)
