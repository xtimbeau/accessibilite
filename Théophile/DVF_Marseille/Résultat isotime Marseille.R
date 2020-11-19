source("access.r")

paca.mbr <- load_DVF("paca.mbr")
iris15 <- load_DVF("iris15")
ttr_r5_emp09_Marseille <- load_DVF("ttr_r5_emp09_Marseille")
tcar_osrm_emp09_Marseille <- load_DVF("tcar_osrm_emp09_Marseille")
ttr_r5_pop15_Marseille <- load_DVF("ttr_r5_pop15_Marseille")
tcar_osrm_pop15_Marseille <- load_DVF("tcar_osrm_pop15_Marseille")

# Construction bb33701

uu759 <- iris15 %>% filter(UU2010=="00759") %>% st_union 
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
c_uu851 <- uu851 %>% st_centroid
c_uu759 <- uu759 %>% st_centroid
bb759 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu759[[1]]), crs=3035)

# construction uu33701$bbox

dpaca <- load_DVF("dpaca")
cpaca <- load_DVF("cpaca")

uu759 <- NULL
iris15 <- load_DVF("iris15")
uu759$iris <- iris15 %>% filter(UU2010=="00759") %>% st_union 
cpaca <- load_DVF("cpaca") %>% st_transform(3035)

uu759$depsf <- dpaca

uu759$border <- cpaca %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="00759") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu759$bbox <- st_bbox(uu759$border)

# construction riv

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu759$border) %>% st_crop(uu759$border)

# Cartes isotime sur fond de carte

ttr_r5_emp09_isotime_Marseille <- tm_shape(paca.mbr,bbox=bb759)+tm_rgb()+
                     tm_shape(ttr_r5_emp09_Marseille$to100k)+tm_raster(style="cont",palette=heatrg)+
                     tm_shape(riv,bbox=uu759$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(isotime_Marseille, file="{DVFdata}/presentation/vv/isotime_Marseille" %>% glue)

isotime_r5_emp09_Marseille <- tm_shape(paca.mbr,bbox=bb759)+tm_rgb()+
  tm_shape(ttr_r5_emp09_Marseille$to25k)+tm_raster(style="cont",palette=heatrg)+
  tm_shape(riv,bbox=uu759$bbox)+tm_fill("dodgerblue",alpha=1)+
  tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(isotime_Marseille, file="{DVFdata}/presentation/vv/isotime_Marseille 25k" %>% glue)

ttr_r5_emp09_isotime_Marseille <- tm_shape(paca.mbr,bbox=bb759)+tm_rgb()+
                                  tm_shape(ttr_r5_emp09_Marseille)+tm_raster(style="cont",palette=heatrg)+
                                  tm_shape(riv,bbox=uu759$bbox)+tm_fill("dodgerblue",alpha=1)

tcar_osrm_emp09_isotime_Marseille <- tm_shape(paca.mbr,bbox=bb759)+tm_rgb()+
                                     tm_shape(tcar_osrm_emp09_Marseille)+tm_raster(style="cont",palette=heatrg)+
                                     tm_shape(riv,bbox=uu759$bbox)+tm_fill("dodgerblue",alpha=1)

ttr_r5_po15_isotime_Marseille <- tm_shape(paca.mbr,bbox=bb759)+tm_rgb()+
                                 tm_shape(ttr_r5_pop15_Marseille)+tm_raster(style="cont",palette=heatrg)+
                                 tm_shape(riv,bbox=uu759$bbox)+tm_fill("dodgerblue",alpha=1)

tcar_osrm_pop15_isotime_Marseille <- tm_shape(paca.mbr,bbox=bb759)+tm_rgb()+
                                     tm_shape(tcar_osrm_pop15_Marseille)+tm_raster(style="cont",palette=heatrg)+
                                     tm_shape(riv,bbox=uu759$bbox)+tm_fill("dodgerblue",alpha=1)