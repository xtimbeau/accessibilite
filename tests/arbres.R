source("access.r")

res <- 25
arbres <- st_read("{DVFdata}/fdcartes/arbres/20190318-referentiel-arbre-namr.geojson" %>% glue)
arbres_idf <- arbres %>% filter(code_dept%in%depIdf) %>% st_transform(3035)

arbrr <- rasterize(arbres_idf %>% as_Spatial(), raster_ref(arbres_idf, resolution=res), field=1, fun=sum)


ecomos <- st_read("{DVFdata}/fdcartes/ecomos/ecomos-idf.shp" %>% glue)
ecmnr <- ecomos %>% filter(!clc6%in%c(231114, 332202 , 0)) %>% st_transform(3035) %>% fasterize(raster = raster_ref(uu851$iris %>% st_buffer(2000), resolution=res), fun="any")

tmap_mode("plot")
tm_shape(ecmnr>0)+tm_raster(palette=c(NULL, "red"))+tm_shape(mask(arbrr, (arbrr>20), maskvalue=0))+tm_raster(alpha=0.5, style="kmeans")


c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(4000)
plot(idf4km)
