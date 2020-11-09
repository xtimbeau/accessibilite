source("dvf.r")
cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 #projection lambert 93 standard
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))
depPaca <- c("83","84","04","13")
cpaca <- cfr %>% filter(DEP %in% depPaca)
dpaca <- cpaca %>% group_by(DEP) %>% summarize()

save_DVF(dpaca)
save_DVF(cpaca)

uu759 <- NULL
iris15 <- load_DVF("iris15")
uu759$iris <- iris15 %>% filter(UU2010=="00759") %>% st_union 
cpaca <- load_DVF("cpaca") %>% st_transform(3035)

dpaca <- cpaca %>% group_by(DEP) %>% summarize()

uu759$depsf <- dpaca

uu759$border <- cpaca %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="00759") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu759$bbox <- st_bbox(uu759$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu759$border) %>% st_crop(uu759$border)

uu759$fdc <- tm_shape(uu759$border, bbox = uu759$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu759$hdc <- tm_shape(dpaca, bbox = uu759$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu759$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu759)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
pacaplus <- iris15 %>% filter(UU2010=="00759") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(pacaplus) <- st_crs("+proj=longlat +ellps=WGS84")

paca.mbr <- cc_location(loc=pacaplus, zoom = 9,
                        base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu759 <- iris15 %>% filter(UU2010=="00759") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu759 <- uu759 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb759 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu759[[1]]), crs=3035)

paca.mbfdc <- tm_shape(paca.mbr, bbox = bb759)+tm_rgb()

save_DVF(paca.mbfdc)