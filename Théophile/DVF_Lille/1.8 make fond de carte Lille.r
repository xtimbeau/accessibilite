source("dvf.r")

# Construction cnrdcal & dnrdcal

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 #projection lambert 93 standard
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))
depNrdCal <- c("62","59")
cnrdcal <- cfr %>% filter(DEP %in% depNrdCal)
dnrdcal <- cnrdcal %>% group_by(DEP) %>% summarize()

save_DVF(cnrdcal)
save_DVF(dnrdcal)

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


riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu59702$border) %>% st_crop(uu59702$border)

uu59702$fdc <- tm_shape(uu59702$border, bbox = uu59702$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu59702$hdc <- tm_shape(dnrdcal, bbox = uu59702$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu59702$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu59702)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
nrdcalplus <- iris15 %>% filter(UU2010=="59702") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(nrdcalplus) <- st_crs("+proj=longlat +ellps=WGS84")

nrdcal.mbr <- cc_location(loc=nrdcalplus, zoom = 9,
                 base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu59702 <- iris15 %>% filter(UU2010=="59702") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu59702 <- uu59702 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb59702 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu59702[[1]]), crs=3035)

nrdcal.mbfdc <- tm_shape(nrdcal.mbr, bbox = bb59702)+tm_rgb()

save_DVF(nrdcal.mbfdc)
