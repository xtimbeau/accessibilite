source("dvf.r")
cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 #projection lambert 93 standard
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))
depAls <- c("67","68","57","54","88")
cals <- cfr %>% filter(DEP %in% depAls)
dals <- cals %>% group_by(DEP) %>% summarize()

save_DVF(dals)
save_DVF(cals)

uu67701 <- NULL
iris15 <- load_DVF("iris15")
uu67701$iris <- iris15 %>% filter(UU2010=="67701") %>% st_union 
cals <- load_DVF("cals") %>% st_transform(3035)

dals <- cals %>% group_by(DEP) %>% summarize()

uu67701$depsf <- dals

uu67701$border <- cals %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="67701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu67701$bbox <- st_bbox(uu67701$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu67701$border) %>% st_crop(uu67701$border)

uu67701$fdc <- tm_shape(uu67701$border, bbox = uu67701$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu67701$hdc <- tm_shape(dals, bbox = uu67701$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu67701$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu67701)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
alsplus <- iris15 %>% filter(UU2010=="67701") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(alsplus) <- st_crs("+proj=longlat +ellps=WGS84")

als.mbr <- cc_location(loc=alsplus, zoom = 9,
                        base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu67701 <- iris15 %>% filter(UU2010=="67701") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu67701 <- uu67701 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb67701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu67701[[1]]), crs=3035)

als.mbfdc <- tm_shape(als.mbr, bbox = bb67701)+tm_rgb()

save_DVF(als.mbfdc)