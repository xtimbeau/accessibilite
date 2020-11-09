source("dvf.r")

# Construction cnrdcal & dnrdcal

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 #projection lambert 93 standard
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))
depNice_Paca <- c("06, 04, 83")
cnice_paca <- cfr %>% filter(DEP %in% depNice_Paca)
dnice_paca <- cnice_paca %>% group_by(DEP) %>% summarize()

save_DVF(cnice_paca)
save_DVF(dnice_paca)

uu06701 <- NULL
iris15 <- load_DVF("iris15")
uu06701$iris <- iris15 %>% filter(UU2010=="06701") %>% st_union 
cnice_paca <- load_DVF("cnice_paca") %>% st_transform(3035)

uu06701$depsf <- dnice_paca

uu06701$border <- cnice_paca %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="06701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu06701$bbox <- st_bbox(uu06701$border)


riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu06701$border) %>% st_crop(uu06701$border)

uu06701$fdc <- tm_shape(uu06701$border, bbox = uu06701$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu06701$hdc <- tm_shape(dnice_paca, bbox = uu06701$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu06701$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu06701)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
nice_pacaplus <- iris15 %>% filter(UU2010=="06701") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(nice_pacaplus) <- st_crs("+proj=longlat +ellps=WGS84")

nice_paca.mbr <- cc_location(loc=nice_pacaplus, zoom = 9,
                           base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu06701 <- iris15 %>% filter(UU2010=="06701") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu06701 <- uu06701 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb06701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu06701[[1]]), crs=3035)
nice_paca.mbfdc <- tm_shape(nice_paca.mbr, bbox = bb06701)+tm_rgb()

save_DVF(nice_paca.mbfdc)