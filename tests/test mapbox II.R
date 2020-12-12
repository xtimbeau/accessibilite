
library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
rhaplus <- iris15 %>% filter(UU2010=="00758") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(rhaplus) <- st_crs("+proj=longlat +ellps=WGS84")

rha.mbr <- cc_location(loc=rhaplus, zoom = 9,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")
maxs <- cellStats(rha.mbr, max)
rha.mbr <- projectRaster(from=rha.mbr, crs=CRS(SRS_string="EPSG:3035"))
rha.mbr <- rha.mbr/cellStats(rha.mbr, max)*maxs %>% as.integer
# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu758 <- iris15 %>% filter(UU2010=="00758") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu758 <- uu758 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb758 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu758[[1]]), crs=3035)

rha.mbfdc <- tm_shape(rha.mbr, bbox = bb758)+tm_rgb()
