source("access.r")

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

iris15 <- load_DVF("iris15")
c200 <- load_DVF("c200")

uu35701 <- iris15 %>% filter(UU2010=="35701") %>% st_union()
c200_35701 <- c200 %>% filter(st_within(., uu35701, sparse=FALSE))
c200_35701 %<>% st_transform(3035) %>% st_as_sf
uu35701plus20 <- uu758 %>% st_buffer(20000)
c200_bre <- c200_35701 %>% filter(st_within(., uu35701plus20, sparse=FALSE))

cbre <- cfr %>% filter(DEP %in% c200_bre)
dbre <- cbre %>% group_by(DEP) %>% summarize()

save_DVF(uu35701plus20)
save_DVF(dbre)
save_DVF(cbre)

uu35701 <- NULL
uu35701$iris <- iris15 %>% filter(UU2010=="35701") %>% st_union 
cbre <- load_DVF("crha") %>% st_transform(3035)

dbre <- cbre %>% group_by(DEP) %>% summarize()

uu35701$depsf <- dbre

uu35701$border <- cbre %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="35701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu35701$bbox <- st_bbox(uu35701$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu35701$border) %>% st_crop(uu35701$border)

uu35701$fdc <- tm_shape(uu35701$border, bbox = uu35701$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu35701$hdc <- tm_shape(dbre, bbox = uu35701$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu35701$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu35701)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
breplus <- iris15 %>% filter(UU2010=="35701") %>% st_buffer(40000) %>% st_union %>% st_transform(4326)
st_crs(breplus) <- st_crs("+proj=longlat +ellps=WGS84") 

bre.mbr <- cc_location(loc=breplus, zoom = 9,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(bre.mbr, max)
bre.mbr <- projectRaster(from=bre.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
bre.mbr <- bre.mbr/cellStats(bre.mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035

save_DVF(bre.mbr)

# Mise à l'échelle du fond de carte par rapport à l'aire urbaine la plus grande: Paris

iris15 <- load_DVF("iris15")
uu35701 <- iris15 %>% filter(UU2010=="35701") %>% st_union # Lyon
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu35701 <- uu35701 %>% st_centroid

# les limites appliquées à Lyon sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lyon
bb35701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu35701[[1]]), crs=3035)

bre.mbfdc <- tm_shape(bre.mbr, bbox = bb35701)+tm_rgb()

save_DVF(bre.mbfdc)