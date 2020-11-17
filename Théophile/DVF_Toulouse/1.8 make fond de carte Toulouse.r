source("access.r")

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

iris15 <- load_DVF("iris15")
c200 <- load_DVF("c200")

uu31701 <- iris15 %>% filter(UU2010=="31701") %>% st_union()
c200_31701 <- c200 %>% filter(st_within(., uu31701, sparse=FALSE))
c200_31701 %<>% st_transform(3035) %>% st_as_sf
uu31701plus20 <- uu31701 %>% st_buffer(20000)
c200_lgdrous <- c200_31701 %>% filter(st_within(., uu31701plus20, sparse=FALSE))

clgdrous <- cfr %>% filter(DEP %in% c200_lgdrous)
dlgdrous <- clgdrous %>% group_by(DEP) %>% summarize()

save_DVF(dlgdrous)
save_DVF(clgdrous)

uu31701 <- NULL
iris15 <- load_DVF("iris15")
uu31701$iris <- iris15 %>% filter(UU2010=="31701") %>% st_union 
clgdrous <- load_DVF("clgdrous") %>% st_transform(3035)

dlgdrous <- clgdrous %>% group_by(DEP) %>% summarize()

uu31701$depsf <- dlgdrous

uu31701$border <- clgdrous %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="31701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu31701$bbox <- st_bbox(uu31701$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu31701$border) %>% st_crop(uu31701$border)

uu31701$fdc <- tm_shape(uu31701$border, bbox = uu31701$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu31701$hdc <- tm_shape(dlgdrous, bbox = uu31701$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu31701$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu31701)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
lgdrousplus <- iris15 %>% filter(UU2010=="31701") %>% st_buffer(35000) %>% st_union %>% st_transform(4326)
st_crs(lgdrousplus) <- st_crs("+proj=longlat +ellps=WGS84")

lgdrous.mbr <- cc_location(loc=lgdrousplus, zoom = 9,
                        base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(lgdrous.mbr, max)
lgdrous.mbr <- projectRaster(from=lgdrous.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
lgdrous.mbr <- lgdrous.mbr/cellStats(lgdrous.mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035

save_DVF(lgdrous.mbr)

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu31701 <- iris15 %>% filter(UU2010=="31701") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu31701 <- uu31701 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb31701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu31701[[1]]), crs=3035)

lgdrous.mbfdc <- tm_shape(lgdrous.mbr, bbox = bb31701)+tm_rgb()

save_DVF(lgdrous.mbfdc)