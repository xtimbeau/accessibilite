source("access.r")

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

iris15 <- load_DVF("iris15")
c200 <- load_DVF("c200")

uu758 <- iris15 %>% filter(UU2010=="00758") %>% st_union()
c200_758 <- c200 %>% filter(st_within(., uu758, sparse=FALSE))
c200_758 %<>% st_transform(3035) %>% st_as_sf
uu758plus20 <- uu758 %>% st_buffer(20000)
c200_rha <- c200_758 %>% filter(st_within(., uu758plus20, sparse=FALSE))

crha <- cfr %>% filter(DEP %in% c200_rha)
drha <- crha %>% group_by(DEP) %>% summarize()

save_DVF(uu758plus20)
save_DVF(drha)
save_DVF(crha)

uu758 <- NULL
uu758$iris <- iris15 %>% filter(UU2010=="00758") %>% st_union 
crha <- load_DVF("crha") %>% st_transform(3035)

drha <- crha %>% group_by(DEP) %>% summarize()

uu758$depsf <- drha

uu758$border <- crha %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="00758") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu758$bbox <- st_bbox(uu758$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu758$border) %>% st_crop(uu758$border)

uu758$fdc <- tm_shape(uu758$border, bbox = uu758$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu758$hdc <- tm_shape(drha, bbox = uu758$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu758$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu758)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
uu758plus20 <- load_DVF("uu758plus20")
uu758plus20 <- uu758plus20 %>% st_transform(4326)  
st_crs(uu758plus20) <- st_crs("+proj=longlat +ellps=WGS84")

rha.mbr <- cc_location(loc=uu758plus20, zoom = 9,
                        base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(rha.mbr, max)
rha.mbr <- projectRaster(from=rha.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
rha.mbr <- rha.mbr/cellStats(rha.mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035


# Mise à l'échelle du fond de carte par rapport à l'aire urbaine la plus grande: Paris

iris15 <- load_DVF("iris15")
uu758 <- iris15 %>% filter(UU2010=="00758") %>% st_union # Lyon
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu758 <- uu758 %>% st_centroid

# les limites appliquées à Lyon sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lyon
bb758 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu758[[1]]), crs=3035)

rha.mbfdc <- tm_shape(rha.mbr, bbox = bb758)+tm_rgb()

save_DVF(rha.mbfdc)