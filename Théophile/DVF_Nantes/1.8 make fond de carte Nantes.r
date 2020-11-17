source("access.r")

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

iris15 <- load_DVF("iris15")
c200 <- load_DVF("c200")

uu44701 <- iris15 %>% filter(UU2010=="44701") %>% st_union()
c200_44701 <- c200 %>% filter(st_within(., uu44701, sparse=FALSE))
c200_44701 %<>% st_transform(3035) %>% st_as_sf
uu44701plus20 <- uu44701 %>% st_buffer(20000)
c200_pdloire <- c200_44701 %>% filter(st_within(., uu44701plus20, sparse=FALSE))

cpdloire <- cfr %>% filter(DEP %in% c200_pdloire)
dpdloire <- cpdloire %>% group_by(DEP) %>% summarize()

save_DVF(uu44701plus20)
save_DVF(dpdloire)
save_DVF(cpdloire)

uu44701 <- NULL
iris15 <- load_DVF("iris15")
uu44701$iris <- iris15 %>% filter(UU2010=="44701") %>% st_union 
cpdloire <- load_DVF("cpdloire") %>% st_transform(3035)

dpdloire <- cpdloire %>% group_by(DEP) %>% summarize()

uu44701$depsf <- dpdloire

uu44701$border <- cpdloire %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="44701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu44701$bbox <- st_bbox(uu44701$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu44701$border) %>% st_crop(uu44701$border)

uu44701$fdc <- tm_shape(uu44701$border, bbox = uu44701$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu44701$hdc <- tm_shape(dpdloire, bbox = uu44701$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu44701$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu44701)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
pdloireplus <- iris15 %>% filter(UU2010=="44701") %>% st_buffer(33000) %>% st_union %>% st_transform(4326)
st_crs(pdloireplus) <- st_crs("+proj=longlat +ellps=WGS84")

pdloire.mbr <- cc_location(loc=pdloireplus, zoom = 9,
                        base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(pdloire.mbr, max)
pdloire.mbr <- projectRaster(from=pdloire.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
pdloire.mbr <- pdloire.mbr/cellStats(pdloire.mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035

save_DVF(pdloire.mbr)

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu44701 <- iris15 %>% filter(UU2010=="44701") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu44701 <- uu44701 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb44701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu44701[[1]]), crs=3035)

pdloire.mbfdc <- tm_shape(pdloire.mbr, bbox = bb44701)+tm_rgb()

save_DVF(pdloire.mbfdc)