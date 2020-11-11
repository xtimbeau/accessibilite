source("access.r")

#Construction caqui & daqui
cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 #projection lambert 93 standard
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

c200 <- load_DVF("c200")
uu33701 <- iris15 %>% filter(UU2010=="33701") %>% st_union()
c200_33701 <- c200 %>% filter(st_within(., uu33701, sparse=FALSE))
c200_33701 %<>% st_transform(3035) %>% st_as_sf
uu33701plus20 <- uu33701 %>% st_buffer(20000)
c200_aqui <- c200_33701 %>% filter(st_within(., uu33701plus20, sparse=FALSE))

caqui <- cfr %>% filter(DEP %in% c200_aqui)
daqui <- caqui %>% group_by(DEP) %>% summarize()

save_DVF(uu33701plus20)
save_DVF(daqui)
save_DVF(caqui)


uu33701 <- NULL
iris15 <- load_DVF("iris15")
uu33701$iris <- iris15 %>% filter(UU2010=="33701") %>% st_union 
caqui <- load_DVF("caqui") %>% st_transform(3035)

uu33701$depsf <- daqui

uu33701$border <- caqui %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="33701") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu33701$bbox <- st_bbox(uu33701$border)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu33701$border) %>% st_crop(uu33701$border)

uu33701$fdc <- tm_shape(uu33701$border, bbox = uu33701$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu33701$hdc <- tm_shape(daqui, bbox = uu33701$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu33701$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu33701)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
aquiplus <- iris15 %>% filter(UU2010=="33701") %>% st_buffer(30000) %>% st_union %>% st_transform(4326)
st_crs(aquiplus) <- st_crs("+proj=longlat +ellps=WGS84")

aqui.mbr <- cc_location(loc=aquiplus, zoom = 9,
                          base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(aqui.mbr, max)
aqui.mbr <- projectRaster(from=aqui.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
aqui.mbr <- aqui.mbr/cellStats(aqui.mbr, max)*maxs %>% as.integer 

save_DVF(aqui.mbr)

# Mise à l'échelle du fond de carte

iris15 <- load_DVF("iris15")
uu33701 <- iris15 %>% filter(UU2010=="33701") %>% st_union # Lille
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu33701 <- uu33701 %>% st_centroid

# les limites appliquées à Lille sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lille
bb33701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu33701[[1]]), crs=3035)

aqui.mbfdc <- tm_shape(aqui.mbr, bbox = bb33701)+tm_rgb()

save_DVF(aqui.mbfdc)