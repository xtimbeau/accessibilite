source("access.r")

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

iris15 <- load_DVF("iris15")
c200 <- load_DVF("c200")

uu06701 <- iris15 %>% filter(UU2010=="06701") %>% st_union()
c200_06701 <- c200 %>% filter(st_within(., uu06701, sparse=FALSE))
c200_06701 %<>% st_transform(3035) %>% st_as_sf
uu06701plus20 <- uu06701 %>% st_buffer(20000)
c200_paca <- c200_06701 %>% filter(st_within(., uu06701plus20, sparse=FALSE))

cpaca_nice <- cfr %>% filter(DEP %in% c200_paca)
dpaca_nice <- cpaca_nice %>% group_by(DEP) %>% summarize()

save_DVF(uu06701plus20)
save_DVF(dpaca_nice)
save_DVF(cpaca_nice)

uu06701 <- NULL
uu06701$iris <- iris15 %>% filter(UU2010=="06701") %>% st_union 
cpaca_nice <- load_DVF("cpaca_nice") %>% st_transform(3035)

dpaca_nice <- cpaca_nice %>% group_by(DEP) %>% summarize()

uu06701$depsf <- dpaca_nice

uu06701$border <- cpaca_nice %>%
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

uu06701$hdc <- tm_shape(dpaca_nice, bbox = uu06701$bbox ) +
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
pacaplus <- iris15 %>% filter(UU2010=="06701") %>% st_buffer(30000) %>% st_union %>% st_transform(4326)
st_crs(pacaplus) <- st_crs("+proj=longlat +ellps=WGS84") 

paca_nice.mbr <- cc_location(loc=pacaplus, zoom = 9,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(paca_nice.mbr, max)
paca_nice.mbr <- projectRaster(from=paca_nice.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
paca_nice.mbr <- paca_nice.mbr/cellStats(paca_nice.mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035

save_DVF(paca_nice.mbr)

# Mise à l'échelle du fond de carte par rapport à l'aire urbaine la plus grande: Paris

iris15 <- load_DVF("iris15")
uu06701 <- iris15 %>% filter(UU2010=="06701") %>% st_union # Lyon
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu06701 <- uu06701 %>% st_centroid

# les limites appliquées à Lyon sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lyon
bb06701 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu06701[[1]]), crs=3035)

paca_nice.mbfdc <- tm_shape(paca_nice.mbr, bbox = bb06701)+tm_rgb()

save_DVF(paca_nice.mbfdc)