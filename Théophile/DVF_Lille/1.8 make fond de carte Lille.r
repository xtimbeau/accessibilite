source("access.r")

cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))

iris15 <- load_DVF("iris15")
c200 <- load_DVF("c200")

uu59702 <- iris15 %>% filter(UU2010=="59702") %>% st_union()
c200_59702 <- c200 %>% filter(st_within(., uu59702, sparse=FALSE))
c200_59702 %<>% st_transform(3035) %>% st_as_sf
uu59702plus20 <- uu59702 %>% st_buffer(20000)
c200_nrdcal <- c200_59702 %>% filter(st_within(., uu59702plus20, sparse=FALSE))

cnrdcal <- cfr %>% filter(DEP %in% c200_nrdcal)
dnrdcal <- cnrdcal %>% group_by(DEP) %>% summarize()

save_DVF(uu59702plus20)
save_DVF(dnrdcal)
save_DVF(cnrdcal)

uu59702 <- NULL
uu59702$iris <- iris15 %>% filter(UU2010=="59702") %>% st_union 
cnrdcal <- load_DVF("cnrdcal") %>% st_transform(3035)

dnrdcal <- cnrdcal %>% group_by(DEP) %>% summarize()

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
  tm_shape(riv, bbox=uu758$bbox ) + tm_fill("dodgerblue", aplha=1)

save_DVF(uu59702)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
nrdcalplus <- iris15 %>% filter(UU2010=="59702") %>% st_buffer(40000) %>% st_union %>% st_transform(4326)
st_crs(nrdcalplus) <- st_crs("+proj=longlat +ellps=WGS84") 

nrdcal.mbr <- cc_location(loc=nrdcalplus, zoom = 9,
                          base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(nrdcal.mbr, max)
nrdcal.mbr <- projectRaster(from=nrdcal.mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
nrdcal.mbr <- nrdcal.mbr/cellStats(nrdcal.mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035

save_DVF(nrdcal.mbr)

# Mise à l'échelle du fond de carte par rapport à l'aire urbaine la plus grande: Paris

iris15 <- load_DVF("iris15")
uu59702 <- iris15 %>% filter(UU2010=="59702") %>% st_union # Lyon
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu59702 <- uu59702 %>% st_centroid

# les limites appliquées à Lyon sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lyon
bb59702 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu59702[[1]]), crs=3035)

nrdcal.mbfdc <- tm_shape(nrdcal.mbr, bbox = bb59702)+tm_rgb()

save_DVF(nrdcal.mbfdc)