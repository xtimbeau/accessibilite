source("access.r")
uu851 <- NULL
iris15 <- load_DVF("iris15")
uu851$iris <- iris15 %>% filter(UU2010=="00851") %>% st_union 

cidf <- load_DVF("cidf") %>% st_transform(3035)
didf <- cidf %>% group_by(DEP) %>% summarize()
uu851$depsf <- didf

uu851$border <- cidf %>%
  filter(insee %chin%
           (iris15 %>%
              filter(UU2010=="00851") %>%
              pull(COM) %>%
              unique)) %>%
  st_union

uu851$bbox <- st_bbox(uu851$iris)

riv <- st_read("{DVFdata}/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu851$border) %>% st_crop(uu851$border)

uu851$fdc <- tm_shape(uu851$border, bbox = uu851$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray90", alpha = 0.5)

uu851$hdc <- tm_shape(didf, bbox = uu851$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_shape(riv, bbox=uu851$bbox ) + tm_fill("dodgerblue", aplha=1)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "theophilegervais"
mapbox_key <- "pk.eyJ1IjoidGhlb3BoaWxlZ2VydmFpcyIsImEiOiJja2gwZjY0N2YweGU0MnFudml6YmNoM2l4In0.jjY7QwYgIgAB7xHGHrT3ig"
style_id <- "ckh3em89k2lf919nkb0joxe70" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
uuplus <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(uuplus) <- st_crs("+proj=longlat +ellps=WGS84") 

mbr <- cc_location(loc=uuplus, zoom = 9,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

maxs <- cellStats(mbr, max)
mbr <- projectRaster(from=mbr, crs=CRS("EPSG:3035")) # la projection fait un truc bizarre sur les entiers
mbr <- mbr/cellStats(mbr, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035

# les limites appliquées à Lyon sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lyon

uu851$mbfdc <- tm_shape(mbr, bbox = uu851$bbox)+tm_rgb()

save_DVF(uu851)
