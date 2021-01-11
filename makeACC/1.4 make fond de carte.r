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

riv <- st_read("{DVFdata}/sources/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035)
riv <- riv %>% filter(Type=="Cours d'eau") %>% st_filter(uu851$border) %>% st_crop(uu851$border)

uu851$fdc <- tm_shape(uu851$border, bbox = uu851$bbox ) +
  tm_borders(lwd = 0.25) +
  tm_fill(col = "gray97", alpha = 0.1)

uu851$hdc <- tm_shape(didf, bbox = uu851$bbox ) +
  tm_borders(lwd = 0.25, col = "gray50") +
  tm_scale_bar(lwd=0.5, position=c("left", "bottom"), width=0.1) +
  tm_compass(type="arrow")

uu851$riv <- tm_shape(riv, bbox=uu851$bbox ) + 
  tm_fill("dodgerblue", aplha=1)

# fonds de cartes de Mapbox------------------

library(ceramic)

username <- "xtimbeau"
mapbox_key <- "pk.eyJ1IjoieHRpbWJlYXUiLCJhIjoiY2tnMHhiNnAwMGJyaTJzcXdqbXU1c3Y0MiJ9.ydGev8EOzUGtIUHeLlZqtQ"
style_id <- "ckg2d9ypq0lzv19m8p6a7xqoi" # défini sur mon compte MapBox
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

iris15 <- load_DVF("iris15")
uuplus <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union %>% st_transform(4326)
st_crs(uuplus) <- st_crs("+proj=longlat +ellps=WGS84") 

mbrz11 <- cc_location(loc=uuplus, max_tiles=80,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

mbrz11 <- projectrgb(mbrz11)
mbrz10 <- aggregate(mbrz11, 4)
# les limites appliquées à Lyon sont la boite de l'aire urbaine de Paris translatée sur le centre de l'aire urbaine de Lyon

uu851$mbz11 <- tm_shape(mbrz11, bbox = uu851$bbox)+tm_rgb()
uu851$mbz10 <- tm_shape(mbrz10, bbox = uu851$bbox)+tm_rgb()

save_DVF(uu851, local=FALSE)
