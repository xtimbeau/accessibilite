source("dvf.r")
library(RCurl)
library(rjson)
library(jpeg)
library(png)
library(magick)
library(ceramic)

username <- "xtimbeau"
mapbox_key <- "pk.eyJ1IjoieHRpbWJlYXUiLCJhIjoiY2tnMHhiNnAwMGJyaTJzcXdqbXU1c3Y0MiJ9.ydGev8EOzUGtIUHeLlZqtQ"
style_id <- "ckg2d9ypq0lzv19m8p6a7xqoi"
Sys.setenv(MAPBOX_API_KEY=mapbox_key)

iris15 <- load_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union %>% st_transform(4326)


a <- cc_location(loc=idfplus20, zoom = 9,
            base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")

ttr <- load_DVF("ttr_emp09")
tm_shape(a)+tm_rgb()+tm_shape(ttr$emp10)+tm_raster(alpha=0.5, style="cont", palette=heatrg)


lon_lat <- idfplus20 %>% st_centroid %>% st_coordinates
zoom <- 10.5
api_call <- str_glue(
    "https://api.mapbox.com/styles/v1/{username}/{style_id}/static/",
    "{lon_lat[,1]},{lon_lat[,2]},{zoom}/{1200}x{900}?access_token={mapbox_key}")

getURLContent(api_call) %>% image_read

