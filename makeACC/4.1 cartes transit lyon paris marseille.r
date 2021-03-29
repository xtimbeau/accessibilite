source('init.r')
c200 <- load_DVF('c200')
iris15 <- load_DVF("iris15")
uu758 <- iris15 %>% filter(UU2010=="00758") %>% st_union # Lyon
uu759 <- iris15 %>% filter(UU2010=="00759") %>% st_union # Lyon
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union # Paris
c_uu851 <- uu851 %>% st_centroid
c_uu758 <- uu758 %>% st_centroid
c_uu759 <- uu759 %>% st_centroid
bb758 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu758[[1]]), crs=3035)
bb759 <- st_bbox((uu851[[1]]-c_uu851[[1]]+c_uu759[[1]]), crs=3035)
bb851 <- st_bbox((uu851[[1]]), crs=3035)

library(ceramic)
username <- Sys.getenv("mapboxusername")
mapbox_key <- Sys.getenv("mapboxkey")
style_id <- "ckjka0noe1eg819qrhuu1vigs"
Sys.setenv(MAPBOX_API_KEY= mapbox_key)

bb_paris <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(50000) %>% st_transform(4326) 
st_crs(bb_paris) <- st_crs("+proj=longlat +ellps=WGS84") 
paris.mb <- cc_location(loc=bb_paris, zoom = 9, base_url = "https://api.mapbox.com/styles/v1/xtimbeau/{style_id}/tiles/512/{zoom}/{x}/{y}")
maxs <- cellStats(paris.mb, max)
paris.mb <- projectRaster(from=paris.mb, crs=st_crs(3035)$proj4string) # la projection fait un truc bizarre sur les entiers
paris.mb <- paris.mb/cellStats(paris.mb, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035
iso_paris <- lload_DVF("tr_r550_2020")
paris <- iso2time(iso_paris$EMP09, seuils=c(50000, 250000))
p <- tm_shape(paris.mb, bbox = bb851)+tm_rgb()+tm_shape(paris)+tm_raster(style="cont", palette=terrain.colors(20, rev=FALSE), breaks=c(20,40,60,80,100))

bb_lyon <- iris15 %>% filter(UU2010=="00758") %>% st_buffer(75000) %>% st_union() %>% st_transform(4326)
st_crs(bb_lyon) <- st_crs("+proj=longlat +ellps=WGS84")
lyon.mb <- cc_location(loc=bb_lyon, zoom = 9,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")
maxs <- cellStats(lyon.mb, max)
lyon.mb <- projectRaster(from=lyon.mb, crs=st_crs(3035)$proj4string) # la projection fait un truc bizarre sur les entiers
lyon.mb <- lyon.mb/cellStats(lyon.mb, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035
iso_lyon <- lload_DVF("iso_transit_50_r5_Lyon")
lyon <- iso2time(iso_lyon$EMP09, seuils=c(50000, 250000))
l <- tm_shape(lyon.mb, bbox = bb758)+tm_rgb()+tm_shape(lyon)+tm_raster(style="cont", palette=terrain.colors(20, rev=FALSE), breaks=c(20,40,60,80,100))

bb_marseille <- iris15 %>% filter(UU2010=="00759") %>% st_buffer(50000) %>% st_union() %>% st_transform(4326)
st_crs(bb_marseille) <- st_crs("+proj=longlat +ellps=WGS84")
marseille.mb <- cc_location(loc=bb_marseille, zoom = 9,
                       base_url = "https://api.mapbox.com/styles/v1/{username}/{style_id}/tiles/512/{zoom}/{x}/{y}")
maxs <- cellStats(marseille.mb, max)
marseille.mb <- projectRaster(from=marseille.mb, crs=st_crs(3035)$proj4string) # la projection fait un truc bizarre sur les entiers
marseille.mb <- marseille.mb/cellStats(marseille.mb, max)*maxs %>% as.integer # on remet tout comme avant mais en 3035
iso_marseille <- lload_DVF("iso_transit_50_r5_Marseille")
marseille <- iso2time(iso_marseille$EMP09, seuils=c(50000, 250000))
m <- tm_shape(marseille.mb, bbox = bb759)+tm_rgb()+tm_shape(marseille)+tm_raster(style="cont", palette=terrain.colors(20, rev=FALSE), breaks=c(20,40,60,80,100))

mmm <- tmap_arrange(p, l , m, nrow=3)
tmap_save(tm=mmm, "{DVFdata}/presentation/vv/plm transit.svg" %>% glue)
