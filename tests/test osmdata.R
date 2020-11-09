source("dvf.r")

library(osmdata)

iris15 <- load_DVF("iris15")

idf <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union
iris_idf <- iris15 %>% filter(st_within(iris15,idf, sparse=FALSE))
idf_box <- st_bbox(idf %>% st_transform(4326))

cafe <- opq(bbox = idf_box, timeout=10000, memsize=1024*1024*1024) %>% 
  add_osm_feature(key = 'amenity', value = 'cafe') %>% 
  osmdata_sf()
restaurant <- opq(bbox = idf_box, timeout=10000, memsize=1024*1024*1024) %>% 
  add_osm_feature(key = 'amenity', value = 'restaurant') %>% 
  osmdata_sf()

  
tm_shape(bind_rows(cafe$osm_points, restaurant$osm_points))+tm_dots(col="amenity")


infile <- "france-latest.osm.pbf" %>% glue()
outfile <- "idf.osm.pbf" %>% glue
filtered_idf <- str_c("osmosis.bat --read-pbf {infile}",
                "--bounding-box left={idf_box$xmin} bottom={idf_box$ymin} right={idf_box$xmax} top={idf_box$ymax}",
                "--tf accept-ways highway=* public_transport=platform railway=platform park_ride=*",
                "--tf accept-relations type=restriction --used-node --write-pbf {outfile}" , sep=" ") %>% glue
unfiltered_idf <- str_c("osmosis.bat --read-pbf {infile}",
                    "--bounding-box left={idf_box$xmin} bottom={idf_box$ymin} right={idf_box$xmax} top={idf_box$ymax}",
                    "--write-pbf {outfile}", sep=" ") %>% glue
current.wd <- getwd()
setwd("{DVFdata}/fdCartes/OSM" %>% glue)
system2("powershell.exe", args = unfiltered_idf)
setwd(current.wd)
