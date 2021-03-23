source("access.r")

iris15 <- load_DVF("iris15") %>% st_transform(3035)
als <- iris15 %>% filter(UU2010=="17601") %>% st_buffer(20000) %>% st_union 
iris_als <- iris15 %>% filter(st_within(iris15,als, sparse=FALSE))
als_box <- st_bbox(als %>% st_transform(4326))

infile <- "france-latest.osm.pbf" %>% glue() 
outfile <- "larochelle.osm.pbf" %>% glue 

unfiltered <- str_c("osmosis.bat --read-pbf {infile}",
                    "--bounding-box left={als_box$xmin} bottom={als_box$ymin} right={als_box$xmax} top={als_box$ymax}",
                    "--write-pbf {outfile}", sep=" ") %>% glue
                    
current.wd <- getwd()
setwd("{DVFdata}/fdCartes/OSM" %>% glue)
system2("powershell.exe", args = unfiltered)
setwd(current.wd)





