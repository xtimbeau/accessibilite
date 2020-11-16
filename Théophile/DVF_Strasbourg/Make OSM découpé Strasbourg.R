source("access.r")

iris15 <- load_DVF("iris15") %>% st_transform(3035)
als <- iris15 %>% filter(UU2010=="67701") %>% st_buffer(20000) %>% st_union # le fichier des iris, avec les emplois qui sert aux limites
iris_als <- iris15 %>% filter(st_within(iris15,als, sparse=FALSE)) # les limites rectangulaires
als_box <- st_bbox(als %>% st_transform(4326)) # en coordonnées GPS WG84
infile <- "france-latest.osm.pbf" %>% glue() # la carte complète
outfile <- "alsace.osm.pbf" %>% glue # le fichier de sortie (ici l'idf)
# les commandes osmosis.bat -- attention il faut installer osmosis
unfiltered <- str_c("osmosis.bat --read-pbf {infile}",
                    "--bounding-box left={als_box$xmin} bottom={als_box$ymin} right={als_box$xmax} top={als_box$ymax}",
                    "--write-pbf {outfile}", sep=" ") %>% glue
# appelle les commandes osmosis
current.wd <- getwd()
setwd("{DVFdata}/fdCartes/OSM" %>% glue)
system2("powershell.exe", args = unfiltered)
setwd(current.wd)