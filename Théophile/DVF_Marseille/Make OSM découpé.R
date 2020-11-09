source("dvf.r")

iris15 <- load_DVF("iris15") %>% st_transform(3035)
idf <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union # le fichier des iris, avec les emplois qui sert aux limites
iris_idf <- iris15 %>% filter(st_within(iris15,idf, sparse=FALSE)) # les limites rectangulaires
idf_box <- st_bbox(idf %>% st_transform(4326)) # en coordonnées GPS WG84
infile <- "france-latest.osm.pbf" %>% glue() # la carte complète
outfile <- "idf.osm.pbf" %>% glue # le fichier de sortie (ici l'idf)
# les commandes osmosis.bat -- attention il faut installer osmosis
unfiltered <- str_c("osmosis.bat --read-pbf {infile}",
                    "--bounding-box left={idf_box$xmin} bottom={idf_box$ymin} right={idf_box$xmax} top={idf_box$ymax}",
                    "--write-pbf {outfile}", sep=" ") %>% glue
# appelle les commandes osmosis
current.wd <- getwd()
setwd("{DVFdata}/fdCartes/OSM" %>% glue)
system2("powershell.exe", args = unfiltered)
setwd(current.wd)