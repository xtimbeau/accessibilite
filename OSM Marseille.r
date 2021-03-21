source("init.r")

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
uu759 <- iris15 %>% filter(UU2010=="00759") %>% st_union()
uu759plus20 <- uu759 %>% st_buffer(20000)

box <- uu759plus20 %>% st_transform(4326) %>% st_bbox()

infile <- "france-latest.osm.pbf" %>% glue() # la carte complète
outfile <- "marseille.osm.pbf" %>% glue # le fichier de sortie (ici l'idf)
# les commandes osmosis.bat -- attention il faut installer osmosis
unfiltered <- str_c("osmosis.bat --read-pbf {infile}",
                    "--bounding-box left={box$xmin} bottom={box$ymin} right={box$xmax} top={box$ymax}",
                    "--write-pbf {outfile}", sep=" ") %>% glue
# appelle les commandes osmosis
current.wd <- getwd()
setwd("{DVFdata}/sources/fdCartes/OSM" %>% glue)
system2("powershell.exe", args = unfiltered)
setwd(current.wd)

c200_759 <- c200 %>% filter(st_within(., uu759, sparse=FALSE))

rmax <- sqrt(as.numeric(st_area(uu759))/pi)
paca <- map_dfr(seq(-rmax,50000, 1000),
               ~{
                 uu759plus <- uu759 %>% st_buffer(.x)
                 iris <- iris15 %>% filter(st_within(., uu759plus, sparse=FALSE)) %>% st_drop_geometry()
                 iris <- iris %>% summarize(emp=sum(EMP09), pop=sum(P15_POP)) %>% mutate(dist = .x)
               }) %>%
  arrange(dist) %>%
  mutate(demp = emp - lag(emp),
         dpop = pop-lag(pop)) %>%
  pivot_longer(cols = c(emp, pop, demp, dpop), names_to="var", values_to="p")

ggplot(paca %>% filter(var!="pop",var!="emp"), aes(x=dist, y=p, group=var, col=var))+ geom_line()

# uu758 -> Les opportunités de l'aire urbaine
# uu758plus20 -> Les opportunités de l'aire urbaine étendue
# c200_758 -> Le carroyage de l'aire urbaine stricte