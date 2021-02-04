source("dvf.r")

# lecture des carreaux 200 m pour faire le lissage kernel
# A comparer avec les sources iris et com

c200 <- st_read("{DVFdata}/sources/carreau 200/Filosofi2015_carreaux_200m_metropole.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(c200)<-2154
c200 %<>% st_transform(3035) 
xy <- c200 %>% st_centroid() %>% st_coordinates
idINS200 <- idINS3035(xy[,1], xy[,2], 200)
c200 %<>% select(-IdINSPIRE, -Id_carr1km, -I_est_cr, -Id_carr_n, -Id_car2010) %>% mutate(idINS200=idINS200, dep=str_sub(Depcom, 1,2))
save_DVF(c200, local=FALSE)
c200idf <- c200 %>% filter(dep %chin% depIdf)

g200 <- st_read("{DVFdata}/sources/carreau 200/grille200m_metropole.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(g200)<-2154

cidf <- load_DVF("cidf")
didf <- cidf %>% group_by(DEP) %>% summarize()

l_carreaux <- st_within(g200, st_union(didf))
ll<-map_lgl(l_carreaux, ~not(is_empty(.)))
lll<-which(ll)
g200idf <- g200 %>% slice(lll)

iris <- load_DVF("iris15")
iris.r <- select(iris, geometry, iris=CODE_IRIS, com=COM, dep=DEP)

g200idf <- st_join(g200idf, iris.r %>% st_transform(2154), largest=TRUE)

save_DVF(g200idf, local=FALSE)
save_DVF(c200idf, local=FALSE)
