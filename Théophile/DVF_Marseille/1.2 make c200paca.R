source("dvf.r")

# lecture des carreaux 200 m pour faire le lissage kernel
# A comparer avec les sources iris et com

depPaca <- c("83","84","04","13")
c200 <- st_read("{DVFdata}/carreau 200/Filosofi2015_carreaux_200m_metropole.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(c200)<-2154
c200 %<>% mutate(dep=str_sub(Depcom, 1,2))
c200Paca <- c200 %>% filter(dep %chin% depPaca)


# inutile?

g200 <- st_read("{DVFdata}/carreau 200/grille200m_metropole.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(g200)<-2154

cPaca <- load_DVF("cPaca")
dPaca <- cPaca %>% group_by(DEP) %>% summarize()

l_carreaux <- st_within(g200, st_union(d))
ll<-map_lgl(l_carreaux, ~not(is_empty(.)))
lll<-which(ll)
g200Paca <- g200 %>% slice(lll)

iris <- load_DVF("iris")
iris.r <- select(iris, geometry, iris=CODE_IRIS, com=INSEE_COM, dep=DEP)

g200Ra <- st_join(g200Ra, iris.r %>% st_transform(2154), largest=TRUE)

save_DVF(g200Ra)
# Fin inutile?

save_DVF(c200Paca)
