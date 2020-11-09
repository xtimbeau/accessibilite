source("dvf.r")

# lecture des carreaux 200 m pour faire le lissage kernel
# A comparer avec les sources iris et com

depLgdrous <- c("82","81","32","31")
c200 <- st_read("{DVFdata}/carreau 200/Filosofi2015_carreaux_200m_metropole.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(c200)<-2154
c200 %<>% mutate(dep=str_sub(Depcom, 1,2))
c200Lgdrous <- c200 %>% filter(dep %chin% depLgdrous)

save_DVF(c200Lgdrous)
