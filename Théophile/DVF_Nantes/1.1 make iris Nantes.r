source("dvf.r")

# On fabrique les fichier cidf et iidf.sf ---------------------------------------
depPdloire <- c("49","44","85")

# cartes des IRIS
ifr <- st_read("{DVFdata}/parcelles/fr/CONTOURS-IRIS.shp" %>% glue, stringsAsFactors=FALSE)
ifr <- st_transform(ifr, 2154)

ifr %<>% mutate(DEP =str_sub(ifr$INSEE_COM,1,2))
iPdloire <- ifr %>% filter( DEP %in% depPdloire) %>% st_make_valid %>% st_transform(3035)
iPdloire %<>% mutate(surface=st_area(iPdloire) %>% as.numeric)

#iris 15
iris15 <- setDT(readxl::read_excel(glue('{DVFdata}/population/base-ic-evol-struct-pop-2015.xls'), sheet="IRIS", skip = 5))
iris15 <- iris15[, .(IRIS, REG,DEP,UU2010,COM,LIBCOM,TRIRIS,GRD_QUART,LIBIRIS,TYP_IRIS,MODIF_IRIS,LAB_IRIS,
                     P15_POP,P15_POP_FR,P15_POP_ETR,P15_POP_IMM)]
setnames(iris15, "IRIS", "CODE_IRIS")

emp09.iris <- fread(glue("{DVFdata}/emploi/Emploi IRIS 2009.txt"))
setnames(emp09.iris, c("V1", "V2", "V3", "V4", "V5"), c("ID", "LIBIRIS", "CODE_IRIS", "EMP09", "CLAP"))
emp09.iris[, EMP09:=as.numeric(EMP09)]
emp09.iris[is.na(EMP09)|EMP09==-99999, EMP09:=0]

iris15 <- merge(iris15, emp09.iris[, .(CODE_IRIS, EMP09)], all.x=TRUE) %>% as_tibble
iris15 <- iris15 %>% mutate(EMP09=ifelse(is.na(EMP09), 0, EMP09))
iris15 <- left_join(iris15,ifr %>% as_tibble %>% select(CODE_IRIS, geometry), by="CODE_IRIS")
iris15 <- iris15 %>% st_as_sf %>% st_transform(3035)
save_DVF(iris15)

iris <- left_join(iris, iris15 %>% select(CODE_IRIS, P15_POP, P15_POP_FR, P15_POP_ETR, P15_POP_IMM, EMP09), by="CODE_IRIS")


save_DVF(iPdloire)
save_DVF(iris)
