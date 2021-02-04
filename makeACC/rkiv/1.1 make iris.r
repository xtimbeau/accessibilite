source("dvf.r")

# On fabrique les fichier cidf et iidf.sf ---------------------------------------
depIdf <- c("75","77","78","91","92","93","94","95")

zGP <- st_read("{DVFdata}/zonage/PLU_ZONAGE.shp" %>% glue) %>% st_transform(2154)

# cartes des IRIS
ifr <- st_read("{DVFdata}/parcelles/fr/CONTOURS-IRIS.shp" %>% glue, stringsAsFactors=FALSE)
ifr <- st_transform(ifr, 2154)

ifr %<>% mutate(DEP =str_sub(ifr$INSEE_COM,1,2))
iidf <- ifr %>% filter( DEP %in% depIdf) %>% st_make_valid %>% st_transform(3035)
iidf %<>% mutate(surface=st_area(iidf) %>% as.numeric)

# données aggrégées par IRIS (y compris les prix moyens et m?dian par iris avec observation calculés dans excel)  
iris_data <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/dvfe(irisidf).csv" %>% glue, sep=";", stringsAsFactors = FALSE) %>% 
  as_tibble()
iris_data$iris <- as.character(iris_data$iris)
iris <- iidf %>% left_join(dplyr::select(iris_data, 
                                         iris, REG, UU2010, COM, LIBCOM, TRIRIS, GRD_QUART, LIBIRIS, P14_POP), 
                           by =c("CODE_IRIS"= "iris"))

# données calcul?es sur les distributions de revenu par IRIS (fichiers irsi14dec, iris06dec, iris01dec)
iris14dec <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/iris14dec.csv" %>% glue, sep=",", stringsAsFactors = FALSE)
iris14dec$IRIS <- as.character(iris14dec$IRIS)
iris14dec$pRD914 <- as.numeric(iris14dec$pRD914)
iris14dec$pRD114 <- as.numeric(iris14dec$pRD114)
iris14dec$txpauv14 <- as.numeric(iris14dec$txpauv14)
iris <- iris %>% merge(dplyr::select(iris14dec %>% as_tibble, IRIS, pRD914, pRD114, txpauv14), 
                       by.x = "CODE_IRIS", by.y = "IRIS", all.x=TRUE)

iris06dec <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/iris06dec.csv" %>% glue, sep=",", stringsAsFactors = FALSE)
iris06dec$IRIS <- as.character(iris06dec$IRIS)
iris06dec$pRD906 <- as.numeric(iris06dec$pRD906)
iris06dec$pRD106 <- as.numeric(iris06dec$pRD106)
iris06dec$txpauv06 <- as.numeric(iris06dec$txpauv06)
iris <- iris %>% merge(dplyr::select(iris06dec %>% as_tibble, IRIS, pRD906, pRD106, txpauv06),
                       by.x = "CODE_IRIS", by.y = "IRIS", all.x=TRUE)

iris01dec <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/iris01dec.csv" %>% glue, sep=",", stringsAsFactors = FALSE)
iris01dec$IRIS <- as.character(iris01dec$IRIS)
iris01dec$pRD901 <- as.numeric(iris01dec$pRD901)
iris01dec$pRD101 <- as.numeric(iris01dec$pRD101)
iris01dec$txpauv01 <- as.numeric(iris01dec$txpauv01)
iris <- iris %>% merge(dplyr::select(iris01dec %>% as_tibble, IRIS, pRD901, pRD101, txpauv01),
                       by.x = "CODE_IRIS", by.y = "IRIS", all.x=TRUE)

# ajoutre les données logement
iris14log <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/iris14log.csv" %>% glue, sep=",", stringsAsFactors = FALSE)
iris14log$IRIS <- as.character(iris14log$IRIS)
iris <- iris %>% merge(dplyr::select(iris14log, IRIS, P14_LOG, P14_RP, P14_LOGVAC), by.x = "CODE_IRIS", by.y = "IRIS", all.x=TRUE)
iris06log <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/iris06log.csv" %>% glue, sep=",", stringsAsFactors = FALSE)
iris06log$IRIS <- as.character(iris06log$IRIS)
iris <- iris %>% merge(dplyr::select(iris06log, IRIS, P06_LOG, P06_RP, P06_LOGVAC),
                       by.x = "CODE_IRIS", by.y = "IRIS", all.x=TRUE)
iris$s_iris <- st_area(iris) #calcul de surface (on peut mieux faire sans doute, en utilisant plut?t la surface utile)

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

emp_iris <- left_join(iris15 %>% as_tibble, ifr %>% select(CODE_IRIS), by="CODE_IRIS") %>% st_as_sf
emp_iris_idf <-emp_iris %>% st_filter(emp_iris %>% select(DEP) %>% filter(DEP%in%depIdf) %>% st_union %>% st_buffer(dist = 10000)) 

# cartes des communes&arrondissements municipaux
cfr <- st_read("{DVFdata}/communes/communes.shp" %>% glue, stringsAsFactors=FALSE)
st_crs(cfr) <- 2154 #projection lambert 93 standard
cfr <- st_transform(cfr, 2154)
names(cfr)[1]<-"insee"
cfr %<>% mutate(DEP = str_sub(cfr$insee,1,2))
cidf <- cfr %>% filter( DEP %in% depIdf)
pnl <- fread("{DVFdata}/Etude_IDF/JRC project/DVF/DVF.txt/data.csv/pnl.csv" %>% glue, sep=",", stringsAsFactors = FALSE)
asnums <- c("LOYERS","P5ANS","P1AN","P1AN_Notaires","NOTAIRESPrix","NOTAIRES5ans","PAPP_t",
            "P5ANS_Notaires","ZoneloyerOLAP","LoyersOLAP","PN196","P1A196","P5196")
pnl[, (asnums):=lapply(.SD, as.numeric), .SDcols=asnums]
pnl$INSEE <- as.character(pnl$INSEE)

cidf <-  merge(cidf, dplyr::select(pnl %>% as_tibble, 
                                   INSEE,PAPP,PAPP_t, PN196, P1A196, P5196,
                                   LOYERS,LoyersOLAP,P5ANS_Notaires,P1AN_Notaires,P5ANS,P1AN),
               by.x = "insee", by.y = "INSEE", all.x=TRUE)

save_DVF(cidf)
save_DVF(iidf)
save_DVF(iris)
save_DVF(zGP)
save_DVF(emp_iris)
save_DVF(emp_iris_idf)