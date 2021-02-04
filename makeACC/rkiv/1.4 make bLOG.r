source('dvf.r')

readLOG <- function(annee, rep, skip=5) {
  print(str_c("Lecture de base-ic-logement-20", annee, ".xls"))
  bLOG <- setDT(readxl::read_excel(str_c(rep, "/base-ic-logement-20", annee, ".xls" ), sheet="IRIS", skip = skip))
  invars <- c("tmob","dvac","ploc","phlm","nblog", "nbrp")
  rinvars <- str_c(invars,annee)
  exvars <- c("MEN_ANEM0002","MEN","LOGVAC","RP_LOC","RP_LOCHLMV","LOG","RP")
  rexvars <- map_chr(exvars, function(var) str_c("P",annee,"_", var))
  setnames(bLOG, rexvars, exvars)
  bLOG <- bLOG[, .(iris = IRIS,
                   nblog = LOG,
                   nbrp = RP,
                   tmob = MEN_ANEM0002/MEN,
                   dvac = LOGVAC/LOG,
                   ploc = RP_LOC/RP,
                   phlm = RP_LOCHLMV/RP)]
  setnames(bLOG, invars, rinvars)
  return(bLOG)            
}

bLOGlist <- lapply(c("07","08","09","10", "11", "12", "13", "14", "15"), function(x,y) readLOG(x,"{DVFdata}/logement" %>% glue))
bLOG <- reduce(bLOGlist, function(x,y) merge(x,y, by="iris"))
rm(bLOGlist)

save_DVF(bLOG)
