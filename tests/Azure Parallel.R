source("access.r")
# devtools::install_github("Azure/rAzureBatch")
# devtools::install_github("Azure/doAzureParallel")
# devtools::install_github("Azure/AzureStor")

library(doAzureParallel)
library(AzureStor)
library(rAzureBatch)
library(foreach)
library("magrittr")
library("sf")

source("fonctions/f.accessibilite.r")
source("fonctions/f.communs.r")
source("fonctions/f.map utils.r")

c200 <- qs::qread("G:/Mon Drive/DVFdata/rda/c200.rda") %>% sf::st_transform(3035)
iris15 <- qs::qread("G:/Mon Drive/DVFdata/rda/iris15.rda")
idfplus20 <- iris15 %>% dplyr::filter(UU2010=="00851") %>% sf::st_buffer(20000) %>% sf::st_union()
uu851 <- iris15 %>% dplyr::filter(UU2010=="00851") %>% sf::st_union()
iris15_idf <- iris15 %>% dplyr::filter(sf::st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% dplyr::filter(sf::st_within(., uu851, sparse=FALSE)) %>% dplyr::select(idINS200, Ind, Depcom, dep)

# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% dplyr::transmute(EMP09, P15_POP, cste=1) %>% sf::st_centroid()
total_opp <- opp %>% sf::st_drop_geometry() %>% dplyr::summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

res <- 200
com <- c("75101", "75102", "75103")
dep <- c("75", "91", "92", "93")
pckgs44each <- c("logger", "data.table", "dplyr", "glue", "magrittr", "stringr", "progressr", "r5r", "purrr", "future", "furrr", "lubridate", "rlist", "rJava",
                 "sf", "raster", "fasterize", "matrixStats", "tictoc", "rlang")  

test_az <- foreach(i = 1:2, .packages = pckgs44each) %do% {
  DVFdata <- "."
  options(java.parameters = "-Xmx24G" )
  tr_r5 <- routing_setup_r5(
    path="r5r_data/IDFM", 
    mode=c("WALK", "TRANSIT"),
    time_window=60,
    montecarlo = 100, 
    percentiles = 5L,
    n_threads = 2)
  rr <- iso_accessibilite(
    quoi=opp,            
    ou=c200_idf %>% filter(Depcom==com[i]),          
    resolution=200,      
    tmax=60,            
    pdt=5,               
    routing=tr_r5)
  rr
}

# mit AZURE --------------------
setCredentials("azure/credentials.json")
cluster <- makeCluster("azure/cluster.json")
# "mkdir /mnt/batch/tasks/shared/fileshare",
# "mount -t cifs //totostor.file.core.windows.net/timbsmb /mnt/batch/tasks/shared/fileshare -o vers=3.0 username=totostor,password=xzGGGBX0kthUxzz78zMUNZfHjhWr5L8FlvRpu3xfzT06KHlAKC78+fjY8/3Mp2aE+//yaOz8Vy5MKyKqZfFBZw==, dir_mode=0777, file_mode=0777, sec=ntlmssp"
registerDoAzureParallel(cluster)

xy <- c200_idf %>%  st_centroid() %>% st_coordinates()
morceaux <- c200_idf %>% dplyr::mutate(idINS3200 = idINS3035(xy, resolution = 12800))%>% dplyr::group_by(idINS3200) %>% dplyr::group_split()

# IDFM
bl_endp_key <- storage_endpoint("https://totostor.file.core.windows.net", 
                                key="xzGGGBX0kthUxzz78zMUNZfHjhWr5L8FlvRpu3xfzT06KHlAKC78+fjY8/3Mp2aE+//yaOz8Vy5MKyKqZfFBZw==")
jeton_sas <- "?sv=2019-12-12&ss=bfqt&srt=sco&sp=rl&se=2020-12-31T18:28:15Z&st=2020-11-16T10:28:15Z&spr=https&sig=QcFK2rI9U91Eyabd7keZelEBNjoOzxiroWyrI%2BUkeh4%3D"
file_url <- "https://totostor.file.core.windows.net/?sv=2019-12-12&ss=bfqt&srt=sco&sp=rl&se=2020-12-31T18:28:15Z&st=2020-11-16T10:28:15Z&spr=https&sig=QcFK2rI9U91Eyabd7keZelEBNjoOzxiroWyrI%2BUkeh4%3D"
fl_endp_sas <- storage_endpoint("https://totostor.file.core.windows.net", sas=jeton_sas)


conts <- list_storage_containers(fl_endp_sas)
cont <- storage_container(fl_endp_sas, "timbsmb")

test_az <- foreach(i = iter(morceaux[1:2]), .packages = pckgs44each) %dopar% {
  DVFdata <- "."
  options(java.parameters = "-Xmx16G" )
  library(rJava)
  library(r5r)
  getr5datafromAzFS(jeton_sas = jeton_sas, path="IDFM")
  tr_r5 <- routing_setup_r5(
    path="IDFM", 
    mode=c("WALK", "TRANSIT"),
    time_window=60,
    montecarlo = 100, 
    percentiles = 5L,
    n_threads = 2)
  rr <- iso_accessibilite(
    quoi=opp,
    ou= i,
    resolution=200,
    tmax=60,
    pdt=5,
    routing=tr_r5)
  rr
}

files <- foreach(i = 1:1) %dopar% {
  DVFdata <- "."
  options(java.parameters = "-Xmx16G" )
  library(rJava)
  library(r5r)
  getr5datafromAzFS(jeton_sas = jeton_sas, path="IDFM")
  sys = Sys.getenv("AZ_BATCH_TASK_WORKING_DIR")
  dir = list.files(sys, recursive=FALSE)
  root = list.files(sys, recursive=TRUE)
  list(
    sys = sys,
    dir=dir,
    root = root,
    si = sessionInfo())
  }

session <- foreach(i = 1) %dopar% {
  library(rJava)
  library(r5r)
  download_r5()
  sessionInfo()
}

stopCluster(cluster)


