source("access.r")
# devtools::install_github("Azure/rAzureBatch")
# devtools::install_github("Azure/doAzureParallel")
# devtools::install_github("Azure/AzureStor")

library(doAzureParallel)
library(AzureStor)
library(rAzureBatch)
library(foreach)

c200 <- qs::qread("G:/Mon Drive/DVFdata/rda/c200.rda") %>% sf::st_transform(3035)
iris15 <- qs::qread("G:/Mon Drive/DVFdata/rda/iris15.rda")
idfplus20 <- iris15 %>% dplyr::filter(UU2010=="00851") %>% sf::st_buffer(20000) %>% sf::st_union()
uu851 <- iris15 %>% dplyr::filter(UU2010=="00851") %>% sf::st_union()
iris15_idf <- iris15 %>% dplyr::filter(sf::st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% dplyr::filter(sf::st_within(., uu851, sparse=FALSE)) %>% dplyr::select(idINS200, Ind, Depcom, dep)
rm(c200)
# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% dplyr::transmute(EMP09, P15_POP, cste=1) %>% sf::st_centroid()
total_opp <- opp %>% sf::st_drop_geometry() %>% dplyr::summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

res <- 200
pckgs44each <- c("logger", "data.table", "dplyr", "glue", "magrittr", "stringr", "progressr", "purrr", "future", "furrr", "lubridate", "rlist", 
                 "sf", "raster", "fasterize", "matrixStats", "tictoc", "rlang")  
xy <- c200_idf %>%  st_centroid() %>% st_coordinates()
morceaux <- c200_idf %>% dplyr::mutate(idINSx = idINS3035(xy, resolution = 12800))%>% dplyr::group_by(idINSx) %>% dplyr::group_split()

# démarrage d'Azure

setCredentials("azure/credentials.json")
cluster <- makeCluster("azure/cluster.json")
# "mkdir /mnt/batch/tasks/shared/fileshare",
# "mount -t cifs //totostor.file.core.windows.net/timbsmb /mnt/batch/tasks/shared/fileshare -o vers=3.0 username=totostor,password=xzGGGBX0kthUxzz78zMUNZfHjhWr5L8FlvRpu3xfzT06KHlAKC78+fjY8/3Mp2aE+//yaOz8Vy5MKyKqZfFBZw==, dir_mode=0777, file_mode=0777, sec=ntlmssp"
registerDoAzureParallel(cluster)
jeton_sas <- "?sv=2019-12-12&ss=bfqt&srt=sco&sp=rl&se=2020-12-31T18:28:15Z&st=2020-11-16T10:28:15Z&spr=https&sig=QcFK2rI9U91Eyabd7keZelEBNjoOzxiroWyrI%2BUkeh4%3D"

# IDFM --------------------------- 

tr <- foreach(i = iter(morceaux), .packages = pckgs44each) %dopar% {
  DVFdata <- "."
  install.packages("r5r", repos="https://cloud.r-project.org")
  library("rJava")
  library("r5r")
  log_threshold(FATAL)
  options(java.parameters = "-Xmx16G" )
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
    resolution=50,
    tmax=90,
    pdt=5,
    routing=tr_r5)
  rr
}

stopCluster(cluster)

nn <- names(tr[[1]])
names(nn) <- nn
iso_tr_r5_50 <- map(nn, function(n) reduce(map(tr, n), function(r1, r2) merge(r1, r2)))
iso_tr_r5_50_az <- map(iso_tr_r5_50, ~{
  names(.x) <- names(tr[[1]]$EMP09)
  .x})
ttrr5az_emp09 <- iso2time(iso_tr_r5_50_az$EMP09, seuils=c(25000, 50000, 100000,200000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(iso_tr_r5_50_az, rep="rda/isoIDF50")
save_DVF(ttrr5az_emp09, rep="rda/isoIDF50")

# GPE ------------------

az_GPE_r5 <- foreach(i = iter(morceaux), .packages = pckgs44each) %dopar% {
  DVFdata <- "."
  install.packages("r5r", repos="https://cloud.r-project.org")
  library("rJava")
  library("r5r")
  log_threshold(FATAL)
  options(java.parameters = "-Xmx24G" )
  getr5datafromAzFS(jeton_sas = jeton_sas, path="IDFMGPE")
  tr_r5 <- routing_setup_r5(
    path="IDFMGPE", 
    mode=c("WALK", "TRANSIT"),
    time_window=60,
    montecarlo = 100, 
    percentiles = 5L,
    n_threads = 4)
  rr <- iso_accessibilite(
    quoi=opp,
    ou= i,
    resolution=50,
    tmax=90,
    pdt=5,
    routing=tr_r5)
  rr
}

stopCluster(cluster)

nn <- names(az_GPE_r5[[1]])
names(nn) <- nn
azGPE50 <- map(nn, function(n) reduce(map(test_az, n), function(r1, r2) merge(r1, r2)))
iso_tr_r5_50_az <- map(azGPE50, ~{
  names(.x) <- names(az_GPE[[1]]$EMP09)
  .x})
tGPEr5az_emp09 <- iso2time(iso_tr_r5_50_az$EMP09, seuils=c(25000, 50000, 100000,200000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(iso_tr_r5_50_az, rep="rda/isoIDF")
save_DVF(tGPEr5az_emp09, rep="rda/isoIDF")