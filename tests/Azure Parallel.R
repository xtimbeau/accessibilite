source("access.r")

# devtools::install_github("Azure/rAzureBatch")
# devtools::install_github("Azure/doAzureParallel")

library(doAzureParallel)
library(foreach)

setCredentials("azure/credentials.json")
cluster <- makeCluster("azure/cluster.json")

registerDoAzureParallel(cluster)

c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))

# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% st_drop_geometry() %>%  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

res <- 200
dep <- c("75", "91", "92", "93")

iso_az <- foreach(i = 1:length(dep)) %dopar% {
  tr_r5 <- routing_setup_r5(
    path="r5r_data/IDFM", 
    mode=c("WALK", "TRANSIT"),
    time_window=60,
    montecarlo = 100, 
    percentiles = 5L,
    n_threads = 4)
  rr <- iso_accessibilite(
    quoi=opp,            
    ou=c200_idf %>% filter(dep==dep[i]),          
    resolution=res,      
    tmax=90,            
    pdt=5,               
    routing=tr_r5)
  }

stopCluster(cluster)
