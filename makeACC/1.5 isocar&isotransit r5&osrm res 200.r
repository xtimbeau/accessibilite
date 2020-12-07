source("access.r")
plan(sequential)

c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")

# idf ----------------------------------

idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))

# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% st_drop_geometry() %>%  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

# Calcul de la carte transport en commun r5 ------------------------------------------
tr_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFM" %>% glue, 
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 4)

res <- 200
rr <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=res,      
  tmax=90,            
  pdt=5,               
  routing=tr_r5)

save_DVF(rr, "isotr{res}r5idf" %>% glue, rep="rda/iso200")

# pour réaggréger si en morceau
isos_tr <- map(depIdf, ~load_DVF("iso75/isotr200r5d{.x}"))

iso_tr_200_r5 <-list(
  EMP09 = do.call(raster::merge, map(isos_tr, "EMP09")),
  P15_POP = do.call(raster::merge, map(isos_tr, "P15_POP")),
  cste = do.call(raster::merge, map(isos_tr, "cste")))

names(iso_tr_200_r5$EMP09) <- names(isos_tr[[1]]$EMP09)
names(iso_tr_200_r5$P15_POP) <- names(isos_tr[[1]]$P15_POP)
names(iso_tr_200_r5$cste) <- names(isos_tr[[1]]$cste)
save_DVF(iso_tr_200_r5, rep="rda/iso200")

#isotime
ttrr5_emp09 <- iso2time(iso_tr_200_r5$EMP09, seuils=c(100000, 200000, 250000,500000,1000000,2000000,3000000,4000000))
save_DVF(ttrr5_emp09, rep="rda/iso200")

# voiture OSRM IDF

plan("multiprocess", workers=8)

car_osrm <- routing_setup_osrm(server="5003", profile="driving")
foot_osrm <- routing_setup_osrm(server="5001", profile="walk")

rr <- iso_accessibilite(
    quoi=opp,                       
    ou=c200_idf,                       
    resolution=200,                    
    tmax=90,                         
    pdt=5,                          
    routing=car_osrm)

save_DVF(rr, "isocar{res}osrmidf" %>% glue, rep="rda/iso200")

tcarosrm_emp09 <- iso2time(rr$EMP09, seuils=c(100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tcarosrm_emp09, rep="rda/iso200")

# Calcul de GPE transport en commun ------------------------------------------------------

trGPE_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFMGPE" %>% glue, mode=c("WALK", "TRANSIT"),
                             time_window=60,
                             montecarlo = 100, 
                             percentiles = 5L,
                             n_threads = 4)

iso_GPE_200_r5 <-iso_accessibilite(
  quoi=opp, 
  ou=c200_idf,
  resolution=200, 
  tmax=90,
  pdt=5, 
  routing=tr_r5)

save_DVF(iso_GPE_200_r5, rep="rda/iso200")

tGPEr5_emp09 <- iso2time(iso_GPE_200_r5$EMP09, seuils=c(100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tGPEr5_emp09, rep="rda/iso200")

