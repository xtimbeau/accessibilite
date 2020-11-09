source("access.r")
plan(sequential)
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
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

# la petite couronne en résolution 50 parce que c'est comme ça
walk(c("75", "91", "92", "93"), ~{
  tr_r5 <- routing_setup_r5(
    path="{DVFdata}/r5r_data/IDFM" %>% glue, 
    mode=c("WALK", "TRANSIT"),
    time_window=60,
    montecarlo = 100, 
    percentiles = 5L,
    n_threads = 4)
  rr <- iso_accessibilite(
    quoi=opp,            
    ou=c200_idf %>% filter(dep==.x),          
    resolution=50,      
    tmax=90,            
    pdt=5,               
    routing=tr_r5)
  save_DVF(rr, "isotr50r5d{.x}" %>% glue, rep="rda/iso75")})

# la grande couronne en résolution 200 en attendant d'avoir le temps
walk(c("77", "78", "91", "95"), ~{
  message(.x)
  rr <- iso_accessibilite(
    quoi=opp,            
    ou=c200_idf %>% filter(dep==.x),          
    resolution=200,      
    tmax=90,            
    pdt=5,               
    routing=tr_r5)
  save_DVF(rr, "isotr50r5d{.x}" %>% glue, rep="rda/iso75")})

isos_tr <- map(depIdf, ~load_DVF("iso75/isotr50r5d{.x}"))

iso_tr_50_r5 <-list(
  EMP09 = do.call(raster::merge, map(isos_tr, "EMP09")),
  P15_POP = do.call(raster::merge, map(isos_tr, "P15_POP")),
  cste = do.call(raster::merge, map(isos_tr, "cste")))

names(iso_tr_50_r5$EMP09) <- names(isos_tr[[1]]$EMP09)
names(iso_tr_50_r5$P15_POP) <- names(isos_tr[[1]]$P15_POP)
names(iso_tr_50_r5$cste) <- names(isos_tr[[1]]$cste)
save_DVF(iso_tr_50_r5)

ttrr5_emp09 <- iso2time(iso_tr_50_r5$EMP09, seuils=c(100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(ttrr5_emp09)

# Calcul de la carte voiture OSRM ------------------------------------------------------

plan("multiprocess", workers=8)

car_osrm <- routing_setup_osrm(server="5003", profile="driving")
foot_osrm <- routing_setup_osrm(server="5001", profile="walk")

walk(depIdf, ~{
  message(.x)
  rr <- iso_accessibilite(
    quoi=opp,                       
    ou=c200_idf %>% filter(dep==.x),                       
    resolution=50,                    
    tmax=90,                         
    pdt=5,                          
    routing=car_osrm)
  save_DVF(rr, "isocar50osrmd{.x}" %>% glue, rep="rda/iso75")})               

isos_car <- map(depIdf, ~load_DVF("iso75/isocar50osrmd{.x}"))

iso_car_50_osrm <-list(
  EMP09 = do.call(raster::merge, map(isos_car, "EMP09")),
  P15_POP = do.call(raster::merge, map(isos_car, "P15_POP")),
  cste = do.call(raster::merge, map(isos_car, "cste")))

names(iso_car_50_osrm$EMP09) <- names(isos_car[[1]]$EMP09)
names(iso_car_50_osrm$P15_POP) <- names(isos_car[[1]]$P15_POP)
names(iso_car_50_osrm$cste) <- names(isos_car[[1]]$cste)
save_DVF(iso_car_50_osrm)

tcarosrm_emp09 <- iso2time(iso_car_50_osrm$EMP09, seuils=c(100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tcarosrm_emp09)

# Calcul de GPE transport en commun ------------------------------------------------------

trGPE_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFMGPE" %>% glue, mode=c("WALK", "TRANSIT"),
                             time_window=60,
                             montecarlo = 100, 
                             percentiles = 5L,
                             n_threads = 4)

iso_GPE_50_r5 <-iso_accessibilite(
  quoi=opp, 
  ou=c200_idf,
  resolution=200, 
  tmax=90,
  pdt=5, 
  routing=tr_r5)

save_DVF(iso_GPE_50_r5)

tGPEr5_emp09 <- iso2time(iso_GPE_50_r5$EMP09, seuils=c(100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tGPEr5_emp09)

save_DVF(ttrGPEr5_emp09)
