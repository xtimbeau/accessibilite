source("access.r")
# init ----------------------
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
threads <- 12
# resolution 50 -------------------

## transit r5 ----------------------
tr_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFM" %>% glue, 
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = threads)

# res50
res <- 50
isotrr550 <- iso_accessibilite(
    quoi=opp,            
    ou=c200_idf,          
    resolution=res,      
    tmax=90,            
    pdt=5,               
    routing=tr_r5, 
    dir="{localdata}/trr5{res}" %>% glue)

save_DVF(isotrr550, rep="rda/isoIDF50/")

## transit GPE r5------------------------------------------------------

options(java.parameters = "-Xmx16G" )
trGPE_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFMGPE" %>% glue, 
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

rr <- iso_accessibilite(
    quoi=opp, 
    ou=c200_idf,
    resolution=50, 
    tmax=90,
    pdt=5, 
    routing=trGPE_r5,
    dir=str_c(localdata, "/GPEr550"))

save_DVF(rr, "isoGPE50r5", rep="rda/isoIDF50")

isos_GPE <- map(c("75","91", "92", "93", "77789495"), ~load_DVF("isoIDF50/isoGPE50r5d{.x}"))

iso_GPE_50_r5 <-list(
  EMP09 = do.call(raster::merge, map(isos_GPE, "EMP09")),
  P15_POP = do.call(raster::merge, map(isos_GPE, "P15_POP")),
  cste = do.call(raster::merge, map(isos_GPE, "cste")))

names(iso_GPE_50_r5$EMP09) <- names(isos_GPE[[1]]$EMP09)
names(iso_GPE_50_r5$P15_POP) <- names(isos_GPE[[1]]$P15_POP)
names(iso_GPE_50_r5$cste) <- names(isos_GPE[[1]]$cste)

save_DVF(iso_GPE_50_r5, rep="rda/isoIDF50")

tGPEr5_emp09 <- iso2time(iso_GPE_50_r5$EMP09, seuils=c(25000,50000,750000,100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tGPEr5_emp09, rep="rda/isoIDF50")

# résolution 200 -----------------

tr_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFM" %>% glue, 
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=tr_r5, 
  dir="{localdata}/trr5200" %>% glue)

save_DVF(tr_r5_200, rep="rda/isoIDF200")

trGPE_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFMGPE" %>% glue, 
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

gpe_r5_200 <- iso_accessibilite(
  quoi=opp, 
  ou=c200_idf,
  resolution=200, 
  tmax=90,
  pdt=5, 
  routing=trGPE_r5,
  dir=str_c(localdata, "/GPEr5200"))

save_DVF(gpe_r5_200, rep="rda/isoIDF200")

tGPEr5_emp09_200 <- iso2time(rr200$EMP09, seuils=c(25000, 50000, 750000, 100000,150000,200000, 250000,500000))
save_DVF(tGPEr5_emp09_200, rep="rda/iso200")


# car OSRM ------------------------------------------------------

plan("multiprocess", workers=8)

car_osrm <- routing_setup_osrm(server="5003", profile="driving")
foot_osrm <- routing_setup_osrm(server="5002", profile="walk")

iso_car_50_osrm_idf <- iso_accessibilite(
  quoi=opp,                       
  ou=c200_idf,                       
  resolution=50,                    
  tmax=120,                         
  pdt=5,                          
  routing=car_osrm)

save_DVF(iso_car_50_osrm_idf, rep="rda")               

tcarosrm_emp09 <- iso2time(iso_car_50_osrm_idf$EMP09, seuils=c(25000,50000,750000,100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tcarosrm_emp09)

# réunion des rasters par departement

isos_car <- map(depIdf, ~load_DVF("iso75/isocar50osrmd{.x}"))

iso_car_50_osrm <-list(
  EMP09 = do.call(raster::merge, map(isos_car, "EMP09")),
  P15_POP = do.call(raster::merge, map(isos_car, "P15_POP")),
  cste = do.call(raster::merge, map(isos_car, "cste")))

names(iso_car_50_osrm$EMP09) <- names(isos_car[[1]]$EMP09)
names(iso_car_50_osrm$P15_POP) <- names(isos_car[[1]]$P15_POP)
names(iso_car_50_osrm$cste) <- names(isos_car[[1]]$cste)
save_DVF(iso_car_50_osrm)

tcarosrm_emp09 <- iso2time(iso_car_50_osrm$EMP09, seuils=c(25000,50000,750000,100000,250000,500000,1000000,2000000,3000000,4000000))
save_DVF(tcarosrm_emp09)
