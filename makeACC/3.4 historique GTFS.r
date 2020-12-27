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
threads <- 8

tr_r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2020 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=tr_r5_20, 
  dir="{localdata}/trr5200_2020" %>% glue)

save_DVF(tr_r5_2020)

tr_r5_19 <- routing_setup_r5(
  path="{localdata}/IDFM 2019",
  date = "16-12-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2019 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=tr_r5_19, 
  dir="{localdata}/trr5200_2019" %>% glue)

save_DVF(tr_r5_2019)

tr_r5_18 <- routing_setup_r5(
  path="{localdata}/IDFM 2018",
  date = "15-10-2018 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2018 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=tr_r5_18, 
  dir="{localdata}/trr5200_2018" %>% glue)

save_DVF(tr_r5_2018)

tr_r5_17 <- routing_setup_r5(
  path="{localdata}/IDFM 2017",
  date = "20-03-2017 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2017 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=tr_r5_17, 
  dir="{localdata}/trr5200_2017" %>% glue)

save_DVF(tr_r5_2017)

# refonte des lignes de bus le 20 avril 2019

r5_avb <- routing_setup_r5(
  path="{localdata}/IDFM 2019.04",
  date = "15-04-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_avant_bus <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=r5_ab, 
  dir="{localdata}/trr5200avb" %>% glue)
save_DVF(tr_r5_avant_bus)

tr_r5_avant_bus_50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=50,      
  tmax=90,            
  pdt=5,               
  routing=r5_avb, 
  dir="{localdata}/trr550avb" %>% glue)
save_DVF(tr_r5_avant_bus_50)

tr_r5_apb <- routing_setup_r5(
  path="{localdata}/IDFM 2019.04.2",
  date = "29-04-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_apres_bus_50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=50,      
  tmax=90,            
  pdt=5,               
  routing = tr_r5_apb, 
  dir="{localdata}/trr550apb" %>% glue)

save_DVF(tr_r5_apres_bus_50)

# résolution 50 
