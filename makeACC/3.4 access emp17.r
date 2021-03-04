source("access.r")
# init ----------------------
plan(sequential)
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
c200_idf10k <- c200 %>% filter(st_within(., st_buffer(uu851, 10000), sparse=FALSE))

emp17 <- lload_DVF("emp17")

# définition des points d'arrivée à partir des centres des iris
opp <- emp17 %>% select(-EMP09, -CODE_IRIS) %>% st_centroid()

threads <- 8

# c200idf10k <- st_join(x=c200_idf10k, y=iris15 %>% select(CODE_IRIS, P15_POP), join=st_intersects, largest=TRUE)
# iris2 <- c200idf10k %>% group_by(CODE_IRIS) %>% summarize(pop=first(P15_POP), ind=sum(Ind))
# tm_shape(iris2)+tm_borders()+tm_shape(iris15_idf)+tm_borders(col="blue")

# https://openmobilitydata.org/p/stif/822?p=8

# référence 2020 (50m&200m) ----------------------

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_e17_2020 <- iso_accessibilite(
  quoi=opp,            
  ou=idf,          
  resolution=200,      
  tmax=90,              
  routing=r5_20, 
  dir="{localdata}/trr5emp17_2020" %>% glue)

save_DVF(tr_r5_e17_2020)

tr_r5_e17_50_2020 <- iso_accessibilite(
  quoi=opp,            
  ou=idf,          
  resolution=50,      
  tmax=90,              
  routing=r5_20, 
  dir="{localdata}/trr5emp1750_2020" %>% glue)

save_DVF(tr_r5_e17_50_2020)

# GPE (50m&200m) ----------------------

r5_GPE <- routing_setup_r5(
  path="{localdata}/IDFM 2020 GPE",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_GPE_e17_50 <- iso_accessibilite(
  quoi=opp,            
  ou=idf,          
  resolution=50,      
  tmax=120,             
  routing=tr_r5_20, 
  dir="{localdata}/trr5e1750_GPE" %>% glue)
save_DVF(tr_r5_GPE_e17_50)

tr_r5_GPE <- iso_accessibilite(
  quoi=opp,            
  ou=idf,          
  resolution=200,      
  tmax=120,             
  routing=tr_r5_20, 
  dir="{localdata}/trr5e17200_GPE" %>% glue)
save_DVF(tr_r5_GPE_e17)

# référence 2020 (points DV3F) ----------------------
dv3f <- lload_DVF("dv3f.c3035.u") %>%
  st_as_sf(coords=c("x", "y"), crs=3035)
  
tr_r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_e17_2020_dvf <- iso_accessibilite(
  quoi=opp,            
  ou=dv3f,      
  tmax=120,            
  pdt=1,               
  routing=tr_r5_20, 
  dir="{localdata}/trr5_e17_2020_dvf3" %>% glue,
  out="data.table")

save_DVF(tr_r5_e17_2020_dvf)

# cartes pour différentes années (200m) ---------------

tr_r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode = c("WALK", "TRANSIT"),
  time_window = 60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2020 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=tr_r5_20, 
  dir="{localdata}/trr5200_2020" %>% glue)

save_DVF(tr_r5_2020)

tr_r5_19 <- routing_setup_r5(
  path="{localdata}/IDFM 2019",
  date = "16-12-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2019 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=tr_r5_19, 
  dir="{localdata}/trr5200_2019" %>% glue)

save_DVF(tr_r5_2019)

tr_r5_18 <- routing_setup_r5(
  path="{localdata}/IDFM 2018",
  date = "15-10-2018 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2018 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=tr_r5_18, 
  dir="{localdata}/trr5200_2018" %>% glue)

save_DVF(tr_r5_2018)

tr_r5_17 <- routing_setup_r5(
  path="{localdata}/IDFM 2017",
  date = "20-03-2017 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2017 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=tr_r5_17, 
  dir="{localdata}/trr5200_2017" %>% glue)

save_DVF(tr_r5_2017)

# refonte du RER D le 10 décembre 2018 50m&200m ----------------

r5_Dav <- routing_setup_r5(
  path="{localdata}/IDFM 2018.10",
  date = "22-10-2018 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_Dav <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=r5_Dav, 
  dir="{localdata}/trr5200Dav" %>% glue)
save_DVF(tr_r5_Dav)

tr_r5_Dav_50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=r5_Dav, 
  dir="{localdata}/trr550Dav" %>% glue)
save_DVF(tr_r5_Dav_50)

r5_Dap <- routing_setup_r5(
  path="{localdata}/IDFM 2019.02",
  date = "25-02-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_Dap <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=r5_Dap, 
  dir="{localdata}/trr5200DaP" %>% glue)
save_DVF(tr_r5_Dap)

tr_r5_Dap_50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=50,      
  tmax=90,             
  routing=r5_Dap, 
  dir="{localdata}/trr5200DaP" %>% glue)
save_DVF(tr_r5_Dap_50)

# refonte des lignes de bus le 20 avril 2019 50m&200m ----------------

r5_avb <- routing_setup_r5(
  path="{localdata}/IDFM 2019.04",
  date = "15-04-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_avant_bus <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=r5_avb, 
  dir="{localdata}/trr5200avb" %>% glue)
save_DVF(tr_r5_avant_bus)

tr_r5_avant_bus_50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=50,      
  tmax=90,             
  routing=r5_avb, 
  dir="{localdata}/trr550avb" %>% glue)
save_DVF(tr_r5_avant_bus_50)

r5_apb <- routing_setup_r5(
  path="{localdata}/IDFM 2019.04.2",
  date = "29-04-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_apres_bus <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing = r5_apb, 
  dir="{localdata}/trr5200apb" %>% glue)

save_DVF(tr_r5_apres_bus)

tr_r5_apres_bus_50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=50,      
  tmax=90,             
  routing = r5_apb, 
  dir="{localdata}/trr550apb" %>% glue)

save_DVF(tr_r5_apres_bus_50)

# BUS RAIL vs TRANSIT 200m ---------------------------

rail_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "RAIL"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

rail_r5_2020_200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=rail_20, 
  dir="{localdata}/railr5200_2020" %>% glue)
save_DVF(rail_r5_2020_200)

bus_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "BUS"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

bus_r5_2020_200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=bus_20, 
  dir="{localdata}/busr5200_2020" %>% glue)
save_DVF(bus_r5_2020_200)

# POP c200 versus POP IRIS 200m ----------------------

tr_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = 8)

c200_idf10k <- c200_idf10k %>%
  st_centroid() %>% 
  transmute(pop=Ind) %>% 
  st_agr_aggregate(pop)

popc200_2020 <- iso_accessibilite(
  quoi=c200_idf10k,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=tr_20, 
  dir="{localdata}/trr5200_2020_c200" %>% glue)

save_DVF(popc200_2020)

# medianne au lieu de 5% 200m --------------------

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 50L,
  n_threads = 8)

tr_r5_2020_median <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=r5_20, 
  dir="{localdata}/trr5200_2020_median" %>% glue)
save_DVF(tr_r5_2020_median)

# 20 draws versus 120 200m --------------------

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 20, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2020_20d <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,             
  routing=r5_20, 
  dir="{localdata}/trr5200_2020_20d" %>% glue)
save_DVF(tr_r5_2020_20d)

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 120, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_2020_120d <- iso_accessibilite(
  quoi=opp,
  ou=c200_idf, 
  resolution=200,
  tmax=90, 
  routing=r5_20, 
  dir="{localdata}/trr5200_2020_120d" %>% glue)
save_DVF(tr_r5_2020_120d)
