source("access.r")
# init ----------------------
plan(sequential)
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
c200_idf10k <- c200 %>% filter(st_within(., st_buffer(uu851, 10000), sparse=FALSE))
# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% st_drop_geometry() %>%  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))
threads <- 8

# c200idf10k <- st_join(x=c200_idf10k, y=iris15 %>% select(CODE_IRIS, P15_POP), join=st_intersects, largest=TRUE)
# iris2 <- c200idf10k %>% group_by(CODE_IRIS) %>% summarize(pop=first(P15_POP), ind=sum(Ind))
# tm_shape(iris2)+tm_borders()+tm_shape(iris15_idf)+tm_borders(col="blue")

# https://openmobilitydata.org/p/stif/822?p=8

# cartes pour différentes années ---------------

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

# refonte du RER D le 10 décembre 2018 ----------------

r5_Dav <- routing_setup_r5(
  path="{localdata}/IDFM 2018.10",
  date = "22-10-2018 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_Dav <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=r5_Dav, 
  dir="{localdata}/trr5200Dav" %>% glue)
save_DVF(tr_r5_Dav)

r5_Dap <- routing_setup_r5(
  path="{localdata}/IDFM 2019.02",
  date = "25-02-2019 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

tr_r5_Dap <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=r5_Dap, 
  dir="{localdata}/trr5200DaP" %>% glue)
save_DVF(tr_r5_Dap)

t1 <- iso2time(lload_DVF("tr_r5_Dav")$EMP09, c(50,100,500,1000)*1000)
t2 <- iso2time(lload_DVF("tr_r5_Dap")$EMP09, c(50,100,500,1000)*1000)
tm_shape(t2$to100k-t1$to100k)+tm_raster(style="cont", palette = red2gray)

# refonte des lignes de bus le 20 avril 2019 ----------------

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

# BUS RAIL vs TRANSIT ---------------------------

rail_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "RAIL"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

rail_r5_2020_200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=rail_20, 
  dir="{localdata}/railr5200_2020" %>% glue)
save_DVF(rail_r5_2020_200)

bus_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "BUS"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 5L,
  n_threads = 8)

bus_r5_2020_200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=bus_20, 
  dir="{localdata}/busr5200_2020" %>% glue)
save_DVF(bus_r5_2020_200)

rail <- lload_DVF("rail_r5_2020_200")$EMP09 %>% iso2time(c(50,100,1000)*1000)
bus <- lload_DVF("bus_r5_2020_200")$EMP09 %>% iso2time(c(50,100,1000)*1000)
tr <- lload_DVF("tr_r5_2020")$EMP09 %>% iso2time(c(50,100,1000)*1000)

tm_shape(brick(-rail$to1M+tr$to1M))+tm_raster(style="cont")

# POP c200 versus POP IRIS ----------------------

tr_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
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
  pdt=5,               
  routing=tr_20, 
  dir="{localdata}/trr5200_2020_c200" %>% glue)

save_DVF(popc200_2020)

# medianne au lieu de 5% --------------------

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 100, 
  percentiles = 50L,
  n_threads = 8)

tr_r5_2020 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=r5_20, 
  dir="{localdata}/trr5200_2020_median" %>% glue)
tr_r5_2020_median <- tr_r5_2020
save_DVF(tr_r5_2020_median)

t1 <- iso2time(tr1$EMP09, c(50,100,500,1000)*1000)
t2 <- iso2time(tr_r5_2020_median$EMP09, c(50,100,500,1000)*1000)
tm_shape(t2$to100k-t1$to100k)+tm_raster(style="cont", palette = red2gray)
