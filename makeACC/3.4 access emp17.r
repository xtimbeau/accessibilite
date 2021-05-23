source("init.r")
# init ----------------------
plan(sequential)
c200 <- lload_DVF("c200") %>% st_transform(3035)
iris15 <- lload_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
c200_idf10k <- c200 %>% filter(st_within(., st_buffer(uu851, 10000), sparse=FALSE))

emp17 <- lload_DVF("emp17")

# définition des points d'arrivée à partir des centres des iris
opp17 <- emp17 %>% 
  select(-EMP09, -CODE_IRIS) %>%
  st_centroid() %>% 
  left_join(c200_idf %>% as_tibble() %>% select(idINS200, pop15=Ind), by="idINS200") %>% 
  transmute(emp17, pop15=replace_na(pop15, 0), cste=1)

opp09 <- iris15_idf %>% transmute(emp09=EMP09, pop15=P15_POP, cste=1) %>% st_centroid()
threads <- 16

# c200idf10k <- st_join(x=c200_idf10k, y=iris15 %>% select(CODE_IRIS, P15_POP), join=st_intersects, largest=TRUE)
# iris2 <- c200idf10k %>% group_by(CODE_IRIS) %>% summarize(pop=first(P15_POP), ind=sum(Ind))
# tm_shape(iris2)+tm_borders()+tm_shape(iris15_idf)+tm_borders(col="blue")

# https://openmobilitydata.org/p/stif/822?p=8

# EMP17/POP15 carreau 200m sur grille 50m ----------------------

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("WALK", "TRANSIT"),
  time_window=60,
  montecarlo = 30, 
  percentiles = 5L,
  n_threads = threads)

tr_r5_e17_50_2020 <- iso_accessibilite(
  quoi=opp17,            
  ou=idf,          
  resolution=50,      
  tmax=120,              
  routing=r5_20, 
  dir="{localdata}/trr5e1750_2020" %>% glue)

 save_DVF(tr_r5_e17_50_2020, rep="isochrones")

tr_r5_e09_50_2020 <- iso_accessibilite(
  quoi=opp09,            
  ou=idf,          
  resolution=50,      
  tmax=120,              
  routing=r5_20, 
  dir="{localdata}/trr5e0950_2020" %>% glue)

save_DVF(tr_r5_e09_50_2020, rep="isochrones")

# EMP17/POP15 resolution dvf r5 sur grille 50m ---------------------

dv3fv5 <- lload_DVF("dv3fv5.c3035.u") %>%
  st_as_sf(coords=c("x", "y"), crs=3035)

tr_r5_e17_dvfv5 <- iso_accessibilite(
  quoi=opp17,            
  ou=dv3fv5,      
  tmax=120,            
  routing=r5_20, 
  dir="{localdata}/trr5dvfv5e17" %>% glue,
  out="data.table")

save_DVF(tr_r5_e17_dvfv5)

tr_r5_e09_dvfv5 <- iso_accessibilite(
  quoi=opp09,            
  ou=dv3fv5,      
  tmax=120,            
  routing=r5_20, 
  dir="{localdata}/trr5dvfv5e09" %>% glue,
  out="data.table")

save_DVF(tr_r5_e09_dvfv5)