source("init.r")
# init ----------------------
plan(sequential)
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf10k <- c200 %>% filter(st_within(., st_buffer(uu851, 10000), sparse=FALSE))
# définition des points d'arrivée à partir des centres des iris
opp09 <- iris15_idf %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% st_drop_geometry() %>%  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))
threads <- 12

emp17 <- lload_DVF("emp17")

# définition des points d'arrivée à partir des centres des iris
opp17 <- emp17 %>% select(-EMP09, -CODE_IRIS) %>% st_centroid()

# c200idf10k <- st_join(x=c200_idf10k, y=iris15 %>% select(CODE_IRIS, P15_POP), join=st_intersects, largest=TRUE)
# iris2 <- c200idf10k %>% group_by(CODE_IRIS) %>% summarize(pop=first(P15_POP), ind=sum(Ind))
# tm_shape(iris2)+tm_borders()+tm_shape(iris15_idf)+tm_borders(col="blue")

# https://openmobilitydata.org/p/stif/822?p=8

# référence 2020 (50m) ----------------------

r5_20 <- routing_setup_r5(
  path="{localdata}/IDFM 2020",
  date = "14-12-2020 9:00:00",
  mode=c("CAR"),
  time_window=1,
  montecarlo = 2, 
  percentiles = 50L,
  n_threads = 8)

tr_r550_e09 <- iso_accessibilite(
  quoi=opp09,            
  ou=idf,          
  resolution=50,      
  tmax=90,              
  routing=r5_20, 
  dir="{localdata}/carr550_e09" %>% glue)

tr_r550_e17 <- iso_accessibilite(
  quoi=opp17,            
  ou=idf,          
  resolution=50,      
  tmax=120,              
  routing=r5_20, 
  dir="{localdata}/carr550_e17" %>% glue)

save_DVF(tr_r550_e17)
save_DVF(tr_r550_e09)