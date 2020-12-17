source("access.r")

# init ----------------------
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
mtrl <- iris15 %>% filter(COM=="93048") %>% st_union
mtrl20 <- mtrl %>% st_buffer(20000)
iris15_mtrl20 <- iris15 %>% filter(st_within(., mtrl20, sparse=FALSE))
c200_mtrl <- c200 %>% filter(st_within(., mtrl, sparse=FALSE))
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))

# définition des points d'arrivée à partir des centres des iris
opp <- iris15_mtrl20 %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% 
  st_drop_geometry() %>%
  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

threads <- 8

# resolution 50 -------------------
res <- 50
dir.create("{localdata}/ttemp" %>% glue, recursive=TRUE)

# résolution 200 -----------------
# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% st_drop_geometry() %>%  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

## walk ---------
# r5
plan(multisession)
foot_euc <- routing_setup_euc()
footeuc200 <- iso_accessibilite(
  quoi=c200_idf %>% transmute(c=1),            
  ou=c200_idf,          
  resolution=200,      
  tmax=20,            
  pdt=5,               
  routing=foot_euc)
save_DVF(footr5200, rep="rda/isoComp/")
# osrm 
plna(multisession, workers=8)
dir.create("{localdata}/ttemp3" %>% glue, recursive=TRUE)
foot_osrm <- routing_setup_osrm(server="5004", profile="foot")
carosrm200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=20,            
  pdt=5,               
  routing=car_osrm,
  dir="{localdata}/ttemp3" %>% glue)
save_DVF(carosrm200, rep="rda/isoComp/")