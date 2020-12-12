source("access.r")
# init ----------------------
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
mtrl <- iris15 %>% filter(COM=="93048") %>% st_union
mtrl20 <- mtrl %>% st_buffer(20000)
iris15_mtrl20 <- iris15 %>% filter(st_within(., mtrl20, sparse=FALSE))
c200_mtrl <- c200 %>% filter(st_within(., mtrl, sparse=FALSE))

# définition des points d'arrivée à partir des centres des iris
opp <- iris15_mtrl20 %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% 
  st_drop_geometry() %>%
  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

threads <- 8

# resolution 50 -------------------
res <- 50
dir.create("{localdata}/ttemp" %>% glue, recursive=TRUE)

## road r5 ----------------------
car_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFM" %>% glue, 
  mode=c("CAR"),
  time_window=60,
  montecarlo = 1, 
  percentiles = 50L,
  n_threads = threads)
mtrlcarr550 <- iso_accessibilite(
    quoi=opp,            
    ou=c200_mtrl,          
    resolution=res,      
    tmax=90,            
    pdt=5,               
    routing=car_r5,
    dir="{localdata}/ttemp" %>% glue)
save_DVF(mtrlcarr550, rep="rda/isoComp/")
## road osrm ----------------------
dir.create("{localdata}/ttemp1" %>% glue, recursive=TRUE)
car_osrm <- routing_setup_osrm(server="5003", profile="driving")
mtrlcarosrm50 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_mtrl,          
  resolution=res,      
  tmax=90,            
  pdt=5,               
  routing=car_osrm,
  dir="{localdata}/ttemp1" %>% glue)
save_DVF(mtrlcarosrm50, rep="rda/isoComp/")

# résolution 200 -----------------
c200 <- load_DVF("c200") %>% st_transform(3035)
iris15 <- load_DVF("iris15")
idfplus20 <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(20000) %>% st_union
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
iris15_idf <- iris15 %>% filter(st_within(., idfplus20, sparse=FALSE))
c200_idf <- c200 %>% filter(st_within(., uu851, sparse=FALSE))

# définition des points d'arrivée à partir des centres des iris
opp <- iris15_idf %>% transmute(EMP09, P15_POP, cste=1) %>% st_centroid()

total_opp <- opp %>% st_drop_geometry() %>%  summarize(EMP09=sum(EMP09), P15_POP=sum(P15_POP), cste=sum(cste))

## car ---------
# r5
dir.create("{localdata}/ttemp2" %>% glue, recursive=TRUE)
car_r5 <- routing_setup_r5(
  path="{DVFdata}/r5r_data/IDFM" %>% glue, 
  mode=c("CAR"),
  time_window=60,
  montecarlo = 1, 
  percentiles = 50L,
  n_threads = 8)
carr5200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=car_r5,
  dir="{localdata}/ttemp2" %>% glue)
save_DVF(carr5200, rep="rda/isoComp/")
# osrm 
dir.create("{localdata}/ttemp3" %>% glue, recursive=TRUE)
car_osrm <- routing_setup_osrm(server="5003", profile="driving")
carosrm200 <- iso_accessibilite(
  quoi=opp,            
  ou=c200_idf,          
  resolution=200,      
  tmax=90,            
  pdt=5,               
  routing=car_osrm,
  dir="{localdata}/ttemp3" %>% glue)
save_DVF(carosrm200, rep="rda/isoComp/")

osrm.r <- load_DVF("isoComp/carosrm200")$EMP09
r5.r <- load_DVF("isoComp/carr5200")$EMP09
osrm <- load_DVF("isoComp/carosrm200")$EMP09 %>% r2dt
r5 <- load_DVF("isoComp/carr5200")$EMP09 %>% r2dt
data <- merge(osrm, r5, by="idINS200", suffix=c(".osrm", ".r5"))
ggplot(data, aes(x=iso25m.osrm, y=iso20m.r5))+geom_point(alpha=0.1)
tm_shape(brick(osrm.r$iso30m/cellStats(osrm.r$iso30m, max), crop(r5.r$iso30m/cellStats(r5.r$iso30m, max), extent(osrm.r$iso30m))))+tm_raster(style="cont", palette = heatvb)
