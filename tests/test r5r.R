utils::remove.packages('r5r')
devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

# test f.accessibilite R5.r ----------------

source("dvf.r")
iris15 <- load_DVF("iris15")
emp_iris <- load_DVF("emp_iris_idf") %>% select(EMP09, P15_POP)
iris15_75 <- iris15  %>% select(DEP, UU2010, EMP09, P15_POP) %>% filter(DEP=="75")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union
d75 <- iris15 %>% filter(DEP=="75") %>% st_union
montreuil <- iris15 %>% filter(COM=="93048") %>% st_union
iris15_idf <- iris15 %>% select(DEP, UU2010, EMP09, P15_POP) %>% filter(st_within(.,idf, sparse=FALSE))
# dv3f <- load_DVF("dv3fv4")
c200 <- load_DVF("c200idf") %>% st_transform(3035)  %>% st_as_sf
c200_75 <- c200 %>% filter(st_within(., d75, sparse=FALSE))
c200_mt <- c200 %>% filter(st_within(., montreuil, sparse=FALSE))
core_r5 <- setup_r5(data_path = "{DVFdata}/r5r_data/IDFM" %>% glue, verbose = FALSE)

# transit

rs <- r5_accessibilite(de_quoi=iris15_75, resolution=50, r5_core=core_r5, temps_max=60, pas_de_temps=5, montecarlo=1, time_window=1L)
rs3 <- r5_accessibilite(de_quoi=iris15_75, resolution=50, r5_core=core_r5, temps_max=60, pas_de_temps=5, montecarlo=50, time_window=60L)
a <- tm_shape(rs$EMP09[["iso25m"]])+tm_raster( style="cont", n=9)
b <- tm_shape(rs3$EMP09[["iso25m"]])+tm_raster( style="cont", n=9)
c <- tm_shape((rs3$EMP09[["iso25m"]]-rs$EMP09[["iso25m"]])/cellStats(rs$EMP09[["iso25m"]], max))+tm_raster(breaks=c(-.1, -.05,0,0.05,0.1), style="cont", n=9)
tmap_arrange(a,b,c)

# comparaison avec iso_transit_50 calculé avec OTP
r550.1 <- r5_accessibilite(quoi=iris15_idf,
                           ou=c200_mt,
                           resolution=50,
                           r5_core=core_r5,
                           temps_max=100,
                           pas_de_temps=5,
                           montecarlo = 1,
                           time_window = 1,
                           max_walk_dist = 500)

r550 <- accessibilite_r5(quoi=iris_idf,
                         ou=c200_mt,
                         resolution=200,
                         r5_core=core_r5,
                         temps_max=100,
                         pas_de_temps=5,
                         montecarlo = 100,
                         time_window = 60,
                         max_walk_dist=500)

r550_60 <- r550.1$EMP09[["iso60m"]]
rotp <- load_DVF("iso_transit_50")$bricks$EMP09[["iso60mn"]]
rotp <- mask(crop(rotp, r550_60), r550_60)

c <- brick(rotp, r550_60)
tm_shape(c)+tm_raster(style="cont")
fdc <- tm_shape(read_osm(r550_60, type="stamen-toner", zoom=10))+tm_rgb()

cc <- brick(r550$EMP09[["iso30m"]], r550.1$EMP09[["iso30m"]])
tm_shape(cc)+tm_raster(style="cont")

# CAR
carr550 <- r5_accessibilite(
  quoi=emp_iris,
  ou=c200,
  resolution=50,
  r5_core=core_r5,
  temps_max=15,
  mode="CAR",
  pas_de_temps=1,
  montecarlo = 1,
  time_window = 1,
  max_walk_dist = 500)

carr550_15 <- carr550$EMP09[["iso15m"]]
carotp <- load_DVF("iso_car_50")$bricks$EMP09[["iso25mn"]]
carotp <- mask(crop(carotp, carr550_30), carr550_30)

c <- brick(carotp, carr550_15)
fdc <- tm_shape(read_osm(carr550_15, type="stamen-toner", zoom=10))+tm_rgb()
fdc+tm_shape(carr550_15)+tm_raster(style="cont")

# WALK
r550.w <- r5_accessibilite(quoi=iris15_idf,
                           ou=c200_mt,
                           resolution=50,
                           r5_core=core_r5,
                           mode="WALK",
                           temps_max=20,
                           pas_de_temps=1,
                           montecarlo = 1, time_window = 1,
                           max_walk_dist = 500)

# isochrones pour vérifier
source("dvf.r")
uu851 <- load_DVF("uu851")
fdc <- tm_shape(read_osm(uu851$depsf, type="stamen-toner", zoom=9))+tm_rgb()
library(ceramic)
Sys.setenv(MAPBOX_API_KEY = "pk.eyJ1IjoieHRpbWJlYXUiLCJhIjoiY2tnMHhiNnAwMGJyaTJzcXdqbXU1c3Y0MiJ9.ydGev8EOzUGtIUHeLlZqtQ")
uu851 <- load_DVF("uu851")

remotes::install_github("walkerke/mapboxapi")
library(mapboxapi)
# Get your access token from your Mapbox account and save it in R; save a public token, 
# secret token, or both with successive calls to mb_access_token()
mapboxapi::mb_access_token("pk.eyzdl....", install = TRUE)

im <- cc_location(uu851$depsf %>% st_transform(st_crs("+init=epsg:3857")), 
                  type = "mapbox.light")

# tm_shape(fdc)+tm_rgb()
# car
library(osrm)
options(osrm.server = "http://localhost:5000/", osrm.profile = "driving")
sf <- osrmIsochrone(c(lon=2.448, lat=48.869), breaks=seq(5,30,1), res=100, returnclass="sf")
m2 <- fdc+tm_shape(sf)+tm_fill(col="id", alpha=0.75)

library(r5r)
tr_r5 <- setup_r5(data_path = "{DVFdata}/r5r_data/IDFM" %>% glue, verbose = FALSE)
r1 <- r5_isochrone(lon=2.448, lat=48.869, resolution=50, r5_core = tr_r5, mode=c("WALK"), temps_max=30)
m1 <- fdc+tm_shape(r1)+tm_raster(style="cont", alpha=0.75)

otpc <- OTP_server(router="IDF3", port=8100, memory = "8G", rep=DVFdata)
otp_isochrone(lon=2.448, lat=48.869, otp_core = otpc, mode="CAR", plot=TRUE, temps_max=30)

tmap_arrange(m1, m2)

# walk
fdc <- tm_shape(read_osm(sf, type="stamen-toner"))+tm_rgb()
library(osrm)
options(osrm.server = "http://localhost:5000/", osrm.profile = "driving")
sf <- osrmIsochrone(c(lon=2.448, lat=48.869), breaks=seq(5,15,1)*1.609, res=30, returnclass="sf")
m2 <- fdc+tm_shape(sf)+tm_fill(col="id", alpha=0.5, style="cont")
tmap_arrange(m1, m2)

core_r5 <- setup_r5(data_path = "{DVFdata}/r5r_data/IDFM" %>% glue, verbose = FALSE)
r1 <- r5_isochrone(lon=2.4159, lat=48.8513, resolution=10, r5_core =core_r5, mode=c("WALK"), temps_max=15)
m1 <- fdc+tm_shape(r1)+tm_raster(style="cont", alpha=0.5)

otpc <- OTP_server(router="IDF3", port=8100, memory = "8G", rep=DVFdata)
otp_isochrone(lon=2.448, lat=48.869, otp_core = otpc, mode="CAR", plot=TRUE, temps_max=30)

library(osrm)
options(osrm.server = "http://localhost:5000/", osrm.profile = "driving")
sf <- osrmIsochrone(c(lon=2.448, lat=48.869), breaks=seq(5,30,5), res=500, returnclass="sf")
m2 <- tm_shape(read_osm(sf, type="stamen-toner"))+tm_rgb()+tm_shape(sf)+tm_fill(col="id", alpha=0.5)
tmap_arrange(m1, m2)


# test accessibilite formule générique
source("dvf.r")
plan("multiprocess", workers=8)
iris15 <- load_DVF("iris15")
emp_iris <- load_DVF("emp_iris_idf") %>% select(EMP09, P15_POP)
iris15_75 <- iris15  %>% select(DEP, UU2010, EMP09, P15_POP) %>% filter(DEP=="75")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union
d75 <- iris15 %>% filter(DEP=="75") %>% st_union
montreuil <- iris15 %>% filter(COM=="93048") %>% st_union
iris15_idf <- iris15 %>% select(DEP, UU2010, EMP09, P15_POP) %>% filter(st_within(.,idf, sparse=FALSE))
c200 <- load_DVF("c200idf") %>% st_transform(3035)  %>% st_as_sf
c200_75 <- c200 %>% filter(st_within(., d75, sparse=FALSE))
c200_mt <- c200 %>% filter(st_within(., montreuil, sparse=FALSE))

car_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFM" %>% glue, mode="CAR")

tr_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFM" %>% glue, mode=c("WALK", "TRANSIT"))

iso_transit_50_r5 <- iso_accessibilite(quoi=iris15_idf,
                        ou=c200_75,
                        resolution=50,
                        tmax=60,
                        pdt=5,
                        routing=tr_r5)


r5 <- iso_accessibilite(quoi=iris15_idf,
                  ou=c200_mt,
                  resolution=50,
                  tmax=30,
                  pdt=5,
                  routing=car_r5)

car_otp <- routing_setup_otpv1(mode="CAR", router="IDF3", port=8000)

otp <- iso_accessibilite(quoi=iris15_idf,
                          ou=c200_mt,
                          resolution=200,
                          tmax=30,
                          routing=car_otp)


car_osrm <- routing_setup_osrm(server="5000", profile="driving")
car_osrm_tl20 <- routing_setup_osrm(server="5002", profile="driving")
foot_osrm <- routing_setup_osrm(server="5001", profile="walk")

trafficlight20  <- iso_accessibilite(quoi=iris15_idf,
                        ou=c200,
                        resolution=50,
                        tmax=60,
                        pdt=5,
                        routing=car_osrm)

r2 <- iso_accessibilite(quoi=iris15_idf,
                        ou=c200_mt,
                        resolution=50,
                        tmax=60,
                        pdt=5,
                        routing=routing_setup_osrm(server="5002", profile="driving"))


tm_shape(r2$EMP09-r1$EMP09)+tm_raster(style = "cont")
# debug 
sq <- quoi_4326[quoi_gr[[grps[[1]]]],]
ggplot()+
  geom_point(data=tibble(x=quoi_4326$x, y=quoi_4326$y), mapping=aes(x=x,y=y), col="blue")+
  geom_point(tibble(x=sq$x, y=sq$y),mapping=aes(x=x,y=y), col="grey", alpha=0.5)
