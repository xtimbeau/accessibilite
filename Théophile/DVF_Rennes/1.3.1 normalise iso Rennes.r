source("access.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

iso_transit_50_r5_Rennes <- load_DVF("iso_transit_50_r5_Rennes")

norm_tr_Rennes <- iso_transit_50_r5_Rennes$bricks$EMP09/iso_transit_50_r5_Rennes$vars$EMP09
isotimes_Rennes <- names(norm_tr_Rennes) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Rennes <- iso2time(iso_transit_50_r5_Rennes$EMP09, seuils=c(25000,50000,75000, 100000))

save_DVF(ttr_r5_emp09_Rennes)


# transports en commun P15_POP

norm_tr <- iso_transit_50$bricks$P15_POP/iso_transit_50$vars$P15_POP
isotimes <- names(norm_tr) %>% str_extract("[:digit:]+") %>% as.numeric()


save_DVF(ttr_pop15)

# car emp09 ------------------

iso_car_50_osrm_Rennes <- load_DVF("iso_car_50_osrm_Rennes")

norm_car_Rennes <- iso_car_50_osrm_Rennes$bricks$EMP09/iso_car_50_osrm_Rennes$vars$EMP09
isotimes_Rennes <- names(norm_car_Rennes) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Rennes <- iso2time(iso_car_50_osrm_Rennes$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,350000,400000,450000,500000))

save_DVF(tcar_osrm_emp09_Rennes)

 # car P15_POP

norm_car <- iso_car_50$bricks$P15_POP/iso_car_50$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_pop15_10 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.1))
tcar_pop15_15 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.15))
tcar_pop15_20 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.2))
tcar_pop15_25 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.25))
tcar_pop15_30 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.3))

tcar_pop15 <- brick(list(tcar_pop15_10, tcar_pop15_15, tcar_pop15_20, tcar_pop15_25, tcar_pop15_30))
names(tcar_pop15) <- c("pop10", "pop15", "pop20", "pop25", "pop30")

save_DVF(tcar_pop15)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
