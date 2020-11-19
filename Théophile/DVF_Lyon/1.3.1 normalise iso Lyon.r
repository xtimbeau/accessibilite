source("access.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

# transports en communs

# EMP09

iso_transit_50_r5_Lyon <- load_DVF("iso_transit_50_r5_Lyon")

norm_tr_Lyon <- iso_transit_50_r5_Lyon$bricks$EMP09/iso_transit_50_r5_Lyon$vars$EMP09
isotimes <- names(norm_tr_Lyon) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Lyon <- iso2time(iso_transit_50_r5_Lyon$EMP09, seuils=c(25000,50000,75000, 100000,125000, 150000,175000, 200000,225000, 250000, 275000, 300000))

save_DVF(ttr_r5_emp09_Lyon)


# P15_POP

norm_tr_Lyon <- iso_transit_50_r5_Lyon$bricks$P15_POP/iso_transit_50_r5_Lyon$vars$P15_POP
isotimes_Lyon <- names(norm_tr_Lyon) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_pop15_Lyon <- iso2time(iso_transit_50_r5_Lyon$P15_POP, seuils=c(50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,600000))

save_DVF(ttr_r5_pop15_Lyon)

# car ------------------

# EMP09

iso_car_50_osrm_Lyon <- load_DVF("iso_car_50_osrm_Lyon")

norm_car_Lyon <- iso_car_50_osrm_Lyon$bricks$EMP09/iso_car_50_osrm_Lyon$vars$EMP09
isotimes_Lyon <- names(norm_car_Lyon) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Lyon <- iso2time(iso_car_50_osrm_Lyon$EMP09, seuils=c(50000, 100000,150000,200000,250000,300000,350000,400000,450000,500000))

save_DVF(tcar_osrm_emp09_Lyon)


# P15_POP

norm_car <- iso_car_50_osrm_Lyon$bricks$P15_POP/iso_car_50_osrm_Lyon$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_pop15_Lyon <- iso2time(iso_car_50_osrm_Lyon$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,350000,400000,450000,500000))

save_DVF(tcar_osrm_pop15_Lyon)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
