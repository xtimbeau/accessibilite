source("access.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

# transports en communs

# EMP09

iso_transit_50_r5_Toulouse <- load_DVF("iso_transit_50_r5_Toulouse")

norm_tr_Toulouse <- iso_transit_50_r5_Toulouse$bricks$EMP09/iso_transit_50_r5_Toulouse$vars$EMP09
isotimes_Toulouse <- names(norm_tr_Toulouse) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Toulouse <- iso2time(iso_transit_50_r5_Toulouse$EMP09, seuils=c(25000,50000,75000, 100000,125000, 150000,175000,200000,225000,250000))

save_DVF(ttr_r5_emp09_Toulouse)


# P15_POP

norm_tr_Toulouse <- iso_transit_50_r5_Toulouse$bricks$P15_POP/iso_transit_50_r5_Toulouse$vars$P15_POP
isotimes_Toulouse <- names(norm_tr_Toulouse) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_pop15_Toulouse <- iso2time(iso_transit_50_r5_Toulouse$P15_POP, seuils=c(50000,100000,150000,200000,250000,300000,350000))

save_DVF(ttr_r5_pop15_Toulouse)

# car ------------------

# EMP09

iso_car_50_osrm_Toulouse <- load_DVF("iso_car_50_osrm_Toulouse")

norm_car_Toulouse <- iso_car_50_osrm_Toulouse$bricks$EMP09/iso_car_50_osrm_Toulouse$vars$EMP09
isotimes_Toulouse <- names(norm_car_Toulouse) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Toulouse <- iso2time(iso_car_50_osrm_Toulouse$EMP09, seuils=c(50000, 100000,150000,200000,250000,300000,350000))

save_DVF(tcar_osrm_emp09_Toulouse)


# P15_POP

norm_car <- iso_car_50_osrm_Toulouse$bricks$P15_POP/iso_car_50_osrm_Toulouse$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_pop15_Toulouse <- iso2time(iso_car_50_osrm_Toulouse$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,350000))

save_DVF(tcar_osrm_pop15_Toulouse)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
