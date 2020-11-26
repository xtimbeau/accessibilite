source("access.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

iso_transit_50_r5_Bordeaux <- load_DVF("iso_transit_50_r5_Bordeaux")

norm_tr_Bordeaux <- iso_transit_50_r5_Bordeaux$bricks$EMP09/iso_transit_50_r5_Bordeaux$vars$EMP09
isotimes_Bordeaux <- names(norm_tr_Bordeaux) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Bordeaux <- iso2time(iso_transit_50_r5_Bordeaux$EMP09, seuils=c(25000,50000,75000, 100000, 125000,150000,175000,200000,225000,250000))

save_DVF(ttr_r5_emp09_Bordeaux)


# isotime pop
norm_tr_Bordeaux <- iso_transit_50_r5_Bordeaux$bricks$P15_POP/iso_transit_50_r5_Bordeaux$vars$P15_POP
isotimes_Bordeaux <- names(norm_tr_Bordeaux) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_pop15_Bordeaux <- iso2time(iso_transit_50_r5_Bordeaux$P15_POP, seuils=c(50000,100000,150000,200000,250000,300000,400000,500000))

save_DVF(ttr_r5_pop15_Bordeaux)

# car emp09------------------

iso_car_50_osrm_Bordeaux <- load_DVF("iso_car_50_osrm_Bordeaux")

norm_car_Bordeaux <- iso_car_50_osrm_Bordeaux$bricks$EMP09/iso_car_50_osrm_Bordeaux$vars$EMP09
isotimes_Bordeaux <- names(norm_car_Bordeaux) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Bordeaux <- iso2time(iso_car_50_osrm_Bordeaux$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,400000,500000))

save_DVF(tcar_osrm_emp09_Bordeaux)

# car P15_POP

norm_car <- iso_car_50_osrm_Bordeaux$bricks$P15_POP/iso_car_50_osrm_Bordeaux$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_pop15_Bordeaux <- iso2time(iso_car_50_osrm_Bordeaux$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,400000,500000))

save_DVF(tcar_osrm_pop15_Bordeaux)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
