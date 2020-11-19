source("access.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

iso_transit_50_r5_Marseille <- load_DVF("iso_transit_50_r5_Marseille")

# r5 emp09

norm_tr <- iso_transit_50_r5_Marseille$bricks$EMP09/iso_transit_50_r5_Marseille$vars$EMP09
isotimes <- names(norm_tr) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Marseille <- iso2time(iso_transit_50_r5_Marseille$EMP09, seuils=c(25000,50000,75000, 100000, 125000,150000,175000))

save_DVF(ttr_r5_emp09_Marseille)

# r5 pop

norm_tr <- iso_transit_50_r5_Marseille$bricks$P15_POP/iso_transit_50_r5_Marseille$vars$P15_POP
isotimes <- names(norm_tr) %>% str_extract("[:digit:]+") %>% as.numeric()


ttr_r5_pop15_Marseille <- iso2time(iso_transit_50_r5_Marseille$P15_POP, seuils=c(50000,100000,150000,200000,250000,300000,350000,400000,450000,500000))
save_DVF(ttr_r5_pop15_Marseille)

# car ------------------

iso_car_50_osrm_Marseille <- load_DVF("iso_car_50_osrm_Marseille")

norm_car_Marseille <- iso_car_50_osrm_Marseille$bricks$EMP09/iso_car_50_osrm_Marseille$vars$EMP09
isotimes_Marseille <- names(norm_car_Marseille) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Marseille <- iso2time(iso_car_50_osrm_Marseille$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,350000,400000,450000,500000))

save_DVF(tcar_osrm_emp09_Marseille)


# car pop

norm_car <- iso_car_50_osrm_Marseille$bricks$P15_POP/iso_car_50_osrm_Marseille$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_pop15_Marseille <- iso2time(iso_car_50_osrm_Marseille$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000))

save_DVF(tcar_osrm_pop15_Marseille)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
