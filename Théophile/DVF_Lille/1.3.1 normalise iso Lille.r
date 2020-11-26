source("access.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

iso_transit_50_r5_Lille <- load_DVF("iso_transit_50_r5_Lille")

norm_tr_Lille <- iso_transit_50_r5_Lille$bricks$EMP09/iso_transit_50_r5_Lille$vars$EMP09
isotimes_Lille <- names(norm_tr_Lille) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Lille <- iso2time(iso_transit_50_r5_Lille$EMP09, seuils=c(25000,50000,75000, 100000, 125000,150000,175000,200000,225000,250000))

save_DVF(ttr_r5_emp09_Lille)

# isotime transport en commun pop15
norm_tr_Lille <- iso_transit_50_r5_Lille$bricks$P15_POP/iso_transit_50_r5_Lille$vars$P15_POP
isotimes <- names(norm_tr_Lille) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_pop15_Lille <- iso2time(iso_transit_50_r5_Lille$P15_POP, seuils=c(50000,100000,150000,200000,250000,300000,400000,500000))
save_DVF(ttr_r5_pop15_Lille)

# car ------------------

iso_car_50_osrm_Lille <- load_DVF("iso_car_50_osrm_Lille")

norm_car <- iso_car_50_osrm_Lille$bricks$EMP09/iso_car_50_osrm_Lille$vars$EMP09
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Lille <- iso2time(iso_car_50_osrm_Lille$EMP09, seuils=c(50000, 100000,150000,200000,250000,300000,350000,400000,500000))

save_DVF(tcar_osrm_emp09_Lille)

# car pop15
norm_car <- iso_car_50_osrm_Lille$bricks$P15_POP/iso_car_50_osrm_Lille$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_pop15_Lille <- iso2time(iso_car_50_osrm_Lille$EMP09, seuils=c(50000,100000,150000,200000,250000,300000,400000,500000))

save_DVF(tcar_osrm_pop15_Lille)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
