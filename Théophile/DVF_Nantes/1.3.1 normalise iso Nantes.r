source("access.r")


iso_transit_50_r5_Nantes <- load_DVF("iso_transit_50_r5_Nantes")

# r5 emp09

norm_tr_Nantes <- iso_transit_50_r5_Nantes$bricks$EMP09/iso_transit_50_r5_Nantes$vars$EMP09
isotimes_Nantes <- names(norm_tr_Nantes) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_emp09_Nantes <- iso2time(iso_transit_50_r5_Nantes$EMP09, seuils=c(25000,50000,75000, 100000,125000,150000,1750000,200000,225000,250000))

save_DVF(ttr_r5_emp09_Nantes)


# r5 pop
norm_tr <- iso_transit_50_r5_Nantes$bricks$P15_POP/iso_transit_50_r5_Nantes$vars$P15_POP
isotimes <- names(norm_tr) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_r5_pop15_Nantes <- iso2time(iso_transit_50_r5_Nantes$P15_POP, seuils=c(50000,100000,150000,200000,250000))

save_DVF(ttr_r5_pop15_Nantes)

# car ------------------ EMP09

iso_car_50_osrm_Nantes <- load_DVF("iso_car_50_osrm_Nantes")

norm_car <- iso_car_50_osrm_Nantes$bricks$EMP09/iso_car_50_osrm_Nantes$vars$EMP09
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_emp09_Nantes <- iso2time(iso_car_50_osrm_Nantes$EMP09, seuils=c(50000, 100000,150000,200000,250000))

save_DVF(tcar_osrm_emp09_Nantes)

# car pop

norm_car <- iso_car_50_osrm_Nantes$bricks$P15_POP/iso_car_50_osrm_Nantes$vars$P15_POP
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_osrm_pop15_Nantes <- iso2time(iso_car_50_osrm_Nantes$EMP09, seuils=c(50000,100000,150000,200000,250000))

save_DVF(tcar_osrm_pop15_Nantes)

# quelques graphes

e <- tm_shape(ttr_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
p <- tm_shape(ttr_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(green2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent transit")

e <- tm_shape(tcar_emp09[["emp10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
p <- tm_shape(tcar_pop15[["pop10"]])+tm_raster(style="cont", palette=rev(blue2gray))+uu851.hdc
ep <- tmap_arrange(e,p)
graph2svg(ep, file="temps accès 10 pour cent car")
