source("dvf.r")

fisoinv <- function(x, isotimes, seuil=0.2)
{
  l <- length(isotimes)
  if(x[l]>=seuil) approx(y=isotimes, x=x %>% as.vector , xout=seuil)$y
  else NA
}

iso_transit_50 <- load_DVF("iso_transit_50")

norm_tr <- iso_transit_50$bricks$EMP09/iso_transit_50$vars$EMP09
isotimes <- names(norm_tr) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_emp09_10 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.1))
ttr_emp09_15 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.15))
ttr_emp09_20 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.2))
ttr_emp09_25 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.25))
ttr_emp09_30 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.3))

ttr_emp09 <- brick(list(ttr_emp09_10, ttr_emp09_15, ttr_emp09_20, ttr_emp09_25, ttr_emp09_30))
names(ttr_emp09) <- c("emp10", "emp15", "emp20", "emp25", "emp30")

save_DVF(ttr_emp09)

norm_tr <- iso_transit_50$bricks$P15_POP/iso_transit_50$vars$P15_POP
isotimes <- names(norm_tr) %>% str_extract("[:digit:]+") %>% as.numeric()

ttr_pop15_10 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.1))
ttr_pop15_15 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.15))
ttr_pop15_20 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.2))
ttr_pop15_25 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.25))
ttr_pop15_30 <- calc(norm_tr, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.3))

ttr_pop15 <- brick(list(ttr_pop15_10, ttr_pop15_15, ttr_pop15_20, ttr_pop15_25, ttr_pop15_30))
names(ttr_pop15) <- c("pop10", "pop15", "pop20", "pop25", "pop30")

save_DVF(ttr_pop15)

# car ------------------

iso_car_50 <- load_DVF("iso_car_50")

norm_car <- iso_car_50$bricks$EMP09/iso_car_50$vars$EMP09
isotimes <- names(norm_car) %>% str_extract("[:digit:]+") %>% as.numeric()

tcar_emp09_10 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.1))
tcar_emp09_15 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.15))
tcar_emp09_20 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.2))
tcar_emp09_25 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.25))
tcar_emp09_30 <- calc(norm_car, fun= function(x) fisoinv(x, isotimes=isotimes, seuil=0.3))

tcar_emp09 <- brick(list(tcar_emp09_10, tcar_emp09_15, tcar_emp09_20, tcar_emp09_25, tcar_emp09_30))
names(tcar_emp09) <- c("emp10", "emp15", "emp20", "emp25", "emp30")

save_DVF(tcar_emp09)

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
