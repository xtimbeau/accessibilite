source("access.r")

c200 <- load_DVF("c200") %>% st_drop_geometry() %>% as.data.table()

function(raster, res=NULL, fun=mean)
{
  base_res <- max(raster::res(raster))
  vars <- names(raster)
  xy <- raster::coordinates(raster)
  idINS <- idINS3035(xy[,1], xy[,2], resolution=base_res)
  dt <- raster %>% as.data.frame %>% as.data.table
  dt <- dt[, `:=`(x=xy[,1], y=xy[,2], idINS=idINS)]
  dt <- na.omit(melt(dt, measure.vars=vars), "value")
  dt <- dcast(dt, x+y+idINS~variable, value.var="value")
  setnames(dt, "idINS",str_c("idINS", base_res))
  if(!is.null(res))
  {
    id <- str_c("idINS",res)
    dt[, (str_c("idINS",res)):=idINS3035(x,y,res)]
    dt <- dt[, lapply(.SD, fun, na.rm=TRUE), by=c(id), .SDcols=vars]
  }
  dt
}

# méthode 2, en utilisant geom_massity comme geom_density, mais avec un calcul en plus: mass et cummass
# on définit les données et les distances à grapher (dans l'ordre que l'on souhaite)
idf_dt <- r2dt(load_DVF("iso200/ttrr5_emp09_200"), res=200)
idf_dt <- merge(idf_dt, c200[, .(idINS200, Ind)], by="idINS200")

lyon_dt <- r2dt(load_DVF("ttr_r5_emp09_Lyon"), res=200) # le 200 indique qu'on recalcule le raster en résolution 200
lyon_dt <- merge(lyon_dt, c200[, .(idINS200, Ind)], by="idINS200")

bordeaux_dt <- r2dt(load_DVF("ttr_r5_emp09_Bordeaux"), res=200)
bordeaux_dt <- merge(bordeaux_dt, c200[, .(idINS200, Ind)], by="idINS200")

marseille_dt <- r2dt(load_DVF("ttr_r5_emp09_Marseille"), res=200)
marseille_dt <- merge(marseille_dt, c200[, .(idINS200, Ind)], by="idINS200")

toulouse_dt <- r2dt(load_DVF("ttr_r5_emp09_Toulouse"), res=200)
toulouse_dt <- merge(toulouse_dt, c200[, .(idINS200, Ind)], by="idINS200")

lille_dt <- r2dt(load_DVF("ttr_r5_emp09_Lille"), res=200)
lille_dt <- merge(lille_dt, c200[, .(idINS200, Ind)], by="idINS200")

nantes_dt <- r2dt(load_DVF("ttr_r5_emp09_Nantes"), res=200)
nantes_dt <- merge(nantes_dt, c200[, .(idINS200, Ind)], by="idINS200")

distances <-c("to25k", "to50k","to100k","to150k","to200k","to250k")

# melt transforme idf_dt en format long
idf_dtm <- idf_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
lyon_dtm <- lyon_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
bordeaux_dtm <- bordeaux_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
marseille_dtm <- marseille_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
toulouse_dtm <- toulouse_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
lille_dtm <- lille_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
nantes_dtm <- nantes_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
# on peut utiliser seuil comme couleur
ggplot(idf_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)
ggplot(lyon_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)
# mais aussi comme facette
ggplot(idf_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)
ggplot(lyon_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)
# mais aussi comme facette et cumulé
ggplot(idf_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)
ggplot(lyon_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)
# ou en mélangeant lyon et paris
data <- rbind(idf_dtm[, ville:="paris"], lyon_dtm[, ville:="lyon"])
ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)
# on peut vouloir des fonctions en %
ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass)/after_stat(total_mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)
# on peut vouloir des fonctions en % et en cumul
ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass)/after_stat(total_mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)