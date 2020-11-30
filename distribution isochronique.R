source("access.r")

c200 <- load_DVF("c200") %>% st_drop_geometry()

# transport en commun
ttrr5_emp09 <- load_DVF("ttrr5_emp09")
idf_dt <- r2dt(ttrr5_emp09, 200)
idf_dt <- merge(idf_dt, c200[, c("idINS200", "Ind", "dep")] , by="idINS200")
seuils <- names(idf_dt) %>% keep(~str_detect(.x,"to"))
idf_dtm <- idf_dt %>% melt(measure.vars=seuils, variable.name="seuil", value.name="temps")
idf_dtm[, seuil:=if2si2(seuil)]
idf <- ggplot(idf_dtm, aes(x=temps, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.5, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))

# voiture
tcarosrm_emp09 <- load_DVF("tcarosrm_emp09")
idfc_dt <- r2dt(tcarosrm_emp09, 200)
idfc_dt <- merge(idfc_dt, c200[, c("idINS200", "Ind", "dep")] , by="idINS200")
seuils <- names(idfc_dt) %>% keep(~str_detect(.x,"to"))
idfc_dtm <- idfc_dt %>% melt(measure.vars=seuils, variable.name="seuil", value.name="temps")
idfc_dtm[, seuil:=if2si2(seuil)]
idfc <- ggplot(idfc_dtm[seuil>100000,], aes(x=temps, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.5, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))


# lyon et paris

ttrr5_emp09_lyon <- load_DVF("ttr_r5_emp09_Lyon")
lyon_dt <- r2dt(ttrr5_emp09_lyon, 200)
lyon_dt <- merge(lyon_dt, c200[, c("idINS200", "Ind")], by.x="idINS200", by.y="idINS200")  # ajoute la colonne Ind aux données
setnames(lyon_dt, "idINS200", "idINS")
Nlyon <- lyon_dt[, sum(Ind)] # total des habitants, 21858 étrange ?
# méthode 1, 2 graphs
idf <- ggplot(idf_dt, aes(x=to25k, y=..density..*Nidf, weight=Ind))+geom_density()
lyon <- ggplot(lyon_dt, aes(x=to25k, y=..density..*Nlyon, weight=Ind))+geom_density()
idf+lyon  # affiche les 2 graphiques l'un à côté de l'autre

#méthode 2; 1 seul graphe avec la ville comme groupe
data <- rbind(idf_dt[, .(dist=to25k,Ind, ville="paris")], lyon_dt[, .(dist=to25k,Ind, ville="lyon")])
data <- data[, ind_grp:=sum(Ind), by=ville]
nind <- data[, sum(Ind)]
gg <- map(unique(data$ville), ~geom_density(data=data[ville==.x], aes(x=dist, weight=Ind, y=..density..*(data[ville==.x,sum(Ind)]), col=ville, fill=ville),alpha=0.5))
ggplot()+gg
