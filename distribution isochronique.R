source("access.r")

c200 <- load_DVF("c200") %>% st_drop_geometry()

ttrr5_emp09_200 <- load_DVF("iso200/ttrr5_emp09_200")
idf_dt <- r2dt(ttrr5_emp09_200)
idf_dt <- merge(idf_dt, c200[, c("idINS200", "Ind")] , by.x="idINS", by.y="idINS200")
Nidf <- idf_dt[, sum(Ind)] # 10M normal !
idf <- ggplot(idf_dt, aes(x=to25k, y=..density..*Nidf, weight=Ind))+geom_density()

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
data <- data[, ind_grp:=sum(Ind), by=ville] [, w:= Ind/sum(Ind)]
nind <- data[, sum(Ind)]
gg <- map(unique(data$ville), ~geom_density(data=data[ville==.x], aes(x=dist, weight=Ind, y=..density..*(data[ville==.x,sum(Ind)]), col=ville, fill=ville),alpha=0.5))
ggplot()+gg
ggplot(data, aes(x=dist))+stat_density(aes(y=..density.., fill=ville, weight=w), position="stack", trim=FALSE)

data <- na.omit(data, "dist")
xx <- map(unique(data$ville), ~
            {
              dd <- data[ville==.x]
              density(dd$dist, weight=dd$Ind, from=min(data$dist), to=max(data$dist))
              })
sum(xx$y)
sum(head(xx$y, -1)*(head(xx$x, -1) -tail(xx$x, -1)))

rrr <- iso2time(rr$c, c(1,5,100))
petj_dt <- r2dt(rrr, 200)
petj_dt <- merge(petj_dt, c200[, c("idINS200", "Ind")] , by="idINS200")
idf <- ggplot(petj_dt, aes(x=to5, weight=Ind))+stat_ecdf()
