source("access.r")
library("ggspatial")
library("tidytransit")

idfm <- read_gtfs("{localdata}/IDFM 2020/gtfs STIF 2020 12.zip" %>% glue)
rerd <- get_line(route_id = "800:D", idfm)
colD <- str_c("#", rerd$route_color[[1]])
dline <- tm_shape(rerd)+
  tm_lines(col=colD, alpha=0.5, size=0.1, lwd=3)+
  tm_dots(col="black", shape=21, size=0.5, border.col="white", border.lwd = 2)
uu851 <- load_DVF("uu851")
c200 <- load_DVF("c200")
setDT(c200)
setkey(c200, "idINS200")
c200 <- c200[,.(idINS200, Ind, dep)]
seuils <- c(50,1000)*1000

# cartes des transports
rers <- map(idfm$routes %>% filter(route_type==2, route_id!="800:TER") %>% pull(route_id), ~get_line(.x, idfm))
rers_maps <- map(rers, ~{
  col <- str_c("#", .x$route_color[[1]])
  tm_shape(.x)+
    tm_lines(col=col, alpha=0.5, size=0.05, lwd=1)
})
                 
idf <- load_DVF("iris15") %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- load_DVF("c200") %>% filter(st_within(., idf, sparse=FALSE))
dens_idf <- c200[ dep %in% depIdf, ] [, Ind:=Ind/0.04] %>% dt2r(resolution = 200)
dens_rer <- tm_shape(idf)+tm_fill(col="grey95")+tm_shape(dens_idf)+tm_raster(style="kmeans", palette=green2gray, alpha=1, title="Resident/km²", n=8)+
  rers_maps[[1]]+rers_maps[[2]]+rers_maps[[3]]+rers_maps[[4]]+rers_maps[[5]]+rers_maps[[6]]+
  rers_maps[[7]]+rers_maps[[8]]+rers_maps[[9]]+rers_maps[[10]]+rers_maps[[11]]+rers_maps[[12]]+rers_maps[[13]]+
  tm_shape(idf)+tm_borders()
tmap_mode("plot")
tmap_save(dens_rer, "Densité et rer.svg", width = 24, height = 16, units="cm")

# réforme du RER D -------------------

t1 <- iso2time(lload_DVF("tr_r5_Dav")$EMP09, seuils)
t2 <- iso2time(lload_DVF("tr_r5_Dap")$EMP09, seuils)
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette="RdBu")+dline+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette="RdBu")+dline+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/rerd map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/rerd map 1M")

tt <- rbind(r2dt(t1, 200)[, rerD:="avant"], r2dt(t2, 200)[, rerD:="après"])
tt <- c200[tt]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, fill=rerD, col=rerD))+
  geom_massity(alpha=0.25)+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/rerd massity", textratio=1.5)

dd <- disichrone(tt, dist=to50k, mass=Ind, by=rerD)
dd <- dd %>% 
  pivot_wider(id_cols=to50k, names_from = rerD, values_from = Ind) %>% 
  mutate(delta=après-avant) %>% 
  arrange(to50k) %>% 
  mutate(dc = cumsum(delta))
dif <- ggplot()+geom_line(data=dd, aes(x=round(to50k), y=dc))
dif <- ggplot(tt)+geom_histogram( aes(x=to50k, y=..density.., weight=Ind), binwidth=1)

graph2svg(dif,"{DVFdata}/presentation/GTFS/diff rerd massity" , textratio=1.5)

# changement des bus -------------------

seuils <- c(50,1000)*1000
t1 <- iso2time(load_DVF("tr_r5_avant_bus_50")$EMP09, seuils)
t2 <- iso2time(load_DVF("tr_r5_apres_bus_50")$EMP09, seuils)
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/reforme des bus map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/reforme des bus map 1M")

t1 <- iso2time(load_DVF("tr_r5_avant_bus")$EMP09, seuils)
t2 <- iso2time(load_DVF("tr_r5_apres_bus")$EMP09, seuils)
tt <- rbind(r2dt(t1, 200)[, Bus:="avant"], r2dt(t2, 200)[, Bus:="après"])
tt <- c200[tt]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, fill=Bus, col=Bus))+
  geom_massity(alpha=0.25)+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/reforme des bus massity", textratio=1.5)

seuils <- c(50,1000)*1000
t1 <- iso2time(load_DVF("tr_r5_avant_bus_50")$EMP09, seuils)
t2 <- iso2time(load_DVF("tr_r5_apres_bus_50")$EMP09, seuils)
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/reforme des bus map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/reforme des bus map 1M")

# r5 5% versus medianne -------------------------

# ddta <- iso2time(load_DVF("tr_r5_2020_median")$EMP09, seuils)-iso2time(load_DVF("tr_r5_2020")$EMP09, seuils)
# ddtb <- iso2time(lload_DVF("tr_r5_2020_median")$EMP09, seuils)-iso2time(lload_DVF("tr_r5_2020")$EMP09, seuils)
# tm_shape(ddta-ddtb)+tm_raster(style="cont", palette="RdBu")

t1 <- iso2time(lload_DVF("tr_r5_2020_median")$EMP09, seuils)
t2 <- iso2time(lload_DVF("tr_r5_2020")$EMP09, seuils)
tt <- rbind(r2dt(t1, 200)[, r5:="médianne"], r2dt(t2, 200)[, r5:="5%"])
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/medianne versus 5% map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/medianne versus 5% map 1M")
tt <- c200[tt]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, fill=r5, col=r5))+
  geom_massity(alpha=0.25)+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/medianne versus 5% massity", textratio=1.5)

tt <- c200[r2dt(t1-t2, 200)]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.25, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  xlab("temps (minutes)")+ylab("Population/m")+
  facet_wrap(~seuil)
graph2svg(idf, "{DVFdata}/presentation/GTFS/medianne versus 5% delta massity", textratio=2)

# r5r draws -------------------------

t1 <- iso2time(load_DVF("tr_r5_2020_median")$EMP09, seuils)
t2 <- iso2time(load_DVF("tr_r5_2020")$EMP09, seuils)
tt <- rbind(r2dt(t1, 200)[, r5:="médianne"], r2dt(t2, 200)[, r5:="5%"])
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/medianne versus 5% map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/medianne versus 5% map 1M")
tt <- c200[tt]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, fill=r5, col=r5))+
  geom_massity(alpha=0.25)+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/medianne versus 5% massity", textratio=1.5)

tt <- c200[r2dt(t1-t2, 200)]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.25, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  xlab("temps (minutes)")+ylab("Population/m")+
  facet_wrap(~seuil)
graph2svg(idf, "{DVFdata}/presentation/GTFS/median versus 5% massity", textratio=2)

# r5r population carreau versus iris -------------------------

tb1 <- load_DVF("tr_r5_2020")$P15_POP$iso45m
tb2 <- load_DVF("popc200_2020")$pop$iso45m
t1 <- iso2time(load_DVF("tr_r5_2020")$P15_POP, seuils)
t2 <- iso2time(load_DVF("popc200_2020")$pop, seuils)
tt <- rbind(r2dt(t1, 200)[, pop:="iris"], r2dt(t2, 200)[, pop:="c200"])
mm45m <- uu851$fdc+tm_shape((tb1-tb2))+tm_raster(style="cont", palette="RdBu")+uu851$hdc
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
graph2svg(mm45m, "{DVFdata}/presentation/GTFS/c200 versus iris map 45m")
graph2svg(mm50, "{DVFdata}/presentation/GTFS/c200 versus iris map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/c200 versus iris map 1M")

# r5 différentes dates -----------------------

t2020 <- iso2time(load_DVF("tr_r5_2020")$EMP09, seuils)
t2019 <- iso2time(load_DVF("tr_r5_2019")$EMP09, seuils)
t2018 <- iso2time(load_DVF("tr_r5_2018")$EMP09, seuils)
t2017 <- iso2time(load_DVF("tr_r5_2017")$EMP09, seuils)
b1 <- brick(t2019$to50k-t2020$to50k, t2018$to50k-t2020$to50k, t2017$to50k-t2020$to50k)
b2 <- brick(t2019$to1M-t2020$to1M, t2018$to1M-t2020$to1M, t2017$to1M-t2020$to1M)
names(b1) <- c("d(2019-2020)","d(2018-2020)","d(2017-2020)")
names(b2) <- names(b1)
mm50 <- uu851$fdc+tm_shape(b1)+tm_raster(style="cont", palette="RdBu")+uu851$hdc
mm1000 <- uu851$fdc+tm_shape(b2)+tm_raster(style="cont", palette="RdBu")+uu851$hdc

graph2svg(mm50, "{DVFdata}/presentation/GTFS/differentes annee0 map 50k", textratio=2)
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/differentes annees map 1m", textratio=2)

tt <- c200[r2dt(b1$y2017, 200)]
idf <- ggplot(tt, aes(x=y2017, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.25, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/2017-2020 massity", textratio=2)

# r5 transit, bus, rail --------------------------

tbus <- iso2time(load_DVF("bus_r5_2020_200")$EMP09, seuils)
trail <- iso2time(load_DVF("rail_r5_2020_200")$EMP09, seuils)
ttr <- iso2time(load_DVF("tr_r5_2020")$EMP09, seuils)
mm50 <- uu851$fdc+tm_shape((trail-ttr)$to50k)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((trail-ttr)$to1M)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/sans les bus map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/sans les bus map 1M")
tt <- c200[r2dt(trail-ttr, 200)]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.25, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/sans les bus massity", textratio=1.5)

# GPE -----------------------

t1 <- iso2time(load_DVF("isoIDF50/isotrr550")$EMP09, seuils)
t2 <- iso2time(load_DVF("isoIDF50/iso_GPE_50_r5")$EMP09, seuils)
tt <- rbind(r2dt(t1, 200)[, GPE:="sans"], r2dt(t2, 200)[, GPE:="avec"])
mm50 <- uu851$fdc+tm_shape((t1-t2)$to50k)+tm_raster(style="cont", palette=red2gray)+uu851$hdc
mm1000 <- uu851$fdc+tm_shape((t1-t2)$to1M)+tm_raster(style="cont", palette=red2gray)+dline+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/GPE map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/GPE map 1M")
tt <- c200[tt]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, fill=GPE, col=GPE))+
  geom_massity(alpha=0.25)+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/GPE massity", textratio=1.5)

# Cartes de référence -------------------------

t1 <- iso2time(load_DVF("isoIDF50/isotrr550")$EMP09, seuils)
tt <- r2dt(t1, 200)
mm50 <- uu851$fdc+tm_shape(t1$to50k)+tm_raster(style="cont", palette=heatbg)+uu851$hdc
mm1000 <- uu851$fdc+tm_shape(t1$to1M)+tm_raster(style="cont", palette=heatbg)+uu851$hdc
graph2svg(mm50, "{DVFdata}/presentation/GTFS/REF map 50k")
graph2svg(mm1000, "{DVFdata}/presentation/GTFS/REF map 1M")
tt <- c200[tt]
ttm <- tt %>% melt(measure.vars=names(.) %>% keep(~str_detect(.x,"to")), variable.name="seuil", value.name="temps")
ttm[, seuil:=if2si2(seuil)]
idf <- ggplot(ttm, aes(x=temps, y=after_stat(mass), mass=Ind, col=dep, fill=dep))+
  geom_massity(alpha=0.25, position="stack")+
  scale_y_continuous(labels=uf2si2)+
  facet_wrap(~seuil, labeller=function(x) f2si2df(x, "emplois"))+
  xlab("temps (minutes)")+ylab("Population/m")
graph2svg(idf, "{DVFdata}/presentation/GTFS/REF massity", textratio=2)
