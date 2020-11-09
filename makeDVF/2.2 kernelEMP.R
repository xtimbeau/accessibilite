# On utilise ici des données 2009 d'emploi au niveau IRIS pour faire un kernel
# le but du kernel est d'avoir une pondératio de l'emploi avec une assez grande bande passante pour indiquer la densité d'emploi
# a priori on utilise la version de l'INSEE, mais celle du package ADE est assez intéressante

# on lit le fichier emploi par IRIS en 2009
# on lit également les données recensement publiées depuis, au niveau communal

source("DVF.r")

load_DVF("iris15")
load_DVF("com15")

ifr <- st_read("{DVFdata}/Parcelles/fr/CONTOURS-IRIS.shp" %>% glue, stringsAsFactors=FALSE)
ifr <- st_transform(ifr, 3035)
setDT(ifr)
ifr[, ':='(IRIS=NULL, TYP_IRIS=NULL)]
setnames(ifr, "CODE_IRIS", "iris")
setkey(ifr, iris)

ifr <- merge(ifr, iris15, all.x=TRUE)
ifr[, EMP09:= as.numeric(EMP09)]
ifr[is.na(EMP09), EMP09:=0]
ifr[EMP09==-99999, EMP09:=0]

load_DVF("cidf")
setDT(cidf)
setkey(cidf, insee)
cidf <- merge(cidf, com15, by.x="insee", by.y="CODGEO")

iidf.sf <- st_as_sf(ifr[DEP%in%depIdf]) %>% st_make_valid %>% st_transform(3035)
iidf.sf <- mutate(iidf.sf, surface=st_area(iidf.sf) %>% as.numeric)
save_DVF(iidf.sf)
cidf.sf <- st_as_sf(cidf) %>% st_make_valid %>% st_transform(3035)
cidf.sf <- mutate(cidf.sf, surface=st_area(cidf.sf) %>% as.numeric)
cidf.sf<-st_set_agr(cidf.sf, c(pop15="constant",emp15="constant", surface="constant", name="identity", LIBGEO="identity", DEP="identity" ))
nei <- st_touches(cidf.sf)
comIdf <- unique(cidf.sf$insee)

load_DVF("g200idf")
g200idf %<>% st_set_agr( c(rowid="identity",idINSPIRE="identity",id_carr_1k="identity", iris="identity", com="identity", dep="identity" )) %>%
  st_transform(3035)
load_DVF("c200idf")

i851.sf <- st_as_sf(ifr[UU2010=="00851"]) %>% st_make_valid

bbPC <- st_bbox(st_union(iidf.sf[iidf.sf$DEP %in% c("75", "92", "93", "94"),]))
bb851 <- st_bbox(st_union(st_as_sf(iidf.sf[iidf.sf$UU2010=="00851", ])))

# on calcule la masse totale d'emploi et de population

tot.com <- cidf.sf %>% as.data.frame %>% summarize(pop15=sum(pop15), emp15=sum(emp15))
tot.iris <- iidf.sf %>% as.data.frame %>% summarize(pop15=sum(P15_POP), emp09=sum(EMP09))

cidf.sf <- st_transform(cidf.sf, 3035)
iidf.sf <- st_transform(iidf.sf, 3035)
cidf.sf <- mutate(cidf.sf,
                  emp15n=emp15,
                  pop15n=pop15)

iidf.sf <- mutate(iidf.sf,
                  emp09n=EMP09,
                  emp09d=EMP09/st_area(iidf.sf),
                  pop15n=P15_POP,
                  dpop15=P15_POP/st_area(iidf.sf))

iidf.sf.maxd <- iidf.sf %>% as_tibble() %>% ungroup %>% filter(dpop15==max(dpop15))

pas <- 200 #carreau de 200m de cote
rayon <- 2500 #bande passante de 5000m
rayons <- c(500, 1000, 1500, 2000, 2500, 5000, 7500, 10000)
breaks <- seq(0, 50000, length.out=6)
l10break <- seq(0, 5, length.out=6)

emp15ks.names<-str_c("emp15k", round(rayons/1000, 1))
emp09ks.names<-str_c("emp09k", round(rayons/1000, 1))
emp09kgs.names<-str_c("emp09kgs", round(rayons/1000, 1))
pop15ks.names<-str_c("pop15k", round(rayons/1000, 1))
pop15kgs.names<-str_c("pop15kgs", round(rayons/1000, 1))

my_palette=sequential_hcl(n = 10, h = c(140, 80), c = c(50, NA, 10), l = c(40, 97), power = c(0.7, 1.8), rev=TRUE)

cidf.sf <- st_transform(cidf.sf, 3035)
uu851 <- st_union(filter(iidf.sf, UU2010=="00851")) %>% st_sf
g200idf <- arrange(g200idf, idINSPIRE)
didf.sf <- cidf.sf %>% as.data.frame %>% st_as_sf %>% group_by(DEP) %>% summarise(pop15=sum(pop15n))

# # sampling ----------------------------------------------------------------
#
#
# #on utilise st_sample
#
#
# nbsp <- 25
# sizes.com <- rep.int(nbsp, nrow(cidf.sf))
# sizes.iris <- rep.int(nbsp, nrow(iidf.sf))
#
# sp.com <- st_sample(cidf.sf, sizes.com) %>% st_sf %>% st_as_sf
# sp.com$id <- 1:nrow(sp.com)
#
# sp.iris <- st_sample(iidf.sf, sizes.iris) %>% st_sf %>% st_as_sf
# sp.iris$id <- 1:nrow(sp.iris)
#
# sp.com <- st_join(sp.com, cidf.sf[, c("pop15n", "emp15n", "insee", "surface")])
# sp.iris <- st_join(sp.iris, iidf.sf[, c("pop15n", "emp09n", "iris", "surface")])
#
# cd.iris <- st_coordinates(st_centroid(iidf.sf))
# cd.com <- st_coordinates(st_centroid(cidf.sf))
# cd.com.sp <- st_coordinates(sp.com)
# cd.iris.sp <- st_coordinates(sp.iris)
#
# data.iris <- data.frame(x=cd.iris[,1], y=cd.iris[,2],
#                    emp09=as.numeric(iidf.sf$emp09n),
#                    pop15=as.numeric(iidf.sf$pop15n),
#                    n=rep.int(1,nrow(iidf.sf)))
#
# data.com <- data.frame(x=cd.com[,1], y=cd.com[,2],
#                        emp15=as.numeric(cidf.sf$emp15n),
#                        pop15=as.numeric(cidf.sf$pop15n),
#                        n=rep.int(1,nrow(cidf.sf)))
#
# data.com.sp <- data.frame(x=cd.com.sp[,1], y=cd.com.sp[,2],
#                       emp15=as.numeric(sp.com$emp15n/nbsp),
#                       pop15=as.numeric(sp.com$pop15n/nbsp),
#                       n=rep.int(1,nrow(sp.com)))
#
# data.iris.sp <- data.frame(x=cd.iris.sp[,1], y=cd.iris.sp[,2],
#                           emp09=as.numeric(sp.iris$emp09n/nbsp),
#                           pop15=as.numeric(sp.iris$pop15n/nbsp),
#                           n=rep.int(1,nrow(sp.iris)))
#
# # Interpolation é partir du sampling (k points sur chaque featur --------
#
# pas <- 200 #carreau de 100m de cote
# rayons <- c(500, 1000, 1500, 2000, 2500, 5000, 7500, 10000)
#
# kernels.c.sp<-map(rayons,function(x) kernelSmoothing(dfObservations = data.com.sp, iCellSize = pas,
#                                  iBandwidth = x, sEPSG="3035"))
#
# kernels.i.sp<-map(rayons,function(x) kernelSmoothing(dfObservations = data.iris.sp, iCellSize = pas,
#                                                       iBandwidth = x, sEPSG="3035"))
#
# temp_bbox <- st_bbox(kernels.c.sp[[1]])
#
# Raster <- raster(res=pas,
#                  xmn=temp_bbox$xmin, xmx=temp_bbox$xmax,
#                  ymn=temp_bbox$ymin, ymx=temp_bbox$ymax,
#                  crs=CRS('+init=EPSG:3035'))
#
# temp.emp <- map(kernels.c.sp, function(df) rasterize(as.matrix(as.data.frame(df)[, c("x","y")]), Raster, df$emp15/(pas/1000)^2))
# temp.pop <- map(kernels.i.sp, function(df) rasterize(as.matrix(as.data.frame(df)[, c("x","y")]), Raster, df$pop15/(pas/1000)^2))
#
# maps.emp <- map2(temp.emp,emp15ks.names, ~tm_shape(.x)+
#                    tm_raster(style="cont", palette="-Spectral", title=.y, midpoint = NA)+
#                    tm_legend(outside=TRUE)+
#                    tm_shape(didf.sf)+tm_borders(lwd=0.01))
# maps.pop <- map2(temp.pop,pop15ks.names, ~tm_shape(.x)+
#                    tm_raster(style="cont", palette="-Spectral", title=.y, midpoint = NA)+
#                    tm_legend(outside=TRUE)+
#                    tm_shape(didf.sf)+tm_borders(lwd=0.01))
#
# tmap_arrange(maps.emp)
# tmap_arrange(maps.pop)
# tmap_arrange(maps.pop[[1]], maps.emp[[5]])
# emp15ks <- map(temp.emp, ~raster::extract(., t_idf.co) )
# pop15ks <- map(temp.pop, ~raster::extract(., t_idf.co) )
#


# # Interpolation é partir d'une grille 200*200m --------------------------
# on gridife sur une grille et on rÃ©parti les observations (emploi et population) sur cette grille
# on prend comme grille le carreau 200 de l'insee, g200idf

plan("multiprocess", workers=availableCores())

# au niveau com


gridos <- mc_interpolate_aw(cidf.sf %>% select(emp15n), g200idf)
gridos <- arrange(gridos, idINSPIRE)

xy <- st_coordinates(st_centroid(gridos$geometry))
data.gcom <- data.frame(x=xy[,1], y=xy[,2],
                     emp15n=gridos$emp15n,
                     n=rep.int(1,nrow(xy)))

kernel.gcom <- map(rayons, ~btb::kernelSmoothing(dfObservations = data.gcom, iCellSize = pas,
                               iBandwidth = ., sEPSG="3035"))

emps.gcom <- future_map(kernel.gcom, ~crop(mask(xt_as_raster(., .$emp15n/(pas/1000)^2), uu851), xt_as_extent(uu851)))

# empmap.gcom<-map2(emps.gcom,emp15ks.names,
#                  ~tm_shape(.x)+
#                   tm_raster(style="log10",palette=my_palette, title=.y, midpoint=NA, breaks=l10break)+
#                   tm_legend(outside=TRUE)+
#                    tm_layout(legend.width=100)+
#                   tm_shape(st_intersection(didf.sf, uu851))+tm_borders(lwd=0.1))
# popmap.gcom<-map2(pops.gcom, pop15ks.names,
#                  ~tm_shape(.x)+
#                   tm_raster(style="log10", palette=my_palette, title=.y, midpoint=NA, breaks=l10break)+
#                   tm_legend(outside=TRUE)+
#                   tm_shape(st_intersection(didf.sf, uu851))+tm_borders(lwd=0.1))
#
# tmap_arrange(empmap.gcom)
# tmap_arrange(popmap.gcom)
#
# tmap_arrange(empmap.gcom[[8]],popmap.gcom[[8]])
# tmap_arrange(empmap.gcom[[1]],popmap.gcom[[1]])

# au niveau IRIS

gridiris <- mc_interpolate_aw(iidf.sf[, c("emp09n")], g200idf)
gridiris <- arrange(gridiris, idINSPIRE)
xy <- st_coordinates(st_centroid(gridiris$geometry))
data.giris <- data.frame(x=xy[,1], y=xy[,2],
                     emp09n=gridiris$emp09n,
                     n=rep.int(1,nrow(xy)))

kernel.giris <- map(rayons, ~btb::kernelSmoothing(dfObservations = data.giris, iCellSize = pas,
                                                  iBandwidth = ., sEPSG="3035"))

emps.giris <- map(kernel.giris, ~crop(mask(xt_as_raster(., .$emp09n/(pas/1000)^2), uu851), xt_as_extent(uu851)))

# empmap.giris<-map2(emps.giris,emp09ks.names,
#                  ~tm_shape(.x)+
#                    tm_raster(style="cont",palette=my_palette, title=.y, midpoint=NA, breaks = breaks)+
#                    tm_legend(show=FALSE)+
#                    tm_shape(st_intersection(didf.sf, uu851))+tm_borders(lwd=0.1))
# popmap.giris<-map2(pops.giris, pop15ks.names,
#                  ~tm_shape(.x)+
#                    tm_raster(style="cont", palette=my_palette, title=.y, midpoint=NA)+
#                    tm_legend(show=FALSE)+
#                    tm_shape(st_intersection(didf.sf, uu851))+tm_borders(lwd=0.1))
#
# tmap_arrange(empmap.giris)
# tmap_arrange(popmap.giris)
#
# tmap_arrange(empmap.giris[[8]],popmap.giris[[8]])

# # kernel é partir de la grille 200 --------------------------------------

c200idf <- st_transform(c200idf, 3035)
xy <- st_coordinates(st_centroid(c200idf$geometry))
data.c200 <- data.frame(x=xy[,1], y=xy[,2],
                       pop=c200idf$Ind)
pas <- 200 #carreau de 200m de cote
rayon <- 2500 #bande passante de 5000m

kernel.c200 <- map(rayons, ~btb::kernelSmoothing(dfObservations = data.c200, iCellSize = pas,
                               iBandwidth = ., sEPSG="3035"))
pops.gc200 <- future_map(kernel.c200, ~crop(mask(xt_as_raster(., .$pop/(pas/1000)^2), uu851), xt_as_extent(uu851)))

# popmap.gc200<- map2(pops.gc200, pop15ks.names,
#                    ~tm_shape(.x)+
#                      tm_raster(style="cont", palette=my_palette, title=.y, midpoint=NA)+
#                      tm_legend(outside=TRUE)+
#                      tm_shape(st_intersection(didf.sf, uu851))+tm_borders(lwd=0.1))
#
# tmap_arrange(popmap.gc200)
# popmap.gc200[[1]]
# tmap_arrange(popmap.giris[[5]],popmap.gc200[[5]])
# tmap_arrange(empmap.giris[[1]],popmap.giris[[1]])
# tmap_arrange(empmap.giris[[3]],empmap.gcom[[3]])
#
# on enregsitre les différents kernel dans t_idf

t_idf[, (emp15ks.names):=NULL]
t_idf[, (emp09ks.names):=NULL]
t_idf[, (pop15ks.names):=NULL]

t_idf.co <- t_idf %>% st_as_sf %>% st_transform(3035) %>% st_coordinates
dvfplus.co <- dvfplus %>% st_coordinates

Ind.raster <- xt_as_raster(c200idf, field=c200idf$Ind)
# timings <- map_df(cross2(c(1, 4, 8, 16), c(32, 64)), ~{
#   plan("multiprocess", workers=.x[[1]])
#   tic()
#   pop.kgs <- future_map(seq(500, 10000, length.out=.x[[2]]),
#                         ~focal(Ind.raster, focalWeight(Ind.raster, ., "Gauss"), na.rm=TRUE, padValue=0))
#   time <- toc(quiet = TRUE)
#   dtime <- (time$toc - time$tic)
#   tibble(workers=.x[[1]], n=.x[[2]], time=dtime)
# })
#
# timings <- timings %>% mutate(parallel="HT off")
# ggplot(timings , aes(x=workers, y=time, fill=n, group=n))+geom_col(position="dodge")
# save_DVF(timings,"timing HT off")

s1<-map2(emps.gcom, emp15ks.names, function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co)])
s2<-map2(emps.giris, emp09ks.names, function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co)])
s3<-map2(pops.gc200, pop15ks.names, function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co)])

dvfplus %<>% mutate(pop15g=raster::extract(xt_as_raster(sf=c200idf, field=c200idf$Ind/(pas/1000)^2), dvfplus.co))
s1<-map2(emps.gcom, emp15ks.names, function(x,y) dvfplus<<- dvfplus %>% mutate(!!y := raster::extract(!!x, dvfplus.co)))
s2<-map2(emps.giris, emp09ks.names, function(x,y) dvfplus<<- dvfplus %>% mutate(!!y := raster::extract(!!x, dvfplus.co)))
s3<-map2(pops.gc200, pop15ks.names, function(x,y) dvfplus<<- dvfplus %>% mutate(!!y := raster::extract(!!x, dvfplus.co)))

# # kernels gaussien (raster&focal) -----------------------------------------
#
c200idf <- st_transform(c200idf, 3035)
gridiris <- mc_interpolate_aw(iidf.sf %>% as.data.frame() %>% st_as_sf %>% select(emp09n), g200idf)
gridiris <- arrange(gridiris, idINSPIRE)
emp09.raster <- xt_as_raster(gridiris, field=gridiris$emp09n)

emp.kgs <- future_map(rayons, ~focal(emp09.raster, focalWeight(emp09.raster, ., "Gauss"), na.rm=TRUE, padValue=0))
names(emp.kgs)<-emp09kgs.names

Ind.raster <- xt_as_raster(c200idf, field=c200idf$Ind)
pop.kgs <- future_map(rayons, ~focal(Ind.raster, focalWeight(Ind.raster, ., "Gauss"), na.rm=TRUE, padValue=0))
names(pop.kgs)<-pop15kgs.names


# enregistre t_idf avec les kernels gaussiens

walk2(emp.kgs, emp09kgs.names, function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co) ])
walk2(pop.kgs, pop15kgs.names, function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co) ])

walk2(emp.kgs, emp09kgs.names, function(x,y) dvfplus <<- dvfplus %>% mutate(!!y:=raster::extract(!!x, dvfplus.co)))
walk2(pop.kgs, pop15kgs.names, function(x,y) dvfplus <<- dvfplus %>% mutate(!!y:=raster::extract(!!x, dvfplus.co)))


save_DVF(t_idf)
save_DVF(dvfplus)

#
# femp<-emp.kgs[["emp09kgs1.5"]]
# femp.dt <- crop(mask(femp, uu851), xt_as_extent(uu851)) %>% as.data.frame(xy=TRUE)
# my_bbox <-  c(xmin=min(femp.dt$x), xmax=max(femp.dt$x), ymin=min(femp.dt$y), ymax=max(femp.dt$y))
# pop_kemp <- ggplot()+
#   ggtitle("Job density (kerneled)")+
#   theme(plot.title = element_text(size=14))+
#   geom_raster(data=femp.dt,
#               aes(x=x,y=y, fill=layer), na.rm=TRUE)+
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="right",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())+
#   scale_fill_gradientn(colours=terrain.colors(20, rev = TRUE), na.value=NA)+
#   geom_sf(data=st_crop(didf.sf,my_bbox), fill=NA, colour="white", size=0.1 )
# plot_gg(pop_kemp, raytrace = TRUE, sunangle=225, width=5,height=5,scale=250,windowsize=c(2000,1125),
#         zoom = 0.5, theta=10, phi = 30)
# render_snapshot(filename=str_c(drpath, "emp 3D gs.svg"), clear = TRUE)
#
# my_bbox <-  c(xmin=min(fInd.dt$x), xmax=max(fInd.dt$x), ymin=min(fInd.dt$y), ymax=max(fInd.dt$y))
# pop_kpop <- ggplot()+
#   ggtitle("Pop. density (gaussian kernel)")+
#   theme(plot.title = element_text(size=14))+
#   geom_raster(data=fInd.dt,
#               aes(x=x,y=y, fill=layer), na.rm=TRUE)+
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="right",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())+
#   scale_fill_viridis_c(na.value=NA, begin=0.2, end=1)+
#   geom_sf(data=st_crop(didf.sf,my_bbox), fill=NA, colour="white", size=0.1 )
# plot_gg(pop_kpop, raytrace = TRUE, sunangle=225, width=5,height=5,scale=250,windowsize=c(2000,1125),
#         zoom = 0.5, theta=10, phi = 30)
# render_snapshot(filename=str_c(drpath, "pop 3D gs.svg"), clear = TRUE)
#
#
# # # cartes en 3D rayshader ------------------------------------------------
# #
# #
# # devtools::install_github("tylermorganwall/rayshader")
# # library(rayshader)
# #
# # popraster <- pops.gc200[[2]] %>% as.data.frame(., xy=TRUE)
# # empraster <- emps.giris[[2]] %>% as.data.frame(., xy=TRUE)
# #
# #
# # box <- c(xmin=min(popraster$x), xmax=max(popraster$x), ymin=min(popraster$y), ymax=max(popraster$y))
#
# pop_map <- ggplot()+
#   ggtitle("Population density")+
#   theme(plot.title = element_text(size=14))+
#   geom_raster(data=popraster,
#               aes(x=x,y=y, fill=layer), na.rm=TRUE)+
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="left",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())+
#   scale_fill_viridis_c(na.value=NA, begin=0.2, end=1)+
#   geom_sf(data=st_crop(didf.sf,box), fill=NA, colour="white", size=0.1 )
#
# emp_map <- ggplot()+
#   ggtitle("Jobs density")+
#   theme(plot.title = element_text(size=14))+
#   geom_raster(data=empraster,
#               aes(x=x,y=y, fill=layer), na.rm=TRUE)+
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())+
#   scale_fill_viridis_c(na.value=NA, begin=0.2, end=1)+
#   geom_sf(data=st_crop(didf.sf,box), fill=NA, colour="white", size=0.1 )
#
# drpath <- "G:/Mon Drive/Etude_IDF/draft report/svg/"
# plot_gg(pop_map, raytrace = TRUE, sunangle=225, width=5,height=5,scale=250,windowsize=c(1400,866),
#         zoom = 0.5, theta=10, phi = 30)
# render_snapshot(filename=str_c(drpath, "pop 3D.svg"), clear = TRUE)
#
# plot_gg(emp_map, raytrace = TRUE, sunangle=225, width=5,height=5,scale=250,windowsize=c(1400,866),
#         zoom = 0.5, theta=10, phi = 30)
# render_snapshot(filename=str_c(drpath, "emp 3D.svg"), clear = TRUE)
