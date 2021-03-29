source('init.r')
c200 <- load_DVF('c200')
iris15 <- load_DVF("iris15")

# IDF -----------------------------------
# isos_tr <- map(depIdf, ~load_DVF("iso75/isotr50r5d{.x}"))
# 
# iso_tr_50_r5 <-list(
#   EMP09 = do.call(raster::merge, map(isos_tr, "EMP09")),
#   P15_POP = do.call(raster::merge, map(isos_tr, "P15_POP")),
#   cste = do.call(raster::merge, map(isos_tr, "cste")))
# 
# names(iso_tr_50_r5$EMP09) <- names(isos_tr[[1]]$EMP09)
# names(iso_tr_50_r5$P15_POP) <- names(isos_tr[[1]]$P15_POP)
# names(iso_tr_50_r5$cste) <- names(isos_tr[[1]]$cste)
# save_DVF(iso_tr_50_r5)

iso_trr5 <- lload_DVF("tr_r550_2020")

ttrr5_emp09 <- iso2time(iso_trr5$EMP09, seuils=c(50000, 100000, 250000, 1000000))
names(ttrr5_emp09)

# ttrr5_emp09_200 <- raster::aggregate(ttrr5_emp09, 4)
# save_DVF(ttrr5_emp09_200, rep="rda/iso200")

#transit
tmap_mode("view")
uu851 <- load_DVF("uu851")
m_idf <- uu851$mbfdc+
  tm_shape(ttrr5_emp09$to100k)+
  tm_raster(style="cont", palette=terrain.colors(20, rev=FALSE))+
  uu851$hdc
tmap_save(tm=m_idf, "{DVFdata}/presentation/vv/idf_ttremp09100k.svg" %>% glue)
# m_idf <- uu851$mbfdc+
#   tm_shape(ttrr5_emp09_200$to25k)+
#   tm_raster(style="cont", palette=heatrg)+
#   uu851$hdc+tm_layout(legend.title.size = 2, legend.text.size = 2)
# graph2svg(m_idf, file="{DVFdata}/presentation/vv/idf_ttremp09 25k" %>% glue)


iris <- load_DVF("iris15") %>% filter(UU2010=="00851") %>% st_union()
mg <- ggplot(iris)+
  geom_sf(fill="grey97", color=NA)+
  geom_Raster(ttrr5_emp09, aes(fill=value), long=TRUE, style="cont")+
  scale_fill_continuous_sequential(palette="Terrain 2", na.value=NA, rev=FALSE)+
  geom_sf(data=iris,fill="transparent", col="black", size=0.1)+
  theme_void(base_size = 9)+
  labs(fill="Time to jobs, minutes")+
  theme(legend.title=element_text(size=7), legend.text=element_text(size=7))+
  facet_wrap(~variable)
ggsave(plot=mg, "{DVFdata}/presentation/vv/idf_ttremp09 facet.jpg" %>% glue, width=25, height = 17, units="cm")


lyon <- lload_DVF("iso_transit_50_r5_Lyon")
t_e9_lyon <- iso2time(lyon$EMP09, seuils=c(100000, 500000, 1000000, 4000000))
m_lyon <- tm_shape(ttrr5_emp09$to100k)+
  tm_raster(style="cont", palette=terrain.colors(20, rev=FALSE))+
  uu851$hdc
tmap_save(tm=m_idf, "{DVFdata}/presentation/vv/idf_ttremp09100k.svg" %>% glue)




ttrr5_emp09_200 <- load_DVF("iso200/ttrr5_emp09_200")
idf_dt <- r2dt(ttrr5_emp09_200)
idf_dt <- merge(idf_dt, c200[, c("idINS200", "Ind")], by.x="idINS", by.y="idINS200")
Nidf <- idf_dt[, sum(Ind)]
idf <- ggplot(idf_dt, aes(x=to25k, y=..density..*Nidf, weight=Ind))+geom_density()

ttrr5_emp09_lyon <- lload_DVF("ttr_r5_emp09_Lyon")
ttrr5_emp09_lyon <- aggregate(ttrr5_emp09_lyon, 4)
lyon_dt <- r2dt(ttrr5_emp09_lyon)
lyon_dt <- merge(lyon_dt, c200[, c("idINS200", "Ind")], by.x="idINS", by.y="idINS200")
Nlyon <- lyon_dt[, sum(Ind)]

# méthode 1, 2 graphs
idf <- ggplot(idf_dt, aes(x=to25k, y=..density..*Nidf, weight=Ind))+geom_density()
lyon <- ggplot(lyon_dt, aes(x=to25k, y=..density..*Nlyon, weight=Ind))+geom_density()
idf+lyon

#méthode 2; 1 seul graphe avec la ville comme groupe
data <- rbind(idf_dt[, .(dist=to25k,Ind, ville="paris")], lyon_dt[, .(dist=to25k,Ind, ville="lyon")])
data <- data[, ind_grp:=sum(Ind), by=ville]
nind <- data[, sum(Ind)]
gg <- map(unique(data$ville), ~geom_density(data=data[ville==.x], aes(x=dist, weight=Ind, y=..density..*(data[ville==.x,sum(Ind)]), col=ville, fill=ville),alpha=0.5))
ggplot()+gg

#distance par buffer
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union()
uu851plus20 <- uu851 %>% st_buffer(20000)
c200_851 <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
c200_851_plus <- c200 %>% filter(st_within(., uu851 %>% st_buffer(10000), sparse=FALSE))
rmax <- sqrt(as.numeric(st_area(uu851))/pi)
idf_buf <- map_dfr(c(-seq(0, 20000, 1000), seq(0, 20000, 1000)) %>% sort %>% unique,
                    ~{
                      uuplus <- uu851 %>% st_buffer(.x)
                      if(st_is_empty(uuplus)) 
                        ccc <- tibble()
                      else {
                        cccc <- c200_851_plus %>% filter(st_covered_by(., uuplus, sparse=FALSE)) %>% st_drop_geometry()
                        cccc <- cccc %>% summarize(pop=sum(Ind)) %>% mutate(dist_buf = .x)
                        }
                      }) %>%
  arrange(dist_buf) %>% 
  mutate(dpop = pop-lag(pop)) %>%
  filter(pop>0) %>% 
  mutate(dist_buf = dist_buf -min(dist_buf))

dist_buf <- ggplot(idf_buf, aes(x=dist_buf, y=dpop))+
  geom_line(col="blue", size=2)+ylim(c(0,1000000))+
  theme(text = element_text(size=24))
graph2svg(dist_buf, file="{DVFdata}/presentation/vv/idf_d_buff" %>% glue)

#distance euclidienne
d2c <- st_distance(st_centroid(uu851), st_as_sf(idf_dt, coords=c("x", "y"), crs=3035)) %>% as.numeric
bb <- seq(0,max(idf_dt$d_c), 1000)
idf_dt[, d_c:=as.numeric(d2c)] [, d_c_bin:=bb[findInterval(d_c, vec=bb)+1]]
dist_c <- ggplot(idf_dt[, .(Ind=sum(Ind)), by="d_c_bin"], aes(x=d_c_bin, y=Ind))+
  geom_line(col="orange", size=2)+
  ylim(c(0,1000000))+
  theme(text = element_text(size=24))
graph2svg(dist_c, file="{DVFdata}/presentation/vv/idf_d_c" %>% glue)

#distance transit

bb <- seq(0,90,2)
idf_dt[, d_buf_bin:=bb[findInterval(to100k, vec=bb)]]
dist_tt <- ggplot(idf_dt[, .(Ind=sum(Ind)), by="d_buf_bin"], aes(x=d_buf_bin, y=Ind))+
  geom_line(col="green", size=2)+
  ylim(c(0,1000000))+
  theme(text = element_text(size=24))
graph2svg(dist_tt, file="{DVFdata}/presentation/vv/idf_d_ttr100kemp09" %>% glue)
dcvsdt <- ggplot(idf_dt, aes(x=d2c, y=to100k))+geom_point(alpha=0.05, col="darkblue")+
  theme(text = element_text(size=24))
graph2svg(dcvsdt, file="{DVFdata}/presentation/vv/idf_dc_versus_dt" %>% glue)

# isochrones
montreuil <- iris15 %>% filter(COM=="93048") %>% st_buffer(4000) %>% st_union
library(ceramic)
username <- Sys.getenv("mapboxusername")
mapbox_key <- Sys.getenv("mapboxkey")
style_id <- "ckjka0noe1eg819qrhuu1vigs"
Sys.setenv(MAPBOX_API_KEY= mapbox_key)
mont_bb <- montreuil %>% st_union %>% st_transform(4326) 
mont_bb2 <- montreuil %>%  st_buffer(2000) %>% st_union() %>% st_transform(4326)
st_crs(mont_bb) <- st_crs("+proj=longlat +ellps=WGS84") 
st_crs(mont_bb2) <- st_crs("+proj=longlat +ellps=WGS84") 
m.mb <- cc_location(loc=mont_bb, zoom = 11, base_url = "https://api.mapbox.com/styles/v1/xtimbeau/{style_id}/tiles/512/{zoom}/{x}/{y}")

tr_r5 <- r5r::setup_r5(data_path = "{DVFdata}/r5r_data/IDFM" %>% glue, verbose = FALSE)
r1 <- r5_isochrone(lon=2.448, lat=48.869, resolution=50, r5_core = tr_r5, mode=c("WALK"), temps_max=20)
r2 <- r5_isochrone(lon=2.416, lat=48.851, resolution=50, r5_core = tr_r5, mode=c("WALK"), temps_max=20)
r3 <- r5_isochrone(lon=2.416, lat=48.851, resolution=50, r5_core = tr_r5, mode=c("CAR"), temps_max=10)
r4 <- r5_isochrone(lon=2.416, lat=48.851, resolution=50, r5_core = tr_r5, mode=c("WALK","TRANSIT"), temps_max=30, max_walk_dist = 1000)
m1 <- tm_shape(m.mb)+tm_rgb()+
  tm_shape(r1, bbox=montreuil)+tm_raster(style="cont", alpha=0.25, title="minutes to reach", palette="Blues")

m2 <- tm_shape(m.mb)+tm_rgb()+
  tm_shape(r2, bbox=montreuil)+tm_raster(style="cont", alpha=0.5, title="minutes to reach", palette="Blues")+
  tm_shape(r1, bbox=montreuil)+tm_raster(style="cont", alpha=0.5, title="minutes to reach", palette="Blues")
m3 <- tm_shape(m.mb)+tm_rgb()+  tm_shape(r3, bbox=montreuil)+tm_raster(style="cont",alpha=0.5, title="minutes to reach", palette="Greens")
m4 <- tm_shape(m.mb)+tm_rgb(alpha=1)+
  tm_shape(r4, bbox=montreuil)+tm_raster(style="cont", alpha=0.5, title="minutes to reach", palette="Greens")

tmap_save(tm=m1, filename="{DVFdata}/presentation/vv/iso_foot1.svg" %>% glue, width=25, height = 17, units="cm")
tmap_save(tm=m2, filename="{DVFdata}/presentation/vv/iso_foot2.svg" %>% glue, width=25, height = 17, units="cm")
tmap_save(tm=m3, filename="{DVFdata}/presentation/vv/iso_car.svg" %>% glue, width=25, height = 17, units="cm")
tmap_save(tm=m4, filename="{DVFdata}/presentation/vv/iso_transit.svg" %>% glue, width=25, height = 17, units="cm")


# voiture
isos <- map(depIdf, ~load_DVF("iso75/isocar50osrmd{.x}"))
iso_car_50_osrm <-list(
  EMP09 = do.call(raster::merge, map(isos, "EMP09")),
  P15_POP = do.call(raster::merge, map(isos, "P15_POP")),
  cste = do.call(raster::merge, map(isos, "cste")))

names(iso_car_50_osrm$EMP09) <- names(isos[[1]]$EMP09)
names(iso_car_50_osrm$P15_POP) <- names(isos[[1]]$P15_POP)
names(iso_car_50_osrm$cste) <- names(isos[[1]]$cste)
save_DVF(iso_car_50_osrm)
iso_car_200_osrm <- map(iso_car_50_osrm, ~raster::aggregate(.x, 4))
save_DVF(iso_car_200_osrm, rep="rda/iso200")

tcarosrm_emp09 <- iso2time(iso_car_50_osrm$EMP09, seuils=c(50000, 750000, 100000,150000,200000, 250000,500000))
save_DVF(tcarosrm_emp09)

tcarosrm_emp09_200 <- raster::aggregate(tcarosrm_emp09, 4)
save_DVF(tcarosrm_emp09_200, rep="rda/iso200")

tcarosrm_emp09_200 <- load_DVF("iso200/tcarosrm_emp09_200")
uu851.mbfdc <- load_DVF("uu851.mbfdc")
fdc851 <- load_DVF("uu851")
m_idf <- uu851.mbfdc+tm_shape(tcarosrm_emp09_200$to100k)+tm_raster(style="cont", palette=heatrg)+fdc851$hdc+tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(m_idf, file="{DVFdata}/presentation/vv/idf_tcaremp09 200" %>% glue)

# graphique en distance
idf_dt <- r2dt(ttrr5_emp09_200)
idf_dt <- merge(idf_dt, c200[, c("idINS200", "Ind")], by.x="idINS", by.y="idINS200")

uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union()
uu851plus20 <- uu851 %>% st_buffer(20000)
c200_851 <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
c200_851_plus <- c200 %>% filter(st_within(., uu851 %>% st_buffer(10000), sparse=FALSE))

# parcs et jardins

pejt <- load_DVF("iso_f_petj_50_osrm")
m_pejt <- uu851.mbfdc+
  tm_shape(pejt$c$iso15m^0.2)+
  tm_raster(style="cont", palette=heatrg)+
  fdc851$hdc+tm_layout(legend.title.size = 2, legend.text.size = 2)
graph2svg(m_pejt, file="{DVFdata}/presentation/vv/pejt iso15m" %>% glue)
