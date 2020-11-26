source("access.r")

res <- 50

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(4000)
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., idf, sparse=FALSE))
c200_75 <- c200 %>% filter(dep=="75")
c200_mtrl <- c200 %>% filter(Depcom=="93048")
rm(c200)

plan(multisession, workers=8)
foot_osrm <- routing_setup_osrm(server="5002", profile="walk")

fdt_idf_50 <- iso_accessibilite(
  quoi = idf4km %>% st_sf() %>% transmute(c=1),
  ou = c200_idf,
  resolution=res,
  routing=foot_osrm,
  tmax=20,
  ttm_out = TRUE, 
  future=TRUE,
  dir="e:/osrm23112020")

save_DVF(fdt_idf_50, preset="high")
rm(fdt_idf_50)

# test ---------------------
source("access.r")

c200 <- load_DVF("c200") 
iris15 <- load_DVF("iris15")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
uu851 <- load_DVF("uu851")
c200_idfdt <- c200_idf %>%
  st_drop_geometry() %>%
  as.data.table()

fdt_idf_50 <- load_DVF("fdt_idf_50") 
fdt_idf_50 <- swap2tmp_routing(fdt_idf_50, qs=TRUE)
plan(multisession, workers=8)

# ecomos -----------------
# ne marche que pour l'idf, détail pour bcp d'espace vert, même les plus petits

ecomos <- st_read("{DVFdata}/fdcartes/ecomos/ecomos-idf.shp" %>% glue) %>% st_transform(3035)
ecomos_idf <- ecomos %>%
  mutate(area=as.numeric(st_area(ecomos))) %>% 
  filter(str_sub(clc6_tx,1,3)%in%c("231", "311")) %>%
  filter(area>5000) %>% 
  filter(st_within(., uu851$iris %>% st_union %>% st_buffer(2000), sparse=FALSE))

rr <- iso_accessibilite(
  quoi=ecomos_idf %>% transmute(c=1),
  ou=c200_idf,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=fdt_idf_50)

ist_ecomos <- rr$c %>% iso2time(c(1,5,10,15,20,25,30,35,40,45,50,100))
save_DVF(ist_ecomos)
tm_shape(ist_ecomos)+tm_raster(style="cont", palette=heatrg)

idf_dt <- r2dt(ist_ecomos, 200)
idf_dt <- merge(idf_dt, c200_idf[, .(idINS200, Ind)], by="idINS200")
distances <-c("to1", "to5","to10","to15","to20","to50", "to100")
idf_dtm <- idf_dt %>% melt(measure.vars=distances, variable.name="seuil", value.name="temps")
ggplot(idf_dtm)+geom_massity(aes(x=temps, mass=Ind, y=after_stat(cummass)/after_stat(cummass), col=seuil))+scale_y_continuous(labels=f2si2)

# CORINE ----------------

CORINE_fr <- load_DVF("CORINE_fr")
CORINE_idf <- CORINE_fr %>%
  filter(Code_18%in%c(141,142)|str_detect(Code_18, "^3")|str_detect(Code_18, "^4")) %>% 
  filter(st_intersects(., uu851$iris %>% st_union %>% st_buffer(2000), sparse=FALSE))

korine <- iso_accessibilite(
  quoi=CORINE_idf %>% transmute(c=1),
  ou=c200_idf,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=fdt_idf_50)

save_DVF(korine, rep="rda/isoIDF")
korine <- load_DVF("isoIDF/korine")
torine <- iso2time(korine$c, c(0.5, 1, 5))
save_DVF(torine, rep="rda/isoIDF")
mm <- tm_shape(torine$to20)+tm_raster(style="cont", palette=heatrg)
graph2svg(mm, file="test", textratio=3)
torine_dt <- r2dt(torine, 200)
torine_dt <- merge(c200_idfdt[, .(idINS200, Ind, dep)], torine_dt, by="idINS200")
distances <-c("to0.5", "to1", "to5")
tor_dtm <- torine_dt %>%
  melt(measure.vars=distances, variable.name="seuil", value.name="temps")
gg <- ggplot(tor_dtm)+geom_massity(aes(x=temps, mass=Ind, y=after_stat(mass), col=dep, fill=dep), position="stack")+scale_y_continuous(labels=uf2si2)+facet_wrap(~seuil)
graph2svg(gg, file="test2")
