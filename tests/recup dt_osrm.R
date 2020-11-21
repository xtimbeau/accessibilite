source("access.r")

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(4000)
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., idf, sparse=FALSE))
c200_75 <- c200 %>% filter(dep=="75")
c200_mtrl <- c200 %>% filter(Depcom=="93048")
c200_idf4km <- c200 %>% filter(st_within(., idf4km, sparse=FALSE))
rm(c200)

aa <- list.files("E:/run")
access <- map(aa, ~{ 
  dd <- fread("E:/run/{.x}" %>% glue)
  dd <- dd[travel_time<=20,.(fromId, toId, travel_time)]
  setkey(dd, fromId)
  setindex(dd, toId)
  dd})
names(access) <- str_extract(aa, "N[:alnum:]*")

# access <- rbindlist(map(aa,~(fread("E:/run/{.x}" %>% glue))))
# access <- access[travel_time<=20,]
names(access) <- str_extract(aa, "N[:alnum:]*")

ouetquoi <- iso_ouetquoi_4326(
  ou=c200_idf,
  quoi=c200_idf4km %>% transmute(c=1), 
  res_ou=50, 
  res_quoi=Inf,
  opp_var=c(c="c"),
  fun_quoi="any",
  resolution=50)

groupes <- iso_split_ou(
  ou=ouetquoi$ou_4326, 
  quoi=ouetquoi$quoi_4326,
  chunk=5000000,
  routing=routing_setup_osrm(server="5002", profile="walk"),
  tmax=20)

f20_osrm_idf_50 <- list(
  type = "dt",
  origin = "OSRM",
  origin_string = "",
  string = "matrice de time travel OSRM precalculee" %>% glue,
  time_table = access,
  fromId = groupes$ou,
  toId = ouetquoi$quoi_4326[, .(id, lon, lat, x, y)], 
  groupes=groupes$ou_gr,
  resolution=groupes$resINS,
  res_ou = 50,
  res_quoi = 50,
  ancres=FALSE, 
  future=FALSE)

save_DVF(f20_osrm_idf_50, preset="high")

f20_osrm_idf_50 <- load_DVF("f20_osrm_idf_50")
f20_osrm_idf_50$future <- FALSE
plan(multisession, workers=2)
foot_ttm_50 <- iso_accessibilite(quoi = c200_idf4km %>% transmute(c=1),
                                 ou = c200_idf,
                                 resolution=50,
                                 routing=f20_osrm_idf_50,
                                 tmax=20, future=FALSE)
uu851 <- load_DVF("uu851")
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
  routing=f20_osrm_idf_50)

ist_ecomos <- rr$c %>% iso2time(c(1,5,10,15,20,50,100))
save_DVF(ist_ecomos)
c200 <- load_DVF("c200") %>% st_drop_geometry() %>% as.data.table()
idf_dt <- r2dt(ist_ecomos, 200)
idf_dt <- merge(idf_dt, c200[, .(idINS200, Ind)], by="idINS200")
distances <-c("to1", "to5","to10","to15","to20","to50", "to100")
idf_dtm <- idf_dt %>% melt(measure.vars=distances, variable.name="seuil", value.name="temps")
ggplot(idf_dtm)+geom_massity(aes(x=temps, mass=Ind, y=after_stat(cummass), col=seuil))+scale_y_continuous(labels=f2si2)
