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
access <- map(aa,~(fread("E:/run/{.x}" %>% glue))[travel_time<=20,.(fromId, toId, travel_time)])
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

setkey(access, fromId)
setindex(access, toId)

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
  ancres=FALSE, 
  future=FALSE)

save_DVF(f20_osrm_idf_50)

f20_osrm_idf_50 <- load_DVF("f20_osrm_idf_50")

foot_ttm_50 <- iso_accessibilite(quoi = c200_idf4km %>% transmute(c=1),
                                 ou = c200_idf,
                                 resolution=50,
                                 routing=f20_osrm_idf_50,
                                 tmax=20)
uu851 <- load_DVF("uu851")
ecomos <- st_read("{DVFdata}/fdcartes/ecomos/ecomos-idf.shp" %>% glue) %>% st_transform(3035)
ecomos_idf <- ecomos %>% filter(!clc6%in%c(231114, 332202 , 0)) %>% filter(st_within(., uu851$iris %>% st_union %>% st_buffer(2000), sparse=FALSE))

rr <- iso_accessibilite(
  quoi=ecomos_idf %>% transmute(c=1),
  ou=c200_idf,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=f20_osrm_idf_50)
