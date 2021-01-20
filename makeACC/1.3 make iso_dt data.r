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

# lancer le docker correspondant
foot_osrm <- routing_setup_osrm(server="5002", profile="walk")

fdt_idf_50 <- iso_accessibilite(
  quoi = idf4km %>% st_sf() %>% transmute(c=1),
  ou = idf,
  resolution=res,
  routing=foot_osrm,
  tmax=20,
  ttm_out = TRUE, 
  future=TRUE,
  dir="{localdata}/osrm_foot_45_50" %>% glue)

save_DVF(fdt_idf_50, preset="high", local=TRUE)
