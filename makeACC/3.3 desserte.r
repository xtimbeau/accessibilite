source("access.r")
plan(multisession, workers=8)
c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_as_sf()
idf4k <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(4000) %>% st_as_sf()

c200_idf <- c200 %>% filter(st_within(., idf, sparse=FALSE))

rm(c200)
uu851 <- load_DVF("uu851")
c200_idfdt <- c200_idf %>%
  st_drop_geometry() %>%
  as.data.table()

fdt <- routing_setup_dt("fdt_idf_50", local=TRUE)
plan(multisession, workers=8)

walk <- iso_accessibilite(
  quoi= idf4k %>% transmute(c=1),
  res_quoi=50,
  ou=c200_idf,                       
  resolution=50,                    
  tmax=30,                         
  pdt=1,                          
  routing=fdt)

euc <- routing_setup_euc()
walksl <- iso_accessibilite(
  quoi= idf4k %>% transmute(c=1),
  res_quoi=50,
  ou=c200_idf,                       
  resolution=50,                    
  tmax=30,                         
  pdt=1,                          
  routing=euc)
