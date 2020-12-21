source("access.r")
c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., idf, sparse=FALSE))
rm(c200)
uu851 <- load_DVF("uu851")
c200_idfdt <- c200_idf %>%
  st_drop_geometry() %>%
  as.data.table()

fdt <- routing_setup_dt("fdt_idf_50")
plan(multisession, workers=8)

walk <- iso_accessibilite(
  quoi= idf %>% transmute(c=1),
  res_quoi=50,
  ou=c200_idf,                       
  resolution=50,                    
  tmax=30,                         
  pdt=1,                          
  routing=fdt)

euc <- routing_setup_euc()
walksl <- iso_accessibilite(
  quoi= c200_idf %>% st_centroid() %>% transmute(c=1),
  res_quoi=50,
  ou=c200_idf,                       
  resolution=50,                    
  tmax=30,                         
  pdt=1,                          
  routing=euc)
