source("access.r")

# on traite quelques équipements avec une isochrone à pied
# dans un premier temps on regarde surtout les crêches (D502)

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(4000)
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., idf, sparse=FALSE))
c200_75 <- c200 %>% filter(dep=="75")
c200_mtrl <- c200 %>% filter(Depcom=="93048")
rm(c200)
iris15 <- load_DVF("iris15")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idfdt <- c200_idf %>%
  st_drop_geometry() %>%
  as.data.table()

fdt_idf_50 <- load_DVF("fdt_idf_50") %>% swap2tmp_routing()
plan("multiprocess", workers=8)

# population -> POP et PAUV ------------------------------------
c200_idf <- c200_idf %>% 
  select(Ind, Men, Men_pauv) %>% 
  mutate(cste = 1) %>% 
  st_agr_aggregate(Ind, Men, Men_pauv, cste)
pop <- iso_accessibilite(
  quoi=c200_idf %>% 
    select(Ind, Men, Men_pauv) %>% 
    mutate(cste = 1),
  ou=c200_idf,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=fdt_idf_50)

pop200 <- map(pop, ~aggregate(.x, 4))
densite <- r2dt(pop200$Ind/pop200$cste/0.04)
txpauv <- r2dt(pop200$Men_pauv/pop200$Ind)

popisokernel <- merge(densite[,-c("x","y")], txpauv[,-c("x","y")], by="idINS200", suffix=c(".densite", ".txpauv"))

save_DVF(popisokernel)
