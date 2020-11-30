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

# population -> POP et PAUV ------------------------------------

fdt_idf_50 <- load_DVF("fdt_idf_50") %>% swap2tmp_routing()
plan("multiprocess", workers=8)

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

# hauteur des immeubles du voisinage ------------------

batiments.ff2019 <- vroom("{DVFdata}/rda/csv sources/batiments.ff2019.csv" %>% glue, 
                          col_types = cols_only(idbat="c", dnbniv="d",
                                                stoth="d", stotdsueic="d", stotd="d", slocal="d",
                                                sprincp="d", ssecp="d", sparkp="d",sparkncp="d",
                                                X="d", Y="d")) %>% 
  lazy_dt()

batiments.ff2019 %<>% mutate(parth = (stoth+stotdsueic+stotd)/slocal,
                             partp = (sprincp+ssecp)/slocal,
                             partpk = (sparkp+sparkncp)/slocal) %>%
  as_tibble %>% 
  drop_na(X,Y) %>% 
  filter(dnbniv>0) %>% 
  st_as_sf(coords=c("X", "Y"), crs=2154) %>% 
  st_transform(3035) %>% 
  mutate(r = sqrt(slocal/dnbniv/pi))
batiments.ff2019 %<>%
  st_buffer(batiments.ff2019$r)

otpcs <- future_map(3:4, ~OTP_server(router=str_c("IDF",.), port=8400+10*., memory = "8G", rep=DVFdata))
otpc <- otpcs[[1]]

plan("multiprocess", workers=4)
param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=c(300, 600, 900),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=200,
              mode="WALK")
dv3fv4 <- load_DVF("dv3fv4")
h_pos <- dv3fv4 %>% 
  pull(IdINS_50) %>%
  unique() %>% 
  idINS2point(resolution = 50)  %>% 
  sf_project(from=st_crs(3035), to=st_crs(4326), .)
colnames(h_pos) <- c("X", "Y")
h_pos %<>% as_tibble

hauteurs <- future_kernel_isochronique(sf= batiments.ff2019 %>% 
                                    select(dnbniv) %>% 
                                    mutate(cste = 1),
                                positions=h_pos,
                                param=param,
                                res_fac=1,
                                progress = TRUE,
                                n_split=2*nbrOfWorkers(),
                                otpserver=otpcs, resolution=50)

hauteurs %<>% mutate(h_15 = dnbniv_900/cste_900,
                     h_10 = dnbniv_600/cste_600,
                     h_5 = dnbniv_300/cste_300) %>% 
  st_as_sf(coords=c("X", "Y"),
           crs=4326)
raster_hauteur <- rastervar(hauteurs, h_5, h_10, h_15)
save_DVF(raster_hauteur)
