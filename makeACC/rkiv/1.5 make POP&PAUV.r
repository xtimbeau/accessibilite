source("DVF.r")

# on traite quelques équipements avec une isochrone à pied
# dans un premier temps on regarde surtout les crêches (D502)

c200idf <- load_DVF("c200idf") %>% st_transform(3035)
iris <- load_DVF("iris") %>% st_transform(3035)
bbPC <- st_bbox(st_union(iris %>% filter(DEP %in% c("75"))))
uu <- filter(iris, UU2010==851) %>% pull(INSEE_COM)
didf.sf <- iris %>% 
  filter(DEP%in%depIdf) %>%
  group_by(DEP) %>% 
  summarize(P15_POP=sum(P15_POP), 
            EMP09=sum(EMP09, na.rm=TRUE), 
            UU2010=list(UU2010))

# population -> POP et PAUV ------------------------------------

otpcs <- future_map(3:4, ~OTP_server(router=str_c("IDF",.), port=8400+10*., memory = "8G", rep=localdata))
otpc <- otpcs[[1]]

plan("multiprocess", workers=4)
param <- list(otpcon=otpc,
              date = "12-17-2019", time = "08:00:00",
              cutoffs=c(300, 600, 900),
              walkReluctance=2,
              maxWalkDistance=10000,
              precisionMeters=10,
              offRoadDistanceMeters=50,
              mode="WALK")

g_pos <- c200idf %>% 
  filter(Ind>0) %>%
  st_transform(4326) 
coords <- st_coordinates(st_centroid(g_pos)) 
g_pos <- g_pos %>% 
  as_tibble %>% 
  transmute(Depcom,
            X=coords[, "X"],
            Y=coords[, "Y"])

pop <- future_kernel_isochronique(sf=c200idf %>% 
                                    select(Ind, Men, Men_pauv) %>% 
                                    mutate(cste = 1),
                                  positions=g_pos,
                                  param=param,
                                  res_fac=4,
                                  progress = TRUE,
                                  n_split=2*nbrOfWorkers(),
                                  
                                  otpserver=otpcs)

pop.sf <- pop %>% st_as_sf(crs=4326, coords=c("X","Y")) %>% st_transform(3035)
pop.sf <- pop.sf %>% mutate(PAUV15_5 = ifelse(Men_300>0, Men_pauv_300/Men_300, 0),
                            PAUV15_10 = ifelse(Men_600>0, Men_pauv_600/Men_600, 0),
                            PAUV15_15 = ifelse(Men_900>0, Men_pauv_900/Men_900, 0),
                            POP15_5 = Ind_300/cste_300/0.04, # on calcule la densité, un carreau fait 0.04km²
                            POP15_10 = Ind_600/cste_600/0.04, # on calcule la densité, un carreau fait 0.04km²
                            POP15_15 = Ind_900/cste_900/0.04)

# popisokernel <- st_join(c200idf %>% select(IdINSPIRE), pop.sf, join=st_contains)

popisokernel <- pop.sf %>% 
  mutate(IdINS_200 = point_on_idINS(pop.sf, resolution=200))

save_DVF(popisokernel)

# hauteur des immeubles du voisinage ------------------

batiments.ff2019 <- vroom("{localdata}/batiments.ff2019.csv" %>% glue, 
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
