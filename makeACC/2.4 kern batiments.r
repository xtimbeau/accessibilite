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

# hauteur des immeubles du voisinage ------------------

batiments.ff2019 <- vroom("{DVFdata}/rda/csv sources/batiments.ff2019.csv" %>% glue, 
                          col_types = cols_only(idbat="c", dnbniv="d",
                                                stoth="d", stotdsueic="d", stotd="d", slocal="d",
                                                sprincp="d", ssecp="d", sparkp="d",sparkncp="d",
                                                X="d", Y="d")) %>% 
  lazy_dt()

batiments.ff2019 %<>%
  mutate(parth = (stoth+stotdsueic+stotd)/slocal,
         partp = (sprincp+ssecp)/slocal,
         partpk = (sparkp+sparkncp)/slocal) %>%
  as_tibble() %>% 
  drop_na(X,Y) %>% 
  filter(dnbniv>0) %>% 
  st_as_sf(coords=c("X", "Y"), crs=2154) %>% 
  st_transform(3035) %>% 
  mutate(r = sqrt(slocal/dnbniv/pi))

hauteurs <- iso_accessibilite(
  quoi = batiments.ff2019 %>% 
    select(dnbniv) %>% 
    mutate(cste = 1),
  ou=c200_idf,
  resolution=50,
  res_quoi=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

khauteurs <- hauteurs$dnbniv/hauteurs$cste

save_DVF(khauteurs)