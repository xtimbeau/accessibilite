source("access.r")

# on traite quelques équipements avec une isochrone à pied
# on dispose de 3 sources de données
# 1 parcs et jardins de l'IDF
# 2 ecomos 
# 3 CORINE land use

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% 
  filter(UU2010=="00851") %>% 
  st_union() %>%
  st_buffer(4000)
idf <- iris15 %>%
  filter(UU2010=="00851") %>% 
  st_union()
c200_idf <- c200 %>%
  filter(st_within(., idf, sparse=FALSE))
rm(c200)

fdt_idf_50 <- load_DVF("fdt_idf_50") %>% swap2tmp_routing()

plan("multiprocess", workers=8)

# parcs et jardins

jardins <- st_read("{DVFdata}//fdCartes//espaces verts//espaces verts.shp" %>%  glue) %>%
  st_transform(3035) %>% 
  mutate(area=as.numeric(st_area(.)))

kfoot_jardins_50 <- iso_accessibilite(
  quoi = jardins %>% select(area),
  ou=c200_idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

kfoot_jardins_50 <- kfoot_jardins_50$area

save_DVF(kfoot_jardins_50)

# ecomos

ecomos <- st_read("{DVFdata}/fdcartes/ecomos/ecomos-idf.shp" %>% glue) %>% st_transform(3035)
ecomos_idf <- ecomos %>%
  mutate(area=as.numeric(st_area(ecomos))) %>% 
  filter(str_sub(clc6_tx,1,3)%in%c("231", "311")) %>%
  filter(area>5000) %>% 
  filter(st_intersects(., idf %>% st_buffer(4000), sparse=FALSE))

kfoot_ecomos_50 <- iso_accessibilite(
  quoi = ecomos_idf %>% select(area),
  ou=c200_idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

kfoot_ecomos_50 <- kfoot_ecomos_50$area

save_DVF(kfoot_ecomos_50)

# CORINE land use

CORINE_fr <- load_DVF("CORINE_fr")
CORINE_idf <- CORINE_fr %>%
  filter(Code_18%in%c(141,142)|str_detect(Code_18, "^3")|str_detect(Code_18, "^4")) %>% 
  filter(st_intersects(., idf %>% st_buffer(4000), sparse=FALSE)) %>% 
  mutate(area=as.numeric(st_area(.)))

kfoot_corine_50 <- iso_accessibilite(
  quoi = CORINE_idf %>% select(area),
  ou=c200_idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

kfoot_corine_50 <- kfoot_corine_50$area

save_DVF(kfoot_corine_50)