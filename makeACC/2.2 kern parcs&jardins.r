source("init.r")

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

fdt_idf_50 <- lload_DVF("fdt_idf_50") %>% swap2tmp_routing()

plan("multiprocess", workers=8)

# parcs et jardins

jardins <- st_read("{DVFdata}/sources/fdCartes//espaces verts//espaces verts.shp" %>%  glue) %>%
  st_transform(3035) %>% 
  mutate(area=as.numeric(st_area(.)))

kfoot_jardins_50 <- iso_accessibilite(
  quoi = jardins %>% select(area),
    ou=idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

kfoot_jardins_50 <- kfoot_jardins_50$area

save_DVF(kfoot_jardins_50)

# ecomos

ecomos <- st_read("{DVFdata}/sources/fdcartes/ecomos/ecomos-idf.shp" %>% glue) %>% st_transform(3035)
ecomos_idf <- ecomos %>%
  mutate(area=as.numeric(st_area(ecomos))) %>% 
  filter(str_sub(clc6_tx,1,3)%in%c("231", "311")) %>%
  filter(area>5000) %>% 
  filter(st_intersects(., idf %>% st_buffer(4000), sparse=FALSE))

kfoot_ecomos_50 <- iso_accessibilite(
  quoi = ecomos_idf %>% select(area),
  ou=idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

kfoot_ecomos_50 <- kfoot_ecomos_50$area

save_DVF(kfoot_ecomos_50)

# CORINE land use
# https://land.copernicus.eu/user-corner/technical-library/corine-land-cover-nomenclature-guidelines/html

CORINE_fr <- load_DVF("CORINE_fr")
CORINE_idf <- CORINE_fr %>%
  filter(Code_18%in%c(141,142)|
           str_detect(Code_18, "^2")|
           str_detect(Code_18, "^3")|
           str_detect(Code_18, "^4")|
           str_detect(Code_18, "^5")) %>% 
  filter(st_intersects(., idf %>% st_buffer(4000), sparse=FALSE)) %>% 
  rename(geometry=Shape) %>% 
  mutate(area=as.numeric(st_area(.)), c=1L) %>%
  st_agr_constant(area) %>% 
  st_agr_aggregate(c)
rm(CORINE_fr)

uu851 <- load_DVF("uu851")
riv <- st_read("{DVFdata}/sources/fdCartes/elthydrosurface/EltHydroSurface_FXX.shp" %>% glue) %>%
  st_transform(3035) %>% 
  filter(Type%in%c("Plan d'eau, bassin, réservoir", "Cours d'eau")) %>%
  st_filter(uu851$border) %>% 
  st_crop(uu851$border) %>% 
  transmute(Code_18="513", Remark="", Area_Ha=as.numeric(st_area(.))/10000, ID="", Shape_Length=0, Shape_Area=Area_Ha, area=as.numeric(st_area(.)),c=1L) %>% 
  st_buffer(25)

CORINE_idf2 <- rbind(CORINE_idf, riv) %>% 
  mutate(geometry=st_cast(st_geometry(.), to="MULTIPOLYGON"))

kfoot_corine_50 <- iso_accessibilite(
  quoi = CORINE_idf2 %>% select(area, c),
  ou=idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

riv_50 <- iso_accessibilite(
  quoi = CORINE_idf2 %>% filter(Code_18=="513") %>% select(area, c),
  ou=idf,
  resolution=50,
  tmax=20,
  pdt=1,
  routing=fdt_idf_50)

kfoot_corine_50c <- kfoot_corine_50$c
kfoot_corine_50 <- kfoot_corine_50$area

save_DVF(kfoot_corine_50)
save_DVF(kfoot_corine_50c)

kfoot_corine_50c %>% iso2time(0.05) %>% plot()
ggplot()+geom_Raster(kfoot_corine_50c %>% iso2time(0.05), aes(fill=to50m))

kfoot_corine_50 %>% iso2time(2e+5) %>% plot()
ggplot()+geom_Raster(kfoot_corine_50 %>% iso2time(2e+5), aes(fill=to200k))
