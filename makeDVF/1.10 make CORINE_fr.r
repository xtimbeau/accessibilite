source("access.r")

iris15 <- load_DVF("iris15")

france_metro <- iris15 %>% st_union()

CORINE <- st_read("{DVFdata}/CORINE land use/u2018_clc2018_v2020_20u1_fgdb/DATA/U2018_CLC2018_V2020_20u1.gdb" %>% glue)

CORINE_fr <- CORINE %>%
  filter(st_geometry_type(.)=="MULTIPOLYGON") %>% 
  filter(st_intersects(., france_metro %>% st_buffer(2000), sparse=FALSE))

save_DVF(CORINE_fr)