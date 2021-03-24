source("init.r")

install.packages("RPostgreSQL")
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
# r75 est la région Nouvelle Aquitaine
reg <- "r75"
rep <- glue("FF2018/{reg}r")

# fichiers fonciers ----------------------

con <- dbConnect(drv, dbname = "{reg}_ff2018" %>% glue,
                 host = "localhost", port = 5432,
                 user = "postgres", password = "toto")

tup.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc,
                        ST_AsText(geomtup) as st_geomtup from ff_2018.ffta_2018_tup") %>%
  as_tibble() %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc),
          st_geomtup = ifelse(is.na(st_geomtup), "POLYGON EMPTY", st_geomtup)) %>% 
  st_as_sf(wkt="st_geomtup", crs=2154)

save_DVF(tup.ff2018, rep="FF2018/r11r")

parcelles.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc,
                        ST_AsText(geompar) as st_geompar from ff_2018.fftp_2018_pnb10_parcelle") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc),
          st_geompar = ifelse(is.na(st_geompar), "POLYGON EMPTY", st_geompar)) %>% 
  st_as_sf(wkt="st_geompar", crs=2154)

save_DVF(parcelles.ff2018, rep=rep)

locaux.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc from ff_2018.fftp_2018_pb0010_local") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

save_DVF(locaux.ff2018, rep=rep)

proprietaires.ff2018 <- dbGetQuery(con, "SELECT * from ff_2018.fftp_2018_proprietaire_droit") %>%
  as_tibble()

save_DVF(proprietaires.ff2018, rep=rep)

batiments.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc from ff_2018.ffta_2018_batiment") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

save_DVF(batiments.ff2018, rep=rep)

pevprincipale.ff2018 <- dbGetQuery(con, "SELECT * from ff_2018.fftp_2018_pb40_pevprincipale") %>%
  as_tibble

save_DVF(pevprincipale.ff2018, rep=rep)

pev.ff2018 <- dbGetQuery(con, "SELECT * from ff_2018.fftp_2018_pb21_pev") %>%
  as_tibble

save_DVF(pev.ff2018, rep=rep)

fwrite(pevprincipale.ff2018,"{localdata}/pevprincipale.ff2018.csv" %>% glue)
fwrite(pev.ff2018,"{localdata}/pev.ff2018.{reg}.csv" %>% glue)

parcelles.ff2018 <- lload_DVF("parcelles.ff2018")
coords <- st_coordinates(st_centroid(st_geometry(parcelles.ff2018)))
parcelles.ff2018 <- parcelles.ff2018 %>% 
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble() %>% 
  select(-st_geomloc, -geomloc, -st_geompar, -geompar)
fwrite(parcelles.ff2018,"{localdata}/parcelles.ff2018.{reg}.csv" %>% glue)

coords <- st_coordinates(st_centroid(st_geometry(batiments.ff2018)))
batiments.ff2018 <- batiments.ff2018 %>% 
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble %>% 
  select(-st_geomloc, -geomloc)
fwrite(batiments.ff2018,"{localdata}/batiments.ff2018.{reg}.csv" %>% glue)

coords <- st_coordinates(locaux.ff2018 %>% st_transform(3035))
locaux.ff2018 <- locaux.ff2018 %>%
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble() %>% 
  select(-st_geomloc, -geomloc)
fwrite(locaux.ff2018, file="{localdata}/locaux.ff2018.{reg}.csv" %>% glue)

dbDisconnect(con)
