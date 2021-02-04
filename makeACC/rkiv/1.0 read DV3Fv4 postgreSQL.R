# Utilisateur postgres pwd toto
# cd "C:\program files\postgresql\12\bin"
# .\psql -h localhost -p 5432 -U postgres -c "CREATE DATABASE dv3fv4;"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -c "CREATE EXTENSION postgis; CREATE EXTENSION postgis_topology;"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_initial.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d75.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d77.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d78.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d91.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d92.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d93.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d94.sql"
# .\psql -h localhost -p 5432 -U postgres -d dv3fv4 -f "G:\Mon Drive\DVFdata\DV3F r11\v4\DV3F_4-0_SQL_LAMB93_R011-ED201\1_DONNEES_LIVRAISON\dv3f_d95.sql"
# fichiers fonciers
# .\psql -h localhost -p 5432 -U postgres -c "CREATE DATABASE ff2019;"
# .\psql -h localhost -p 5432 -U postgres -d ff2019 -f "G:\Mon Drive\DVFdata\FF2019\FF_1-0_SQL_LAMB93_R11-ED191\1_DONNEES_LIVRAISON\ff_r11_2019.sql"
# .\psql -h localhost -p 5432 -U postgres -d ff2019 -f "G:\Mon Drive\DVFdata\FF2019\FF_1-0_SQL_LAMB93_R11-ED191\1_DONNEES_LIVRAISON\ff_r11_2019.sql"

source("DVF.r")

# install.packages("RPostgreSQL")
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")

# Données de valeur foncière -----------------------------

con <- dbConnect(drv, dbname = "dv3fv4",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "toto")

mutations <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomlocmut) as st_geomlocmut,
                        ST_AsText(geomparmut) as st_geomparmut,
                        ST_AsText(geompar) as st_geompar from dvf.mutation") %>%
  as_tibble %>% 
  mutate( st_geomparmut = ifelse(is.na(st_geomparmut), "POLYGON EMPTY", st_geomparmut),
          st_geomlocmut = ifelse(is.na(st_geomlocmut), "POINT EMPTY", st_geomlocmut),
          st_geompar = ifelse(is.na(st_geompar), "POINT EMPTY", st_geompar)) %>% 
  st_as_sf(wkt="st_geompar", crs=2154)

locaux <- dbGetQuery(con, "SELECT *, 
                     ST_AsText(geomloc) as st_geomloc from dvf.local") %>% 
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

dispositions_parcelles <- dbGetQuery(con, "SELECT *,
                                     ST_AsText(geomloc) as st_geomloc, 
                                     ST_AsText(geompar) as st_geompar 
                                     from dvf.disposition_parcelle") %>%
  as_tibble %>%
  mutate( st_geompar = ifelse(is.na(st_geompar), "POLYGON EMPTY", st_geompar),
          st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geompar", crs=2154)
  
acheteurs_vendeurs <- dbGetQuery(con, "SELECT * from dvf.acheteur_vendeur") %>% as_tibble

lots <- dbGetQuery(con, "SELECT * from dvf.lot") %>% as_tibble

parcelles <- dbGetQuery(con, "SELECT * from dvf.parcelle") %>% as_tibble

dispositions <- dbGetQuery(con, "SELECT * from dvf.disposition") %>% as_tibble

dv3fv4_idf <- list(mutations=mutations,
                 locaux=locaux,
                 dispositions_parcelles=dispositions_parcelles,
                 acheteurs_vendeurs=acheteurs_vendeurs,
                 lots=lots,
                 parcelles=parcelles,
                 dispositions=dispositions)

walk( names(dv3fv4_idf), ~{
  assign(.x, dv3fv4_idf[[.x]], envir=.GlobalEnv)
  save(list=c(.x), file= "{DVFdata}/dv3f/v4/r11r/{.x}.Rda" %>% glue, envir=.GlobalEnv)
})

walk(c("mutations","locaux", "dispositions_parcelles","lots","parcelles"),
     ~load(file = "{DVFdata}/dv3f/v4/r11r/{.x}.Rda" %>% glue, envir = .GlobalEnv))

fwrite(locaux, "{localdata}/locaux_v4.csv" %>% glue)
coords <- st_coordinates(st_centroid(st_geometry(mutations)))
mutations <- mutations %>% 
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble %>% 
  select(-st_geompar)
fwrite(mutations, "{localdata}/mutations_v4.csv" %>% glue)

fwrite(lots, "{localdata}/lots_v4.csv" %>% glue)

coords <- st_coordinates(st_centroid(st_geometry(dispositions_parcelles)))
dispositions_parcelles <- dispositions_parcelles %>% 
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble %>% 
  select(-st_geompar)
fwrite(dispositions_parcelles, "{localdata}/dispositions_parcelles_v4.csv" %>% glue)

rm(mutations, locaux, dispositions_parcelles, lots, parcelles)

# fichiers fonciers ----------------------

con <- dbConnect(drv, dbname = "ff2019",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "toto")

tup.ff2019 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc,
                        ST_AsText(geomtup) as st_geomtup from ff_2019.ffta_2019_tup") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc),
          st_geomtup = ifelse(is.na(st_geomtup), "POLYGON EMPTY", st_geomtup)) %>% 
  st_as_sf(wkt="st_geomtup", crs=2154)

save_DVF(tup.ff2019, rep="FF2019/r11r")

parcelles.ff2019 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc,
                        ST_AsText(geompar) as st_geompar from ff_2019.fftp_2019_pnb10_parcelle") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc),
          st_geompar = ifelse(is.na(st_geompar), "POLYGON EMPTY", st_geompar)) %>% 
  st_as_sf(wkt="st_geompar", crs=2154)

save_DVF(parcelles.ff2019, rep="FF2019/r11r")

locaux.ff2019 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc from ff_2019.fftp_2019_pb0010_local") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

save_DVF(locaux.ff2019, rep="FF2019/r11r")

proprietaires.ff2019 <- dbGetQuery(con, "SELECT * from ff_2019.fftp_2019_proprietaire_droit") %>%
  as_tibble

save_DVF(proprietaires.ff2019, rep="FF2019/r11r")

batiments.ff2019 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc from ff_2019.ffta_2019_batiment") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

save_DVF(batiments.ff2019, rep="FF2019/r11r")

pevprincipale.ff2019 <- dbGetQuery(con, "SELECT * from ff_2019.fftp_2019_pb40_pevprincipale") %>%
  as_tibble

save_DVF(pevprincipale.ff2019, rep="FF2019/r11r")

pev.ff2019 <- dbGetQuery(con, "SELECT * from ff_2019.fftp_2019_pb21_pev") %>%
  as_tibble

save_DVF(pev.ff2019, rep="FF2019/r11r")

fwrite(locaux.ff2019, file="{localdata}/locaux.ff2019.csv" %>% glue)
fwrite(pevprincipale.ff2019,"{localdata}/pevprincipale.ff2019.csv" %>% glue)
fwrite(pev.ff2019,"{localdata}/pev.ff2019.csv" %>% glue)

coords <- st_coordinates(st_centroid(st_geometry(parcelles.ff2019)))
parcelles.ff2019 <- parcelles.ff2019 %>% 
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble %>% 
  select(-st_geompar, -geompar)
fwrite(parcelles.ff2019,"{localdata}/parcelles.ff2019.csv" %>% glue)

coords <- st_coordinates(st_centroid(st_geometry(batiments.ff2019)))
batiments.ff2019 <- batiments.ff2019 %>% 
  mutate(X = coords[,"X"], Y=coords[,"Y"]) %>% 
  as_tibble %>% 
  select(-st_geomloc, -geomloc)
fwrite(batiments.ff2019,"{localdata}/batiments.ff2019.csv" %>% glue)