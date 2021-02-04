source("DVF.r")

# install.packages("RPostgreSQL")
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "DV3F",
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

dv3f_idf <- list(mutations=mutations,
                 locaux=locaux,
                 dispositions_parcelles=dispositions_parcelles,
                 acheteurs_vendeurs=acheteurs_vendeurs,
                 lots=lots,
                 parcelles=parcelles,
                 dispositions=dispositions)

walk( names(dv3f_idf), ~{
  assign(.x, dv3f_idf[[.x]], envir=.GlobalEnv)
  save(list=c(.x), file= "{DVFdata}/DV3F/r11r/{.x}.Rda", envir=.GlobalEnv)
})

save_DVF(dv3f_idf)

# lecture des fichiers fonciers
# le dump sql  ./psql -h localhost -U postgres -d FF2018 -f "G:\Mon Drive\DVFdata\FF2018\R11_Ile_de_France\R11_Ile_de_France\FF_1-0_SQL_LAMB93_R011-ED181\1_DONNEES_LIVRAISON\ff_r11_2018.sql"

con <- dbConnect(drv, dbname = "FF2018",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "toto")

tup.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc,
                        ST_AsText(geomtup) as st_geomtup from ff_2018.ffta_2018_tup") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc),
          st_geomtup = ifelse(is.na(st_geomtup), "POLYGON EMPTY", st_geomtup)) %>% 
  st_as_sf(wkt="st_geomtup", crs=2154)

save_DVF(tup.ff2018, rep="FF2018/r11r")
rm(tup.ff2018)

parcelles.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc,
                        ST_AsText(geompar) as st_geompar from ff_2018.fftp_2018_pnb10_parcelle") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc),
          st_geompar = ifelse(is.na(st_geompar), "POLYGON EMPTY", st_geompar)) %>% 
  st_as_sf(wkt="st_geompar", crs=2154)

save_DVF(parcelles.ff2018, rep="FF2018/r11r")
rm(parcelles.ff2018)

locaux.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc from ff_2018.fftp_2018_pb0010_local") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

save_DVF(locaux.ff2018, rep="FF2018/r11r")
rm(locaux.ff2018)

proprietaires.ff2018 <- dbGetQuery(con, "SELECT * from ff_2018.fftp_2018_proprietaire_droit") %>%
  as_tibble

save_DVF(proprietaires.ff2018, rep="FF2018/r11r")
rm(proprietaires.ff2018)

batiments.ff2018 <- dbGetQuery(con, "SELECT *,
                        ST_AsText(geomloc) as st_geomloc from ff_2018.ffta_2018_batiment") %>%
  as_tibble %>% 
  mutate( st_geomloc = ifelse(is.na(st_geomloc), "POINT EMPTY", st_geomloc)) %>% 
  st_as_sf(wkt="st_geomloc", crs=2154)

pevprincipale.ff2018 <- dbGetQuery(con, "SELECT * from ff_2018.fftp_2018_pb40_pevprincipale") %>%
  as_tibble

save_DVF(pevprincipale.ff2018, rep="FF2018/r11r")

pev.ff2018 <- dbGetQuery(con, "SELECT * from ff_2018.fftp_2018_pb21_pev") %>%
  as_tibble

save_DVF(pev.ff2018, rep="FF2018/r11r")

rm(batiments.ff2018, pevprincipale.ff2018, pev.ff2018)
