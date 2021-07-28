
library(googledrive)
library(qs)
library(tidyverse)
library(sf)
library(data.table)
library(DBI)
library(nuvolos)


sources <- "https://drive.google.com/drive/folders/12bqsgtWB8-Er_zCKYm-2yuXOrxAu0FM5" |> as_id()
dv3fv5.id <- "https://drive.google.com/drive/folders/1-j3sUqQI9HKqaWEudALgsoiNy-lI1cnU" |> as_id()
drive_auth(email="xavier.timbeau@sciencespo.fr", cache="secrets", use_oob = TRUE)
googledrive::drive_find("DVFdata")
drive_ls(sources, recursive=TRUE)
drive_find("~/DVFdata/sources/DV3F/v5/national/")

drive_download(as_id(drive_ls(dv3fv5)$id))

dv3fv5 <- qs::qread("mutation_dv3fv5.rda", nthreads=16)

geom <- st_geometry(dv3fv5)
dv3fv5 <- dv3fv5 |>
  as_tibble() |> 
  select(-starts_with("st_"))
fwrite(dv3fv5, file="dv3fv5.csv")

dv3fv5 <- qs::qread("mutation_dv3fv5.rda", nthreads=16)
geom <- st_geometry(dv3fv5)
rm(dv3fv5)
gc()
geom <- st_centroid(geom)
gc()
geom <- st_transform(geom, 4326)
gc()
coords <- st_coordinates(geom)
coords <- as.data.frame(coords)
names(coords) <- c("lon", "lat")
fwrite(coords, file="coords.csv")
dv3fv5 <- dv3fv5 |> as_tibble() |> 
  mutate(lon=coords[,1], lat=coords[,2]) |> 
  select(-starts_with("st_"), -geometry)
drive_upload("dv3fv5.csv", dv3fv5.id, name="mutation_dv3fv5.1.csv")

dv3fv5 <- dv3fv5 |> 
  select(-starts_with("geom", ignore.case=FALSE))

dv3fv5 <- fread("dv3fv5.csv")
# coords <- fread("coords.csv")
# dv3fv5 <- cbind(dv3fv5, coords)
# fwrite(dv3fv5, "dv3fv5.csv")
dv3fv5 <- as.data.frame(dv3fv5)
connection <- nuvolos::get_connection()
DBI::dbWriteTable(connection, name="dv3fv5", value=dv3fv5)

nuvolos::to_sql(dv3fv5, name="dv3f_v5", if_exist="fail", index=TRUE, index_label="idmutation")

names(dv3fv5) |> sort()
