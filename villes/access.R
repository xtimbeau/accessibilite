library(googledrive)
library(qs)
library(tidyverse)
library(sf)
library(data.table)
library(vroom)
library(stars)
library(nuvolos)
library(dbplyr)
source("init.r")

sources <- "https://drive.google.com/drive/folders/12bqsgtWB8-Er_zCKYm-2yuXOrxAu0FM5" |> as_id()
accessvilles <- "https://drive.google.com/drive/folders/1P7ZJtrhhYpiSxX5V8RTLw0YflzObjFUe" |> as_id()
dv3fv5.id <- "https://drive.google.com/drive/folders/1-j3sUqQI9HKqaWEudALgsoiNy-lI1cnU" |> as_id()
drive_auth(email="xavier.timbeau@sciencespo.fr", cache="secrets", use_oob = TRUE)

drive_download("https://drive.google.com/file/d/1pKrAu5j0LWg2DHQu0eCFKRE_ZYAYBDDV", overwrite = TRUE) # c200
drive_download("https://drive.google.com/file/d/1-Z4zca3GwKhX1u7zra5raatUwzTuMzw5", path="dv3fv5_idf.rda") # dv3f_v5_idf

c200 <- qread("c200.rda")

dv3fv5_idf <- qread("dv3fv5_idf.rda")
setDT(dv3fv5_idf)

# dv3fv5 <- fread("dv3fv5.csv", select = c("lon", "lat"))
dv3fv5 <- vroom("~/files/dv3fv5.csv", col_types = cols_only(
  idmutation="c", idnatmut="c",
  datemut="D", anneemut="i",  moismut="i",      
  coddep="c", libnatmut="c", vefa="l",
  valeurfonc="d", nbsuf="i",
  sterr="d", nblocmai="i", nblocapt="i",
  nblocdep="i", nblocact="i", sbati="d", sbatmai="d",
  sbatapt="d", sbatact="d", 
  codtypbien="f",
  libtypbien="c", filtre="c",
  devenir="f", nbdispo="i", nblot="i", nblocmut="i" , nblocdep="i", 
  lon="d", lat="d"))

names(vroom("dv3fv5.csv",n_max=2 ))

cols <- c("idnatmut", "datemut", "anneemut",  "moismut",      
         "coddep", "libnatmut", "vefa", "valeurfonc", "sterr", "sbati", "codtypbien", "libtypbien", "filtre",
         "devenir", "lon", "lat")
COLS <- toupper(cols)

# alternative, par SQL et la table distribuée
con <- get_connection()
dv3fv5 <- dbGetQuery(con, "SELECT {cols} FROM dv3f_v5" |> glue()) |>
  rename_with(tolower)
  

# alternative 2, using dbplyr et la table distribuée
db <- tbl(con, "DV3F_V5") 
dv3fv5 <- db |> dplyr::select(all_of(COLS)) |> rename_with(tolower)

dv3fv5 <- dv3fv5 |> 
  collect() |> 
  mutate(
    typebien = case_when(
      str_detect(codtypbien, "^111") ~ "maison",
      str_detect(codtypbien, "^121") ~ "appartement",
      str_detect(codtypbien, "^11") ~ "maisons",
      str_detect(codtypbien, "^12") ~ "appartements",
      str_detect(codtypbien, "^13") ~ "dépendances",
      str_detect(codtypbien, "^14") ~ "activité",
      str_detect(codtypbien, "^15") ~ "mixte",
      str_detect(codtypbien, "^2") ~ "terrain", 
      TRUE ~"autres"),
    surface = sbati,
    prixm2 = valeurfonc/surface,
    idINS = idINS3035(sf_project(from=st_crs(4326), to=st_crs(3035), cbind(.data$lon, .data$lat))))

dv3fv5 |> 
  filter(typebien %in% c("maison", "appartement"), filtre=="0") |> 
  group_by(coddep, typebien) |> 
  summarize(prixm2_m = median(prixm2, na.rm=TRUE),
            prixm2_25 = quantile(prixm2, 0.25, na.rm=TRUE),
            prixm2_75 = quantile(prixm2, 0.75, na.rm=TRUE))

access <- drive_ls(accessvilles)
setwd("./villes")
walk(access |> filter(str_detect(name, "access200")) |> pull(id), ~drive_download(as_id(.x), overwrite = TRUE))
empsf<- list.files(pattern="^access200_")
emps <- map(empsf, ~{
  dt <- qread(.x)$emplois |> iso2time(c(25000, 50000, 100000, 250000)) |> r2dt()
  dt[, ville := str_remove(str_remove(.x, "access200_"), ".rda")]
}) |> rbindlist()
setnames(emps, c("idINS200"), c("idINS"))

setDT(dv3fv5)

villes <- merge(dv3fv5, emps, by="idINS")

villes_200 <- villes[typebien %in% c("appartement") & filtre=="0", .(prixm2_m = median(prixm2, na.rm=TRUE),
                                                                 prixm2_25 = quantile(prixm2, 0.25, na.rm=TRUE),
                                                                 prixm2_75 = quantile(prixm2, 0.75, na.rm=TRUE),
                                                                 to25k = first(to25k),
                                                                 to50k = first(to50k),
                                                                 to100k = first(to100k),
                                                                 to250k = first(to250k),
                                                                 ville = first(ville)), by=idINS]

ggplot(villes_200 |> slice_sample(n=20000)) + geom_point(aes(x=to50k, y=prixm2_m, col=ville), alpha =0.5) + xlim(c(0,60)) + scale_y_log10(limits= c(500, 15000))

ggplot(villes_200 |> filter(ville!="Paris") 
       |> slice_sample(n=20000) |> drop_na(to250k)) + geom_point(aes(x=to250k, y=prixm2_m, col=ville), alpha =0.5) + scale_y_log10(limits= c(500, 15000))



ggplot(villes_200)+ geom_point(aes(x=to50k, y=prixm2_m, col=ville), alpha =0.1) + xlim(c(0,60)) + scale_y_log10(limits= c(500, 15000))+facet_wrap(vars(ville))

ggplot(villes_200)+ geom_point(aes(x=to25k, y=prixm2_m, col=ville), alpha =0.1) + xlim(c(0,60)) + scale_y_log10(limits= c(500, 15000))+facet_wrap(vars(ville))


villes |> filter(typebien %in% c("maison", "appartement"), filtre=="0") |> 
  group_by(idINS, typebien) |> 
  summarize(prixm2_m = median(prixm2, na.rm=TRUE),
            prixm2_25 = quantile(prixm2, 0.25, na.rm=TRUE),
            prixm2_75 = quantile(prixm2, 0.75, na.rm=TRUE))

idf_200 <- dv3fv5_idf[(maison|appartement) & filtre=="0", .(prixm2_m = median(vm, na.rm=TRUE),
                                                                               prixm2_25 = quantile(vm, 0.25, na.rm=TRUE),
                                                                               prixm2_75 = quantile(vm, 0.75, na.rm=TRUE),
                                                                               emplois = first(t2emp17_50k)),
                                                                               by=idINS200]
