
message("Packages")
# packages utilisés

options("rgdal_show_exportToProj4_warnings"="none")

dvfpackages <- c(
   "matrixStats",
   "RhpcBLASctl",
  "rgdal", "rgeos", "raster", "fasterize", "sf", "geojsonsf",
  "leaflet", "leaflet.extras", "rdist",
  "tmap", "tmaptools",  "mapview",
  "scales", "extrafont", "colorspace",
  "patchwork",
  "r5r", "orsm", "rlist",
  "glue", "qs", "otpr", "logger",
  "microbenchmark", "tictoc", "profvis", "progressr", "progress",
  "Rcpp", "skimr", "future", "foreach", "doFuture", "furrr",
  "data.table", "lobstr",  "lubridate", "magrittr",  "tidyverse", "vroom", "dtplyr", "rlang")

pkg <-
  lapply(dvfpackages, function(x)
    if (!is.element(x, installed.packages()))
      install.packages(x, quiet = TRUE))

dvfpackages <- intersect(dvfpackages, installed.packages())

dvflib <-
  lapply(dvfpackages, function(x)
    library(x, quietly = TRUE, character.only = TRUE))

# multicore

blas_set_num_threads(4)
omp_set_num_threads(4)
plan(multicore, workers= availableCores()%/%4)
message("{nbrOfWorkers()} workers" %>% glue)

options(dplyr.summarise.inform=FALSE)
rasterOptions(maxmemory=Inf, memfrac=0.9)
registerDoFuture()

options(java.parameters = "-Xmx48G" )
rJava::.jinit()

# fonctions utilisées
message("Sources")

source("./fonctions/f.communs.R", encoding="UTF-8")
source("./fonctions/f.isochrones.R", encoding="UTF-8")
source("./fonctions/f.optgetmod.r", encoding="UTF-8")
source("./fonctions/f.map utils.r", encoding="UTF-8")
source("./fonctions/f.accessibilite.r", encoding="UTF-8")
source("./fonctions/f.iso2time.r", encoding="UTF-8")

# # calibri
# font_import(prompt=FALSE)
# loadfonts(device="win")

# theme_update(text = element_text(family = "Calibri", size = 9))
xgb_theme <- theme_minimal(base_size=9, base_family = "Calibri")

tmap_options(fontfamily = "Calibri")

colorspace::sequential_hcl(n = 5, h = c(255, 255), c = c(85, 85, 50), l = c(40, 75), power = c(1, 2.25), register = "FanBlues")
colorspace::sequential_hcl(n = 5, h = c(230,230), c = c(180, 180, 150), l = c(35, 70), power = c(0.7, 2.25), register = "FanBlues2")

# quelques fonctions pour les opérations sur les data.table#

dep75 <- c("75")
depPC <- c("75", "94", "92", "93")
depIdf <- c("75", "77", "78", "91", "92", "93", "94", "95")

red2gray <- colorspace::sequential_hcl(n = 50, h = c(15, 30), c = c(180, 90, 0), l = c(25, 100), power = c(1.5, 1.5), register = "red2gray", rev=TRUE)
green2gray <- colorspace::sequential_hcl(n = 50, h = c(115, 115), c = c(180, 90, 0), l = c(35, 100), power = c(2.25, 2.25), register = "green2gray", rev=TRUE)
blue2gray <- colorspace::sequential_hcl(n = 50, h = c(260, 220), c = c(180, 90, 0), l = c(30, 100), power = c(1.25, 1.25), register = "blue2gray", rev=TRUE)
heatrg <- colorspace::sequential_hcl(n = 50, h = c(15, 115), c = c(180, 180, 180), l = c(25, 100), power = c(1, 2.5), register = "heatrg", rev=TRUE)
heatbg <- colorspace::sequential_hcl(n = 50, h = c(300, 200), c = c(150, 180, 0), l = c(25, 95), power = c(1, 2.5), register = "heatbg", rev=TRUE)
heatvb <- colorspace::sequential_hcl(n = 50, h = c(280, 210), c = c(180, 90, 0), l = c(30, 95), power = c(1,0.75), register = "heatvb", rev=TRUE)

tmap_options(max.raster = c(plot = 1e+9, view = 1e+9))

options(future.globals.maxSize= 1024^3)

# chemin des fichiers sources
GD <- Sys.getenv("GOOGLE_DRIVE")[[1]]
LD <- Sys.getenv("LOCAL_DATA")[[1]]
if (GD=="") GD <- "G:/Mon Drive"
DVFdata <- "{GD}/DVFdata" %>% glue
# répertoire local
localdata <- "{LD}/DVF" %>% glue
# fdc_uu851 <- load_DVF("uu851")
# uu851.fdc <- fdc_uu851$fdc
# uu851.hdc <- fdc_uu851$hdc
# bb851 <- fdc_uu851$bbox
