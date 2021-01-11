
message("Packages")
# packages utilisés

options("rgdal_show_exportToProj4_warnings" = "none")
options(java.parameters =c("-Xmx24G"))
gc()

dvfpackages <- c(
  "raster", "sf", "tmap",
  "patchwork", "extrafont",
  "microbenchmark", "tictoc", "profvis",
  "skimr", "furrr", "data.table", "lobstr",
  "magrittr", "glue", "tidyverse"
)

pkg <-
  lapply(dvfpackages, function(x) {
    if (!is.element(x, installed.packages())) {
      install.packages(x, quiet = TRUE)
    }
  })

dvfpackages <- intersect(dvfpackages, installed.packages())

# rend silencieux library
flibrary <- base::library
library <- function(...) suppressPackageStartupMessages(flibrary(...))

dvflib <-
  lapply(dvfpackages, function(x) {
    library(x, quietly = TRUE, character.only = TRUE)
  })

# multicore

plan(multisession, workers = availableCores() %/% 2)
message("{nbrOfWorkers()} workers" %>% glue())

options(dplyr.summarise.inform = FALSE)
rasterOptions(maxmemory = Inf, memfrac = 0.9)

rJava::.jinit()

# fonctions utilisées
message("Sources")

walk(
  list.files("./fonctions"),
  ~ source(str_c("./fonctions/", .x),
    encoding = "UTF-8"
  )
)

# # calibri
# font_import(prompt=FALSE)
# loadfonts(device="win")
theme_update(text = element_text(family = "Calibri", size = 9))
xgb_theme <- theme_minimal(base_size = 9, base_family = "Calibri")

tmap_options(fontfamily = "Calibri")

colorspace::sequential_hcl(n = 5, h = c(255, 255), c = c(85, 85, 50), l = c(40, 75), power = c(1, 2.25), register = "FanBlues")
colorspace::sequential_hcl(n = 5, h = c(230, 230), c = c(180, 180, 150), l = c(35, 70), power = c(0.7, 2.25), register = "FanBlues2")

# quelques fonctions pour les opérations sur les data.table#

dep75 <- c("75")
depPC <- c("75", "94", "92", "93")
depIdf <- c("75", "77", "78", "91", "92", "93", "94", "95")

red2gray <- colorspace::sequential_hcl(n = 50, h = c(15, 30), c = c(180, 90, 0), l = c(25, 100), power = c(1.5, 1.5), register = "red2gray", rev = TRUE)
green2gray <- colorspace::sequential_hcl(n = 50, h = c(115, 115), c = c(180, 90, 0), l = c(35, 100), power = c(2.25, 2.25), register = "green2gray", rev = TRUE)
blue2gray <- colorspace::sequential_hcl(n = 50, h = c(260, 220), c = c(180, 90, 0), l = c(30, 100), power = c(1.25, 1.25), register = "blue2gray", rev = TRUE)
heatrg <- colorspace::sequential_hcl(n = 50, h = c(-50, 180), c = c(60, 180, 20), l = c(30, 100), power = c(0.7, 1.5), register = "heatrg", rev = TRUE)
heatbg <- colorspace::sequential_hcl(n = 50, h = c(230, 80), c = c(110, 90, 60), l = c(25, 90), power = c(1, 1.2), register = "heatbg", rev = TRUE)
heatrb <- colorspace::diverging_hcl(n = 50, h = c(260, 15), c = c(120, 180), l = c(25, 95), power = c(1, 1), register = "heatvb", rev = TRUE)
heatgb <- colorspace::diverging_hcl(n = 50, h = c(200, 350), c = c(100, 80), l = c(50, 95), power = c(0.5, 1.5), register = "heatgb", rev = TRUE)

tmap_options(max.raster = c(plot = 1e+9, view = 1e+9))

options(future.globals.maxSize = 1024^3)

# chemin des fichiers sources
GD <- Sys.getenv("GOOGLE_DRIVE")
LD <- Sys.getenv("LOCAL_DATA")

if (GD == "") GD <- "G:/Mon Drive"
DVFdata <- "{GD}/DVFdata" %>% glue()
# répertoire local
localdata <- "{LD}/DVF" %>% glue()
fdc851 <- load_DVF("uu851")

progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))