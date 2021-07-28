# Cette fonction transforme un raster avec des couches donnant pour des temps donnés 
# en un raster layer qui donne les temps pour un seuil donné
# le seuil doit être donc atteint sur les couches (ou cela renverra NA)


isorenorme <- function(isoraster, facteur)
{
  isoraster/facteur
}

isAraster <- function(x)
{
  return((class(x)[1]=="RasterLayer" || class(x)[1]=="RasterBrick" || class(x)[1]=="RasterStack"))
}

iso2time <- function(isoraster, seuils=median(raster::cellStats(isoraster, median)))
{
  checkmate::assert(checkmate::checkMultiClass(isoraster, c("RasterLayer", "RasterBrick", "RasterStack")))
  seuils <- unique(seuils)
 isotimes <- names(isoraster) %>%
    stringr::str_extract("[:digit:]+") %>%
    as.numeric()
  mm <- isoraster %>%
    as.matrix()
  ncol <- ncol(mm)
  nrow <- nrow(mm)
   rr <- purrr::map(seuils, ~ {
    cc_moins <- max.col(mm<=.x, ties.method = "last")
    cc_plus <- max.col(mm>=.x, ties.method = "first")
    nnas <- !is.na(cc_moins)
    i_nnas <- which(nnas)
    ind_moins <- i_nnas +(cc_moins[nnas]-1)*nrow
    y_moins <- mm[ind_moins]
    y_plus <- mm[i_nnas +(cc_plus[nnas]-1)*nrow]
    out <- c(NA)
    length(out) <- nrow
    out[nnas] <- (.x-y_moins)/(y_plus-y_moins)*(isotimes[cc_plus[nnas]]-isotimes[cc_moins[nnas]])+isotimes[cc_moins[nnas]]
    out[nnas] [y_moins>=y_plus] <- NA   
    res <- raster::raster(isoraster)
    values(res) <- out
    res
  })
  rr <- brick(rr)
  names(rr) <- stringr::str_c(
    "to",
    uf2si2(seuils, rounding=FALSE, unit="multi")
    )
  rr  
}

iso2time_dt <- function(isodt, seuils=median(cellStats(isodt, median)), exclude=1)
{
  checkmate::assert(checkmate::checkMultiClass(isodt, c("data.table", "data.frame", "tibble")))
  isotimes <- names(isodt[,-(exclude),  with=FALSE]) %>%
    stringr::str_extract("[:digit:]+") %>%
    as.numeric()
  mm <- isodt[,-(exclude), with=FALSE] %>%
    as.matrix()
  ncol <- ncol(mm)
  nrow <- nrow(mm)
  rr <- purrr::map(seuils, ~ {
    cc_moins <- max.col(mm<=.x, ties.method = "last")
    cc_plus <- max.col(mm>=.x, ties.method = "first")
    nnas <- !is.na(cc_moins)
    i_nnas <- which(nnas)
    ind_moins <- i_nnas +(cc_moins[nnas]-1)*nrow
    y_moins <- mm[ind_moins]
    y_plus <- mm[i_nnas +(cc_plus[nnas]-1)*nrow]
    out <- c(NA)
    length(out) <- nrow
    out[nnas] <- (.x-y_moins)/(y_plus-y_moins)*(isotimes[cc_plus[nnas]]-isotimes[cc_moins[nnas]])+isotimes[cc_moins[nnas]]
    out[nnas] [y_moins>=y_plus] <- NA   
    out
  })
  names(rr) <- stringr::str_c("to",
    uf2si2(seuils, rounding=FALSE, unit="multi"))
  setDT(rr)
  cbind(isodt[,(exclude),  with=FALSE], rr)
}