# Cette fonction transforme un raster avec des couches donnant pour des temps donnés 
# en un raster layer qui donne les temps pour un seuil donné
# le seuil doit être donc atteint sur les couches (ou cela renverra NA)


iso2time_o <- function(isoraster, seuils=median(raster::cellStats(isoraster, median)))
{
  library("checkmate", quietly=TRUE)
  library("progressr", quietly=TRUE)
  library("stringr", quietly=TRUE)
  library("purrr", quietly=TRUE)
  library("raster", quietly=TRUE)
  
  assert(checkClass(isoraster, c("RasterLayer", "RasterBrick", "RatserStack")))
  
  fisoinv <- function(x, isotimes, seuil)
  {
    xmax <- max(x, na.rm=TRUE)
    xmin <- min(x, na.rm=TRUE)
    
    if(xmin<xmax&&xmax>=seuil&&xmin<=seuil)
      {
      approx(y=isotimes, x=as.vector(x), xout=seuil)[["y"]]
      }
    else 
      NA
  }
  
  isotimes <- names(isoraster) %>% str_extract("[:digit:]+") %>% as.numeric()
  
  with_progress(
    {
      pb <- progressor(steps=length(seuils))
      rr <- map(seuils, ~ {
        ff <- substitute(function(x) fisoinv(x, isotimes=ii, seuil=ss), list(ii=isotimes, ss=.x))
        ff <- compiler::cmpfun(eval(ff))
        bb <- raster::calc(isoraster, fun=ff)
        pb()
        bb
        })
      },
    handlers=handler_progress(format=":bar :percent", width=80))
  
  rr <- brick(rr)
  names(rr) <- str_c("to", uf2si2(seuils, unit="multi"))
  gc()
  rr  
  } 

isorenorme <- function(isoraster, facteur)
{
  isoraster/facteur
}

isAraster <- function(x)
{
  return((class(x)[1]=="RasterLayer" || class(x)[1]=="RasterBrick" || class(x)[1]=="RasterStack"))
}

iso2time <- function(isoraster, seuils=median(cellStats(isoraster, median)))
{
  library("checkmate", quietly=TRUE)
  library("progressr", quietly=TRUE)
  library("stringr", quietly=TRUE)
  library("purrr", quietly=TRUE)
  library("raster", quietly=TRUE)
  
  assert(checkMultiClass(isoraster, c("RasterLayer", "RasterBrick", "RasterStack")))
  isotimes <- names(isoraster) %>% str_extract("[:digit:]+") %>% as.numeric()
  mm <- isoraster %>% as.matrix
  ncol <- ncol(mm)
  nrow <- nrow(mm)
  with_progress(
    {
      pb <- progressor(steps=length(seuils))
      rr <- map(seuils, ~ {
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
        res <- raster(isoraster)
        values(res) <- out
        pb
        res
        })
      })
      rr <- brick(rr)
      names(rr) <- str_c("to", uf2si2(seuils, rounding=FALSE, unit="multi"))
      rr  
}
