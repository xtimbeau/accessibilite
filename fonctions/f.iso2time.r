# Cette fonction transforme un raster avec des couches donnant pour des temps donnés 
# en un raster layer qui donne les temps pour un seuil donné
# le seuil doit être donc atteint sur les couches (ou cela renverra NA)


iso2time <- function(isoraster, seuils)
{
  assertthat::assert_that(is.raster(isoraster)) 
  
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
  names(rr) <- str_c("to", f2si2(seuils))
  gc()
  rr  
  } 

isorenorme <- function(isoraster, facteur)
{
  isoraster/facteur
}

is.raster <- function(x)
{
  return((class(x)[1]=="RasterLayer" || class(x)[1]=="RasterBrick" || class(x)[1]=="RasterStack"))
}