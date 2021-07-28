idINS3035 <- function(x, y=NULL, resolution=200, resinstr=TRUE)
{
  if(is.null(y))
  {
    y <- x[,2]
    x <- x[,1]
  }
  resolution <- round(resolution)
  x <-formatC(floor(x / resolution )*resolution, format="d")
  y <-formatC(floor(y / resolution )*resolution, format="d")
  resultat <- if(resinstr)
    stringr::str_c("r", resolution, "N", y, "E", x)
  else 
    stringr::str_c("N", y, "E", x)
  nas <- which(is.na(y)|is.na(x))
  if (length(nas)>0)
    resultat[nas] <- NA
  resultat
}