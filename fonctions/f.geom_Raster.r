geom_Raster <- function(raster, mapping=NULL, ..., long=FALSE, style="cont", k = 5)
{
  library(ggplot2)
  checkmate::assert(checkmate::checkMultiClass(raster, c("RasterLayer", "RasterBrick", "RasterStack")))
  coords <- raster::coordinates(raster)
  data <- setDT(as.data.frame(raster))
  vv <- names(raster)
  data[, `:=`(x=coords[,1],y=coords[,2])]
  nas <- reduce(vv, function(x,v) x & is.na(data[[v]]), .init=TRUE)
  data <- data[!nas]
  if(long)
  {
    data <- melt(data, measure.vars=vv, na.rm=TRUE)
    vv <- "value"
  }
  if(style=="kmeans")
    for(v in vv)
    {
      clus <- kbins(data[[v]], k=k, bins="factor")
      data[, (v):= clus]
    }
  list(geom_tile(data=data, mapping=modifyList(aes(x=x, y=y), mapping), ...), coord_sf(crs=st_crs(3035)))
}

kbins <- function(x, k=5, bins="factor")
{
  kmeans <- kmeans(x, centers=k, nstart=1L, iter.max = 1000, algorithm = "Lloyd")
  cc <- tibble(centers = as.vector(kmeans$centers), i = 1:nrow(kmeans$centers))
  cc <- cc %>% arrange(cc, centers) %>% mutate(ii=row_number()) %>% arrange(i) %>% pull(ii)
  res <- tibble( x = x, cluster=cc[kmeans$cluster])
  minmax <- res %>% 
    group_by(cluster) %>% 
    summarize(max=max(x), min=min(x), mean=mean(x), median=median(x))
  if(bins=="factor")
    return(factor(res$cluster, labels=str_c("[", uf2si2(minmax$min), ", ", uf2si2(minmax$max), "]")))
  if(bins=="mean")
    return(minmax$mean[res$cluster])
  if(bins=="median")
    return(minmax$median[res$cluster])
  if(bins=="cluster")
    return(res$cluster)
  if(bins=="fullcluster")
    return(list(x=res$cluster, labels=str_c("[", uf2si2(minmax$min), ", ", uf2si2(minmax$max), "]")))
  return(rep(NA, length(x)))
}