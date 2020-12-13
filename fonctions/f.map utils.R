rastermap <-
  function(data, var,
           label=NULL, # pour le graphe
           fun=mean, # opérateur d'aggrégation
           dropth = 0, # drop 1% des valeurs extrêmes
           palette=red2gray, 
           style="kmeans", 
           resolution=50, 
           decor=NULL,
           bbox=NULL, ...) {
    library("tmap", quietly=TRUE)
    
    quo_var <- rlang::enquo(var)

    raster.temp <- rastervar(data=data, var={{var}}, fun=fun, dropth=dropth, resolution=resolution)
    
    text.temp <-ifelse(!is.null(label), label, quo_name(quo_var))
    decor$fdc+
      tm_shape(raster.temp, bbox=bbox) +
      tm_raster(title = str_c("Dens. ", text.temp), style=style, palette = palette, ...)+
      decor$hdc
  }


rastervar <-
  function(data, ...,
           fun=mean, # opérateur d'aggrégation
           dropth = 0, # drop 1% des valeurs extrêmes
           resolution=50, idINS="idINS") {
    
    library("data.table", quietly=TRUE)
    
    quo_var <- rlang::enquos(...)
    idinspire <- getINSres(data,resolution=resolution,idINS=idINS)
    if (any(idinspire==FALSE))
      idinspire <- idINS2point(data %>% st_as_sf, resolution=resolution)
    else
      idinspire <- data[[idinspire]] 

    data.temp <- purrr::map_dfc(quo_var, ~{data %>%
        dplyr::as_tibble() %>%
        dplyr::transmute(!!rlang::quo_name(.x) := !!.x)})

    data.table::setDT(data.temp)
    vars <- data.table::set_names(names(data.temp))
    isnum <- purrr::map_lgl(data.temp, is.numeric)
    data.temp <- data.temp[, lapply(.SD, factor2num)]
    obs_na <- purrr::map(vars, ~is.na(data.temp[[.x]]))
    obs_drop <- purrr::reduce(obs_na,and)
    
    data.temp <- data.temp[!obs_drop, idINS:=idinspire]
    
    if (dropth>0) 
      for(v in vars(isnum)) 
        data.temp[, (v):=ifelse(selxth(get(v), dropth), get(v), NA_real_)]

    data.temp <- data.temp[, lapply(.SD, function(x) fun(x, na.rm=TRUE)), by=idINS]
    
    dt2r(data.temp, resolution=resolution, idINS="idINS")
    }

idINS2square <- function(ids, resolution=NULL)
{
  cr_pos <- stringr::str_locate(ids[[1]], "r(?=[0-9])")[,"start"]+1
  cy_pos <- stringr::str_locate(ids[[1]], "N(?=[0-9])")[,"start"]+1
  cx_pos <- stringr::str_locate(ids[[1]], "E(?=[0-9])")[,"start"]+1
  lcoord <- cx_pos-cy_pos-1
  y <- as.numeric(stringr::str_sub(ids,cy_pos,cy_pos+lcoord))
  x <- as.numeric(stringr::str_sub(ids,cx_pos,cx_pos+lcoord))
  r <- if(is.null(resolution))
    as.numeric(stringr::str_sub(ids,cr_pos,cy_pos-cr_pos))
  else 
    rep(resolution, length(x))
  purrr::pmap(list(x,y,r), ~sf::st_polygon(
    list(matrix(
      c(..1, ..1+r..3,..1+..3,..1, ..1,
        ..2, ..2, ..2+..3,..2+..3,..2),
      nrow=5, ncol=2)))) %>%
    sf::st_sfc(crs=3035)
}

idINS2point <- function(ids, resolution=NULL)
{
  cr_pos <- stringr::str_locate(ids[[1]], "r(?=[0-9])")[,"start"]+1
  cy_pos <- stringr::str_locate(ids[[1]], "N(?=[0-9])")[,"start"]+1
  cx_pos <- stringr::str_locate(ids[[1]], "E(?=[0-9])")[,"start"]+1
  lcoord <- cx_pos-cy_pos-1
  y <- as.numeric(stringr::str_sub(ids,cy_pos,cy_pos+lcoord))
  x <- as.numeric(stringr::str_sub(ids,cx_pos,cx_pos+lcoord))
  r <- if(is.null(resolution))
    as.numeric(stringr::str_sub(ids,cr_pos,cy_pos-cr_pos))
  else 
    rep(resolution, length(x))
  m <- matrix(c(x+r/2,y+r/2), ncol=2)
  colnames(m) <- c("X", "Y")
  m
}

xt_point2square <- function(x, y=NULL, r=200, center=FALSE)
{
  if(is.null(y))
  {
   if(is.matrix(x))
    {
     y <- x[,2]
     x <- x[,1]
   }
    else 
      {xy <- sf::st_coordinates(point)
      x <- xy[[1]]
      y <- xy[[2]]}
  }
  if(center)
  {
    x <- x-r/2
    y <- y-r/2
    } 
 sf::st_polygon(list(matrix(c(x, x+r, x+r, x,   x,
                          y, y,   y+r, y+r, y),nrow=5, ncol=2)))
}

points2square <- function(x, y=NULL, r=200, center=FALSE)
{
  if(is.null(y))
  {
    if(is.matrix(x))
    {
      y <- x[,2]
      x <- x[,1]
    }
    else 
    {xy <- sf::st_coordinates(point)
    x <- xy[[1]]
    y <- xy[[2]]}
  }
  if(center)
  {
    x <- x-r/2
    y <- y-r/2
  } 
  purrr::map2(x,y, ~sf::st_polygon(list(matrix(c(x, x+r, x+r, x,   x,
                                      y, y,   y+r, y+r, y),nrow=5, ncol=2))))
}

make_idINSPIRE <- function(grid) {
  geom <-
    purr::transpose(
      purrr::map(
        sf::st_geometry(grid), function(gg) {
          m = as.matrix(gg)
          c(round(min(m[, 1])), round(min(m[, 2])))
          }))
  stringr::str_c("CRS3035RES200m", "N", geom[[2]], "E", geom[[1]])
}

point_on_idINS <- function(sf_point, resolution=200)
{
  if (!("sfc_POINT"%in%class(sf::st_geometry(sf_point))))
  {
    sf_point <- sf::st_centroid(sf_point)
  }
  if (sf::st_crs(sf_point)$epsg!=3035) 
    sf_point <- sf::st_transform(sf_point, 3035)
  xy <- sf::st_coordinates(sf_point)
  idINS3035(xy[,1], xy[,2], resolution)
}

idINS3035 <- function(x, y=NULL, resolution=200, resinstr=TRUE)
{
  if(is.null(y))
  {
    y <- x[,2]
    x <- x[,1]
  }
  resolution <- round(resolution)
  x <- floor(x / resolution )*resolution
  y <- floor(y / resolution )*resolution
  resultat <- if(resinstr)
    stringr::str_c("r", resolution, "N", y, "E", x)
  else 
    stringr::str_c("N", y, "E", x)
  nas <- which(is.na(y)|is.na(x))
  if (length(nas)>0)
    resultat[nas] <- NA
  resultat
}

raster_ref <- function(data, resolution=200, crs=3035) 
{
  alignres <- max(resolution, 200)
  if("sf"%in%class(data))
  {
    b <- sf::st_bbox(data)
    crs <- sf::st_crs(data)$proj4string
    }
  else
  {
    stopifnot("x"%in%names(data)&"y"%in%names(data))
    b <- list(xmin=min(data$x, na.rm=TRUE),
              xmax=max(data$x, na.rm=TRUE),
              ymin=min(data$y, na.rm=TRUE), 
              ymax=max(data$y, na.rm=TRUE))
    crs <- sp::CRS(SRS_string=glue::glue("EPSG:{crs}"))
  }
  ext <- raster::extent(floor(b$xmin / alignres )*alignres,
                ceiling(b$xmax/alignres)*alignres,
                floor(b$ymin/alignres)*alignres,
                ceiling(b$ymax/alignres)*alignres)
  raster::raster(ext, crs=crs,  resolution=resolution)
}


croppedRaster <- function(x, na.value = NA)
{
  if(!is.na(na.value))
  {
    x[x == na.value] <- NA
  }
  if(raster::canProcessInMemory(x, n = 2))
  {
    x.matrix <- is.na(as.matrix(x))
    colNotNA <- which(colSums(x.matrix) != nrow(x))
    rowNotNA <- which(rowSums(x.matrix) != ncol(x))
    
    croppedExtent <- raster::extent(x, 
                            r1 = rowNotNA[1],
                            r2 = rowNotNA[length(rowNotNA)],
                            c1 = colNotNA[1], 
                            c2 = colNotNA[length(colNotNA)])
    
    raster::crop(x, croppedExtent)
  } else
  {
    xNA <- is.na(x)
    colNotNA <- which(colSums(xNA) != nrow(x))
    rowNotNA <- which(rowSums(xNA) != ncol(x))
    
    croppedExtent <- raster::extent(x, 
                            r1 = rowNotNA[1], 
                            r2 = rowNotNA[length(rowNotNA)],
                            c1 = colNotNA[1], 
                            c2 = colNotNA[length(colNotNA)])
    
    raster::crop(x, croppedExtent)
  }
}

# renvoie l'extent (pour raster) d'un sf

xt_as_extent <- function(sf) {
  b <- sf::st_bbox(sf)
  sf::extent(b$xmin, 
         b$xmax,
         b$ymin, 
         b$ymax)
}
 
r2dt <- function(raster, resolution=NULL, fun=mean)
{
  library(data.table, quietly = TRUE)
  base_res <- max(raster::res(raster))
  vars <- names(raster)
  dt <- as.data.frame(raster, xy=TRUE, centroids=TRUE)
  data.table::setDT(dt)
  dt <- data.table::na.omit(data.table::melt(dt, measure.vars=vars), "value")
  dt <- data.table::dcast(dt, x+y~variable, value.var="value")
  dt[, idINS := idINS3035(x, y, resolution=base_res)]
  data.table::setnames(dt, "idINS",str_c("idINS", base_res))
  navars <- setdiff(vars, names(dt))
  rvars <- setdiff(vars, navars)
  if(!is.null(resolution))
  {
    id <- stringr::str_c("idINS",resolution)
    dt[, (stringr::str_c("idINS",resolution)):=idINS3035(x,y,resolution)]
    dt <- dt[, lapply(.SD, function(x) fun(x, na.rm=TRUE)), by=c(id), .SDcols=rvars]
  }
  if (length(navars)>0)
    dt[, (navars):=rep(list(rep(NA, nrow(dt))), length(navars))]
  dt
}

getresINS <- function(dt, idINS="idINS") {
  purrr::map(
    purrr::keep(names(dt), ~stringr::str_detect(.x,idINS)),
    ~{
      r<-stringr::str_extract(dt[[.x]], "(?<=r)[0-9]+") %>%
        as.numeric()
      ur <- unique(r)
      if (length(ur)==0)
        list(idINS=.x, res=NA_integer_)
      else
        list(idINS=.x, res=ur)
    })
}

getINSres <- function(dt, resolution, idINS="idINS") {
  rr <- getresINS(dt, idINS)
  ncol <- names(dt)
  if(length(rr)==0)
    return(FALSE)
  isresin <- purrr::map_lgl(rr, ~.x[["res"]]==resolution)
  if(any(isresin))
    return(rr[which(isresin)][[1]]$idINS)
  else 
    return(FALSE)
  }

dt2r <- function(dt, resolution=NULL, idINS="idINS") 
  {
  library(data.table, quietly = TRUE)
  rr <- getresINS(dt, idINS)
  ncol <- names(dt)
  if(length(rr)==0)
    idINSin <- FALSE
  else
  {
    if(!is.null(resolution))
      isresin <- purrr::map_lgl(rr, ~.x[["res"]]==resolution)
    else
      isresin <- c(TRUE, rep(FALSE, length(rr)-1))
    if(any(isresin))
      {
      res <- rr[which(isresin)][[1]]$res
      idINS <-rr[which(isresin)][[1]]$idINS
      idINSin <- TRUE
    }
    else
      idINSin <- FALSE
    }
  if (!idINSin)
  {
    stopifnot(!is.null(res))
    stopifnot("x"%in%ncol&"y"%in%ncol)
    dt[, idINS:=idINS3035(x,y,resolution=resolution)]
    idINS <- "idINS"
    res <- resolution
  }
  xy <- idINS2point(dt$idINS, resolution = res)
  dt[, `:=`(x=xy[,1], y=xy[,2])]
  rref <- raster_ref(dt, resolution = res, crs=3035)
  cells <- raster::cellFromXY(rref, xy)
  layers <- purrr::keep(ncol, ~(is.numeric(dt[[.x]]))&(!.x%in%c("x","y")))
  brickette <- raster::brick(
    purrr::map(layers,
      ~{
        r <- raster(rref)
        r[cells] <- dt[[.x]]
        r
        }))
  names(brickette) <- layers
  brickette
  }

raster_max <- function(sf1, sf2, resolution=200) {
  library("sf", quietly=TRUE)
  b1 <- sf::st_bbox(sf1)
  b2 <- sf::st_bbox(sf2 %>% sf::st_transform(st_crs(sf1)))
  bb <- sf::st_bbox(c(xmin = min(b1$xmin, b2$xmin),
                  xmax = max(b1$xmax, b2$xmax),
                  ymax = max(b1$ymax, b2$ymax),
                  ymin = min(b1$ymin, b2$ymin)),
                crs = sf::st_crs(sf1))
  raster_ref(bb, resolution=resolution)}

projectrgb <- function(rgb, crs="3035")
{
  maxs <- raster::cellStats(rgb, max)
  rgbp <- raster::projectRaster(from=rgb, crs=CRS("EPSG:{crs}" %>% glue::glue)) # la projection fait un truc bizarre sur les entiers
  rgbp <- rgbp/raster::cellStats(rgbp, max)*maxs %>%
    round() # on remet tout comme avant mais en 3035
  rgbp
}