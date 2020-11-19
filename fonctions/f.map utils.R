plot_map_var <-
  function(data,
           var,
           type_var = NULL,
           resolution = 200,
           label=NULL,
           palette="-Spectral") {
    grid.r <-
      raster(xt_as_extent(data),
             crs = st_crs(data)$proj4string,
             resolution = resolution)
    r <- rasterize(
      x = data ,
      y = grid.r,
      field = var,
      fun = mean,
      background=0
    )
    if (!is.null(label)) text <- label else test <- var
    tm_shape(uu851.sf, bbox = bb851) + tm_borders(lwd = 0.25) + tm_fill(col = "gray80", alpha = 0.5) +
      tm_shape(didf.sf, bbox = bb851) + tm_borders(lwd = 0.25, col = "gray50") +
      tm_shape(r) + tm_raster(
        col = "layer",
        title = text,
        palette = palette,
        n = 10,
        style = "cont"
      )
  }


# cette fonction fonctionne uniquement si la colonne IdINSPIRE est présente
# il faut lui passer un tibble (ou un sf) avec cette colonne et une grille en sf avec également cette colonne


var_map2 <-
  function(data, grid, var,
           label=NULL, fun=mean,
           palette="-Spectral", resolution=200, ...) {
    quo_var <- enquo(var)
    quo_nm <- quo_name(quo_var)
    data.temp <- data %>%
      as_tibble %>%
      select(IdINSPIRE, !! quo_var)
    types <- data.temp %>%
      dplyr::summarise_all(class) %>%
      tidyr::gather(variable, class)
    
    if( types %>% filter(variable==quo_nm) %>% pull(class)=="factor")
      data.temp <- mutate(data.temp, !!quo_var :=as.numeric(as.character(!! quo_var)))
    data.temp <- data.temp %>%
      group_by(IdINSPIRE) %>%
      summarise( summary = (!!fun)(!! quo_var, na.rm=TRUE))
    
    data.temp <- left_join(data.temp, grid %>% as_tibble %>% dplyr::select(IdINSPIRE, geometry), by="IdINSPIRE") %>% st_as_sf
    resolution <-  round(sqrt(min(as.numeric(st_area(grid)))))
    raster.temp <- fasterize(sf=data.temp, raster=raster_ref(grid, resolution=resolution), fun="sum", field="summary")
    
    text.temp <-ifelse(!is.null(label), label, quo_name(quo_var))
    
    tm_shape(uu851.sf, bbox = bb851) + tm_borders(lwd = 0.25) + tm_fill(col = "gray80", alpha = 0.5) +
      tm_shape(didf.sf, bbox = bb851) + tm_borders(lwd = 0.25, col = "gray50") +
      tm_shape(raster.temp) + tm_raster(title = text.temp,   palette = palette, ...)
  }

rastermap <-
  function(data, var,
           label=NULL, # pour le graphe
           fun=mean, # opérateur d'aggrégation
           dropth = 0, # drop 1% des valeurs extrêmes
           palette=red2gray, style="kmeans", resolution=50, fdc=uu851.fdc, hdc=uu851.hdc,
           bbox=NULL, ...) {
    quo_var <- rlang::enquo(var)

    raster.temp <- rastervar(data=data, var={{var}}, fun=fun, dropth=dropth, resolution=resolution)
    
    text.temp <-ifelse(!is.null(label), label, quo_name(quo_var))
    fdc+
      tm_shape(raster.temp, bbox=bbox) +
      tm_raster(title = str_c("Dens. ", text.temp), style=style, palette = palette, ...)+
      hdc
  }


rastervar <-
  function(data, ...,
           fun=mean, # opérateur d'aggrégation
           dropth = 0, # drop 1% des valeurs extrêmes
           resolution=50) {
    quo_var <- rlang::enquos(...)
    idINS_n  <- str_c("IdINS_", resolution)
    if (!(idINS_n%in%names(data)))
      idinspire <- point_on_idINS(data %>% st_as_sf, resolution=resolution)
    else
      idinspire <- data[[idINS_n]]
    tic()
    data.temp <- map_dfc(quo_var, ~{data %>% as_tibble %>% dplyr::transmute(!!quo_name(.x) := !!.x)})
    data.temp <- data.temp %>% mutate(idINS=idinspire)
    typevar <- map_lgl(data.temp, is.numeric)
    data.temp <- data.temp %>% 
      mutate(across(-idINS,factor2num)) %>% 
      drop_na()
    cy_pos <- str_locate(data.temp[["idINS"]][1], "N(?=[0-9])")[,"start"]+1
    cx_pos <- str_locate(data.temp[["idINS"]][[1]], "E(?=[0-9])")[,"start"]+1
    lcoord <- cx_pos-cy_pos-1
    
    if (dropth>0) data.temp <- data.temp %>%
      mutate(across(which(typevar), ~ifelse(selxth(.x, dropth), .x, NA_real_)))
    
    data.temp <- data.temp %>% 
      group_by(idINS) %>%
      summarise(across(everything(), ~(!!fun)(.x, na.rm=TRUE))) %>% 
      mutate(Y=as.numeric(str_sub(idINS,cy_pos,cy_pos+lcoord)), 
             X=as.numeric(str_sub(idINS,cx_pos,cx_pos+lcoord)))
    xmin=min(data.temp$X)
    xmax=max(data.temp$X)
    ymin=min(data.temp$Y)
    ymax=max(data.temp$Y)
    crs <- sp::CRS(st_crs(3035)$proj4string)
    ext <- extent(xmin,xmax,ymin,ymax)
    nrows <- (ymax-ymin)/resolution
    ncols <- (xmax-xmin)/resolution
    points_m <- matrix(c(data.temp$X+resolution/2, data.temp$Y+resolution/2), ncol=2)
    sp <- SpatialPoints(points_m, proj4string = crs)
    datasp <- SpatialPointsDataFrame(sp, data.temp %>% select(-idINS, -X, -Y))
    ss <- raster::stack(map(names(datasp), 
              ~rasterize(x=datasp, y=raster(ext, nrow=nrows, ncol=ncols, crs=crs), fun="sum", field=.x)))
    names(ss) <- names(data.temp %>% select(-idINS, -X, -Y))
    return(ss)
    }

idINS2square <- function(ids, resolution=200)
{
  cy_pos <- str_locate(ids[[1]], "N(?=[0-9])")[,"start"]+1
  cx_pos <- str_locate(ids[[1]], "E(?=[0-9])")[,"start"]+1
  lcoord <- cx_pos-cy_pos-1
  y <- as.numeric(str_sub(ids,cy_pos,cy_pos+lcoord))
  x <- as.numeric(str_sub(ids,cx_pos,cx_pos+lcoord))
  pmap(list(x,y), ~st_polygon(
    list(matrix(
      c(..1, ..1+resolution,..1+resolution,..1, ..1,
        ..2, ..2, ..2+resolution,..2+resolution,..2),
      nrow=5, ncol=2)))) %>%
    st_sfc(crs=3035)
}

idINS2point <- function(ids, resolution=200)
{
  cy_pos <- str_locate(ids[[1]], "N(?=[0-9])")[,"start"]+1
  cx_pos <- str_locate(ids[[1]], "E(?=[0-9])")[,"start"]+1
  lcoord <- cx_pos-cy_pos-1
  y <- as.numeric(str_sub(ids,cy_pos,cy_pos+lcoord))
  x <- as.numeric(str_sub(ids,cx_pos,cx_pos+lcoord))
  m <- matrix(c(x+resolution/2,y+resolution/2), ncol=2)
  colnames(m) <- c("X", "Y")
  m
}

xt_point2square <- function(point, r=200)
{
  xy <- st_coordinates(point)
  x <- xy[[1]]
  y <- xy[[2]]
  st_polygon(list(matrix(c(x, x+r,x+r,x, y,y+r,y+r,y),nrow=4, ncol=2)))
}

make_idINSPIRE <- function(grid) {
  geom <-
    transpose(map(st_geometry(grid), function(gg) {
      m = as.matrix(gg)
      c(round(min(m[, 1])), round(min(m[, 2])))
    }))
  str_c("CRS3035RES200m", "N", geom[[2]], "E", geom[[1]])
}

point_on_idINS <- function(sf_point, resolution=200)
{
  if (!("sfc_POINT"%in%class(st_geometry(sf_point))))
  {
    sf_point <- st_centroid(sf_point)
  }
  if (st_crs(sf_point)$epsg!=3035) 
    sf_point <- st_transform(sf_point, 3035)
  xy <- st_coordinates(sf_point)
  idINS3035(xy[,1], xy[,2])
}

idINS3035 <- function(x, y=NULL, resolution=200)
{
  if(is.null(y))
  {
    y <- x[,2]
    x <- x[,1]
    }
  x <- floor(x / resolution )*resolution
  y <- floor(y / resolution )*resolution
  resultat <- stringr::str_c("N", y, "E", x)
  nas <- which(is.na(y)|is.na(x))
  if (length(nas)>0)
    resultat[nas] <- NA
  resultat
}

raster_ref <- function(sf, resolution) 
{
  b <- st_bbox(sf)
  ext <- extent(floor(b$xmin / resolution )*resolution,
                ceiling(b$xmax/resolution)*resolution,
                floor(b$ymin/resolution)*resolution,
                ceiling(b$ymax/resolution)*resolution)
  raster(ext, crs=st_crs(sf)$proj4string,  resolution=resolution)
}


croppedRaster <- function(x, na.value = NA)
{
  if(!is.na(na.value))
  {
    x[x == na.value] <- NA
  }
  if(canProcessInMemory(x, n = 2))
  {
    x.matrix <- is.na(as.matrix(x))
    colNotNA <- which(colSums(x.matrix) != nrow(x))
    rowNotNA <- which(rowSums(x.matrix) != ncol(x))
    
    croppedExtent <- extent(x, 
                            r1 = rowNotNA[1],
                            r2 = rowNotNA[length(rowNotNA)],
                            c1 = colNotNA[1], 
                            c2 = colNotNA[length(colNotNA)])
    
    crop(x, croppedExtent)
  } else
  {
    xNA <- is.na(x)
    colNotNA <- which(colSums(xNA) != nrow(x))
    rowNotNA <- which(rowSums(xNA) != ncol(x))
    
    croppedExtent <- extent(x, 
                            r1 = rowNotNA[1], 
                            r2 = rowNotNA[length(rowNotNA)],
                            c1 = colNotNA[1], 
                            c2 = colNotNA[length(colNotNA)])
    
    crop(x, croppedExtent)
  }
}

# renvoie l'extent (pour raster) d'un sf

xt_as_extent <- function(sf) {
  b <- st_bbox(sf)
  extent(b$xmin, 
         b$xmax,
         b$ymin, 
         b$ymax)
}

r2dt <- function(raster, res=NULL, fun=mean)
{
  vars <- names(raster) 
  xy <- raster::coordinates(raster)
  idINS <- idINS3035(xy[,1], xy[,2], resolution=raster::res(raster)[[1]])
  dt <- raster %>% as.data.frame %>% as.data.table
  dt <- dt[, `:=`(x=xy[,1], y=xy[,2], idINS=idINS)]
  dt <- na.omit(melt(dt, measure.vars=vars), "value")
  dt <- dcast(dt, x+y+idINS~variable, value.var="value")
  if(!is.null(res))
  {
    id <- str_c("idINS",res)
    dt[, (str_c("idINS",res)):=idINS3035(x,y,res)]
    dt <- dt[, lapply(.SD, fun), by=c(id), .SDcols=vars]
  }
  dt
}

raster_max <- function(sf1, sf2, resolution=200) {
  b1 <- st_bbox(sf1)
  b2 <- st_bbox(sf2 %>% st_transform(st_crs(sf1)))
  bb <- st_bbox(c(xmin = min(b1$xmin, b2$xmin),
                  xmax = max(b1$xmax, b2$xmax),
                  ymax = max(b1$ymax, b2$ymax),
                  ymin = min(b1$ymin, b2$ymin)),
                crs = st_crs(sf1))
  raster_ref(bb, resolution=resolution)}

