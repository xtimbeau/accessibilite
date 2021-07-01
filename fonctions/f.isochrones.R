add_va <- function(acc_r, pos, param, grid)
{
  #Attention, (x,y) ici mais il faut écrire (y, x) pour OTP
  isos <- do.call(otp_get_iso_mod, c(param, list(location = c(pos$Y, pos$X), format = "SF", fromLocation = TRUE)))

  if (isos$errorId=="OK") {
    intersections <- isos$response %>%
      st_transform(3035) %>%
      as_tibble %>%
      transmute(., time=time, raster = map(geometry, function(g)
        if (st_is_empty(g))
          zero_raster(grid)
        else
          rasterize(x=as(g, "Spatial"), y=grid, field=1L, background=0L)))
    raster_temp <- acc_r
    for (v in names(acc_r %>% select(-time)))
         raster_temp <- raster_temp %>% mutate(
           time=intersections$time,
           !!v:=map2(acc_r[[v]], intersections$raster, function(ar, r) ar+r*as.numeric(pos[[v]])))

  } else
    raster_temp <- acc_r
  raster_temp
}

iso_tib_to_raster <- function(tib, param,
                              grid)
{
  pb <- progress_bar$new(total = nrow(tib))
  list_var <- names(tib) %>% discard(~.x%in%c("X", "Y"))
  n_var <- length(list_var)

  n_col <-replicate(n_var,list(replicate(length(param$cutoff), zero_raster(grid))))
  names(n_col) <- list_var
  raster_accumule <- n_col %>% as_tibble %>% mutate(time=sort(desc(param$cutoff)))

  for (i in 1:nrow(tib))
    {
    pb$tick()
    une_pos <- tib %>% dplyr::slice(i)
    raster_accumule <- add_va(raster_accumule, une_pos, param, grid)
    }
  raster_accumule
}


zero_raster <- function(grid) {
  r0_0 <- raster(grid)
  values(r0_0) <- rep(0L,ncell(r0_0))
  r0_0}


# calcule et affiche les isochrones pour des coordonées WGS84

otp_isochrone <- function(lon, lat,                         # en coordonnées lon, lat
                          resolution= 50,
                          otp_core,                     # serveur OTP
                          mode=c("WALK", "TRANSIT"),    # TRANSIT, CAR, WALK etc...
                          date="17-12-2019 8:00:00",    # au format = "%d-%m-%Y %H:%M:%S"
                          max_walk_dist=2000,           # en mètres
                          temps_max=10L,                # en minutes
                          pas_de_temps=5L,
                          plot=FALSE,
                          nthreads=parallel::detectCores(logical=FALSE))

{
  ds <- str_split(date, " ") %>% unlist
  param <- list(
    otpcon=otp_core,
    date = ds[[1]],
    time = ds[[2]],
    cutoffs = seq(pas_de_temps,temps_max, by=pas_de_temps)*60,
    precisionMeters=resolution,
    mode=mode,
    format="SF",
    fromLocation=TRUE)
  
  isos <- do.call(otp_get_iso_mod, c(param, list(location = c(lat, lon), format = "SF", fromLocation = TRUE)))
  
  error <- isos$errorId != "OK"
  
  if (!error)
  {
    i_sf <- isos$response %>% st_transform(3035)
    valid_sf <- safe_make_valid(i_sf)
    if (is.null(valid_sf$error)) i_sf <- valid_sf$result else error <- TRUE
  }
  else 
  {
    message("erreur {error} {isos$response}" %>% glue)
    i_sf <- NULL
    }
  
  if (plot&!error)
  {
    map <- 
      tm_shape(tmaptools::read_osm(i_sf, type="osm"))+tm_rgb(saturation = 0)+
      tm_shape(i_sf)+tm_fill(fill="time")
    print(map)
  }
  
  invisible(i_sf)
}

# troisième version, utilise DT pour rèduire la consommation de mèmoire et passer sur les rèsolutions importantes
#
# Cette fonction rèalise une aggrègation d'une ou plusieurs variables localisèes (dans le tibble positions, toutes les variables numèriques sauf les colonnes X et Y)
# selon les isochrones. Cela permet de calculer un indice d'accessibilitè.
# la fonction retourne le rèsultat de l'aggrègation (un raster ou un DT) et l'aggrègation de chaque variable sur la totalitè des positions (pour mettre è l'èchelle)
# l'aggrègation est faite sur une grille (grid) qui est un raster (vide). Le raster dèfinit donc une resolution
# param contient les paramères pour la requète OTP. Les paramètres importants sont le jour, le mode, la rèsolution et les cutoffs
# todisk renvoie le rèsultat sous forme d'un fichier dans un fichier temporaire localisè dans le rèpertoire inrep
# la fonction peut retourner un data.table ou un raster suivant le paramètre return
# n_split ne sert è rien mais et lè pour la compatibilitè avec future_aggr_iosochrone_DT
# la fonction peut consommer pas mal de mèmoire lorsqu'on demande beaucoup de cutoff et beaucoup de variables pour une rèsolution èlevèe
 
aggr_isochrone_DT <- function (positions, param, grid,
                               progress=FALSE, n_split=1, todisk=0, timing=FALSE, return="DT", inrep=tempdir(), pbar=NULL, ...)
  {
  library(data.table, quietly=TRUE, warn.conflicts=FALSE)
  if (timing) tic()
  # if(progress) pb <- progress_bar$new(total = nrow(positions))
  if(is.null(pbar)) pbar <- progressor(nrow(positions))
  rasterOptions(maxmemory=5e+9, memfrac=0.9)

  list_var <- names(positions) %>% discard(~.x %in% c("X", "Y"))
  n_var <- length(list_var)
  acc_var <- rep(0L, n_var)
  names(acc_var) <- list_var
  n_layer <- length(param$cutoffs)
  timelist <- str_c("iso", round(param$cutoffs/60), "mn")

  DT_acc <- data.table()
  DT_acc[,(timelist):=rep(0L, grid@nrows*grid@ncols)]
  inter_DT <- copy(DT_acc)
  list_DT <- vector("list", n_var)
  for (i in 1:n_var) list_DT[[i]] <- copy(DT_acc)
  rm(DT_acc)
  names(list_DT) <- list_var

  resolution=(grid@extent@xmax-grid@extent@xmin)/grid@ncols

  safe_make_valid <- safely(sf::st_make_valid)
  safe_crop <- safely(raster::crop)
  safe_st_cast <- safely(sf::st_cast)
  
  errors <- tibble()

  for (i in 1:nrow(positions))
    {
    # if(progress) pb$tick()
    pbar()
    une_pos <- positions %>% dplyr::slice(i)

    coef <- une_pos[list_var]
    names(coef) <- list_var
    isos <- do.call(otp_get_iso_mod, c(param, list(location = c(une_pos$Y, une_pos$X), format = "SF", fromLocation = TRUE)))
    error <- isos$errorId != "OK"

    if (!error)
      {
      i_sf <- isos$response %>% st_transform(3035)
      valid_sf <- safe_make_valid(i_sf)
      if (is.null(valid_sf$error)) i_sf <- valid_sf$result else error <- TRUE
      }
    else
      {
      valid_sf <- NULL
      valid_sf$error <- NULL
      }

    if (!error)
      {
      names_i <- str_c(i_sf$time)
      i_geom <- i_sf$geometry
      names(i_geom) <- names_i
      i_geom <- purrr::map(i_geom, function(x) {
        if(st_is_empty(x))
          {castor <- NULL
          castor$error <- TRUE}
        else
          castor <- safe_st_cast(x, "MULTIPOLYGON")
        if (is.null(castor$error))
          castor$result
        else 
          st_point(x=c(une_pos$X, une_pos$Y)) %>%
             st_sfc(crs=4326) %>%
             st_transform(3035) %>%
             st_buffer(dist = resolution)})

      bboxs <- map(i_geom, function(x) alignExtent(xt_as_extent(x), grid, snap="out"))
      grids_cropped <- map(bboxs, function(x) safe_crop(grid, x, snap="out")$result)
      i_rst <- imap(i_geom, function(x,i) if (!is.null(grids_cropped[[i]]))
        {
        fasterize::fasterize(x %>% st_sfc %>% st_sf,grids_cropped[[i]],fun = "any", background = FALSE)
        }
                             else NULL
        )

      list_in <- purrr::imap(i_rst, function(x,i) if(!is.null(x))
        {
        raster::cellFromXY(grid,
                   raster::xyFromCell(grids_cropped[[i]], which(raster::values(x)==1)))
        }
                              else NULL
        )

      in_dt <- str_c("iso", round(names(list_in) %>% as.numeric /60), "mn")
      names(list_in) <- in_dt

      acc_var <- purrr::map_dbl(list_var, function(x) acc_var[[x]]+coef[[x]])
      names(acc_var) <- list_var
      for (var in list_var)
        for (j in timelist)
          list_DT[[var]] [list_in[[j]],(j):=get(j)+coef[[var]]]
    } # end de if(!error)
    else
    {
      errors <- bind_rows(errors,
                          tibble( X=une_pos$X,
                                  Y=une_pos$Y,
                                  err_iso=ifelse(is.null(isos$errorId),"no err", isos$errorId),
                                  err_valid=ifelse(is.null(valid_sf$error),"no err", as.character(valid_sf$error))))
    } # else de if(!error)
   } # for loop

    rm(inter_DT)

  if (return=="raster") {
    list_out <- map(list_var, function(dt) {
      br <- map(timelist, function(times) raster(list_DT[[dt]][[times]] %>% as.matrix, template=grid)) %>%
        brick
      names(br) <- timelist
      br
    })
    names(list_out) <- list_var
  } else
    list_out <- list_DT

  if (todisk!=0) {
    timestamp = Sys.time() %>% str_replace_all(":|-| ", "")
    filenames <- str_c(inrep, "/raster p", todisk, "_", list_var, "_", timestamp)
    names(filenames) <- list_var
    if (return=="raster")
      {
      walk2(list_out, filenames, function(x,y) writeRaster(x, y))
      out <- list(bricks=filenames)
      }
    else
      {
        walk2(list_out, filenames, function(x,y) fwrite(x, y, nThread=parallel::detectCores() %/% future::nbrOfWorkers()))
        out <- list(dt=filenames)
      }
  }
  else
    if (return=="raster") out <- list(bricks=list_out)
    else out <- list(dt=list_out)

  out <-  append(out, list(vars= acc_var, errors=errors))

  if (timing) {
    time <- toc(quiet = TRUE)
    dtime <- (time$toc - time$tic)
    message("{dtime%/%60}m {round(dtime-60*dtime%/%60)}s" %>% glue())
  }
  return(out)
}

# version parallèlisèe de la fonction aggr_isochrone
# le rèsulat est donc une liste de rasters (un par variable) à plusieurs couches (une par cutoff)
# todisk passe les rèsultat par le disque ce qui est plus prudent pour de grosses rèsolutions
# les paramètres sont les mèmes que pour aggr_isochrone
# la version accepte aussi une liste de serveurs diffèrents (si absente elle prend celui qui est dans param$otpc)

future_aggr_isochrone_DT <- function (positions, param, grid,
                                      progress=TRUE, n_split=nbrOfWorkers(),
                                      todisk=FALSE, inrep=tempdir(), timing=TRUE,
                                      otpserver=NULL)
{
  library(data.table, quietly=TRUE, warn.conflicts = FALSE)
  if (timing) tic()
  in_plan <- plan()
  rasterOptions(maxmemory=Inf, memfrac=0.75)
  
  pos_splitted <- positions %>%
    mutate(grp=rep(1:n_split, length.out=nrow(.))) %>%
    group_by(grp) %>%
    group_split(.keep=FALSE)

  names(pos_splitted) <- 1:length(pos_splitted)

  sessions <- unique(future_map_chr(1:(nbrOfWorkers()), ~future:::session_uuid()[[1]]))
  sessions <- unique(append(sessions, future:::session_uuid()[[1]]))
  load_bal <- if (is.null(otpserver)) 
    tibble(session=sessions, server=list(param$otpcon))
  else 
    tibble(session=sessions, server=rep_len(otpserver, length(sessions)))
  
  with_progress({
    pbar <- progressor(steps = nrow(positions))
    out_p <- future_map(
      pos_splitted, 
      function(pos)
        {
        l_param <- param
        ssi <- as.character(future:::session_uuid())
        l_server <- (filter(load_bal, session==!!ssi) %>% pull(server))
        if (!is_empty(l_server))
          l_param$otpcon <- l_server %>% pluck(1)
        else
          l_param$otpcon <- param$otpcon
        aggr_isochrone_DT(pos,
                          l_param,
                          grid,
                          progress=FALSE,
                          return="DT",
                          todisk=ifelse(todisk==0, 0, ssi),
                          inrep=inrep,
                          pbar=pbar)} # end of function(pos)
      )}, # end of with_progress expr
    handlers=handler_progress(format=":bar :percent", width=80))

  message("Démission des workers")
  plan(in_plan)

  message("Aggrégation")
  list_var <- names(positions) %>% discard(~.x %in% c("X", "Y"))
  n_var <- length(list_var)
  timelist <- str_c("iso", round(param$cutoffs/60), "mn")
  DT_z <- data.table()
  DT_z[,(timelist):=rep(0L, grid@nrows*grid@ncols)]
  out_DT <- vector("list", n_var)
  names(out_DT) <- list_var
  for (i in 1:n_var) out_DT[[i]] <- copy(DT_z)
  rm(DT_z)

  out_p <- purrr::transpose(out_p)
  for (i in 1:length(out_p$dt))
    for (var in names(out_p$dt[[i]]))
    {
      if (todisk) dt_i <- fread(out_p$dt[[i]][[var]])
      else dt_i <- out_p$dt[[i]][[var]]
      for (times in names(dt_i))
        out_DT[[var]] [, (times):= get(times)+dt_i[[times]]]
    }

  message("Rastérization")
  list_brick <- map(names(out_DT), function(dt) {
    br <- map(names(out_DT[[dt]]), function(times) raster(out_DT[[dt]][[times]] %>% as.matrix, template=grid)) %>%
      brick
    names(br) <- names(out_DT[[1]])
    br
  })
  names(list_brick) <- names(out_DT)
  
  errors <- bind_rows(out_p$errors)
  
  out <-list(
    bricks=list_brick,
    vars=map(purrr::transpose(out_p$vars), ~{
      red <- purrr::reduce(.x, `+`)
      names(red) <- names(.x[[1]])
      red}),
    errors=errors)

  rm(out_p, out_DT)

  out$bricks <- map(out$bricks, ~{if(fromDisk(.)) readAll(.)
    else .})

  if (timing) {
    time <- toc(quiet = TRUE)
    dtime <- (time$toc - time$tic)
    message("{dtime%/%60}m {round(dtime-60*dtime%/%60)}s" %>% glue())
  }
  return(out)
}

# cette fonction aggrège simplement les valeurs d'un sf cible sur les positions d'un autre sf à un certaine distance calculèe par OTP
# en utilisant les paramètres dèfinis dans param
# c'est donc un kernel à distance isochronique
# le paramètre res_fac indique le surèchantillonage fait pour calculer l'intersection (ce qui permet de prendre en compte les intersections non entières)
# la fonction crope pour minimiser les calculs et va donc assez vite

kernel_isochronique <- function (sf, positions, param, res_fac=1L, progress=FALSE, progress_fun=NULL, 
                                 resolution=round(sqrt(min(as.numeric(st_area(sf))))),...) {

  if(progress) pb <- progress_bar$new(total = nrow(positions))
  if(is.null(progress_fun)) progress_fun <- progressor(steps = nrow(positions))
  rasterOptions(maxmemory=Inf, memfrac=0.9) # pour empècher d'écrire sur le disque

  list_var <- names(select_if(st_drop_geometry(sf),is.numeric))
  n_var <- length(list_var)
  n_layer <- length(param$cutoffs)
  
  raster <- raster_max(sf, positions %>% st_as_sf(coords=c("X", "Y"), crs=4326), resolution/res_fac)

  rasters_var <- map(list_var,~fasterize(sf=sf, raster=raster, field=.x, background=0L))
  rasters_var <- map(rasters_var, ~calc(.x, fun=function(x) x/res_fac^2))
  names(rasters_var) <- list_var
  safe_make_valid <- safely(sf::st_make_valid)
  
  fforkernel <- function(i) {
    if(progress) pb$tick()
    progress_fun()
    une_pos <- positions %>% dplyr::slice(i)
    
    isos <- do.call(otp_get_iso_mod, c(param, list(location = c(une_pos$Y, une_pos$X), format = "SF", fromLocation = TRUE)))
    
    if (isos$errorId == "OK") 
    {
      i_sf <- isos$response %>% st_transform(3035)
      i_geom <- rlang::set_names(i_sf$geometry %>% unclass, str_c(isos$response$time))
      i_geom <- map(i_geom, ~
                      if (st_is_empty(.)) 
                      {(st_point(x=c(une_pos$X, une_pos$Y)) %>%
                          st_sfc(crs=4326) %>%
                          st_transform(3035) %>%
                          st_buffer(dist = resolution))}
                    else .)
      
      bboxs <- map(i_geom, ~alignExtent(xt_as_extent(.x), raster, snap="out"))
      grids_cropped <- map(bboxs, ~raster::crop(raster, .x, snap="out"))
      
      i_rst <- imap(i_geom, ~fasterize(.x %>% st_sfc %>% st_sf(crs=3035),
                                       grids_cropped[[.y]],
                                       fun = "any",
                                       background = FALSE))
      
      kernel <- map(rasters_var, function(v) {
        res <- map(i_rst, function(r) cellStats(r*crop(v,r), sum))
        names(res) <- names(i_rst)
        res})
      names(kernel) <- list_var
    }
    else 
    {
      layers <- map(str_c(param$cutoffs), ~NA_real_)
      names(layers) <- str_c(param$cutoffs)
      kernel <- map(list_var, ~layers)
      names(kernel) <- list_var
    }
    kernel
  }
  
  safe_fforkernel <- possibly(
    fforkernel,
    otherwise= {
      layers <- map(str_c(param$cutoffs), ~NA_real_)
      names(layers) <- str_c(param$cutoffs)
      kernel <- map(list_var, ~layers)
      names(kernel) <- list_var
      kernel
      },
    quiet=FALSE)
  
  kernels <- map(1:nrow(positions), ~safe_fforkernel(.x))
  kernels <- purrr::transpose(kernels) %>% as_tibble()
  kernels_unnested <- map_dfc(list_var, ~ kernels %>% 
                                dplyr::select(all_of(.x)) %>% 
                                unnest_wider(col = !!.x, names_sep = "_"))
  bind_cols(positions, kernels_unnested)
}


# cette reprend la précédente et permet d'utiliser un kernel circulaire, éventuellement pondéré par la distance et aussi 
# calculé à partir d'otp
# sf doit être sur une grille et donc il faut une colonne IdINS_xxx ou xxx est la résolution
# si la colonne n'est aps présente à la résolution demandée, elle est ajoutée

kernel_polychronique <- function (on_pos, sfdata, var, minutes=c(5,10,15), poids=NULL, 
                                  otp_p=NULL, fun=mean,
                                  resolution=50, kernel="otp", res_fac=1L,
                                  normalize=TRUE,
                                  progress=FALSE, pb = NULL, ...)
  {
  if(progress) pb <- progress_bar$new(total = nrow(on_pos))
  
  quo_var <- rlang::enquo(var)
  
  rasterOptions(maxmemory=Inf, memfrac=0.9) # pour empècher d'écrire sur le disque
  
  n_layer <- length(minutes)
  
  idINS_n  <- str_c("IdINS_", resolution)
  
  if (!(idINS_n%in%names(sfdata)))
  {
    idinspire <- point_on_idINS(sfdata, resolution=resolution)
    sfdata.temp <- sfdata %>%
      selectt(var2ras= !!quo_var) %>% 
      dplyr::mutate(idINS = idinspire) %>%
      drop_na()
  }
   else
    sfdata.temp <- sfdata %>%
      selectt(var2ras = !!quo_var, idINS=!!idINS_n) %>% 
      drop_na()
  
  cy_pos <- str_locate(sfdata.temp[["idINS"]][1], "N(?=[0-9])")[,"start"]+1
  cx_pos <- str_locate(sfdata.temp[["idINS"]][[1]], "E(?=[0-9])")[,"start"]+1
  lcoord <- cx_pos-cy_pos-1
  
  sfdata.temp <- sfdata.temp %>% 
    group_by(idINS) %>%
    summarise(summary = (!!fun)(var2ras, na.rm=TRUE)) %>% 
    mutate(Y=as.numeric(str_sub(idINS,cy_pos,cy_pos+lcoord)), 
           X=as.numeric(str_sub(idINS,cx_pos,cx_pos+lcoord)))
  xmin=min(sfdata.temp$X)
  xmax=max(sfdata.temp$X)
  ymin=min(sfdata.temp$Y)
  ymax=max(sfdata.temp$Y)
  
  pstring <- st_crs(3035)$proj4string
  ext <- extent(xmin,xmax,ymin,ymax)
  nrows <- (ymax-ymin)/(resolution/res_fac)
  ncols <- (xmax-xmin)/(resolution/res_fac)
  points_m <- matrix(c(sfdata.temp$X+resolution/2, sfdata.temp$Y+resolution/2), ncol=2)
  sp <- SpatialPoints(points_m, proj4string = rgdal::CRS(uprojargs=pstring))
  datasp <- SpatialPointsDataFrame(sp, sfdata.temp)
  rrr <- raster(nrows=nrows, ncols=ncols, ext=ext, crs=sp::CRS(uprojargs=pstring) , resolution=resolution/res_fac)
  rastered_var <- rasterize(x=datasp, y=rrr, fun="sum", field="summary")
  names(minutes) <- str_c("iso", minutes, "m")
  minutes <- minutes[sort(names(minutes))]
  rm(sfdata.temp, points_m, datasp)
  gc()
  
  kernels <- map(1:nrow(on_pos), function(i) {
    if (progress) pb$tick() else pb()
    
    une_pos <- on_pos %>% dplyr::slice(i)

    if (!any(is.na(as.list(une_pos))))
    {
    switch(kernel,
           otp={
             otp_p <- list_modify(otp_p, cutoffs=(minutes*60))
             isos <- do.call(otp_get_iso_mod, c(otp_p, list(location = c(une_pos$Y, une_pos$X), format = "SF", fromLocation = TRUE)))
           if (isos$errorId == "OK") {
             i_sf <- isos$response %>%
               st_transform(3035)
             names_i <- str_c("iso", i_sf$time/60, "m")
             i_geom <- i_sf$geometry
             names(i_geom) <- names_i
             i_geom <- map(names_i %>% sort, ~ if (st_is_empty(i_geom[[.]])) 
               {(st_point(x=c(une_pos$X, une_pos$Y)) %>%
                  st_sfc(crs=4326) %>%
                  st_transform(3035) %>%
                  st_buffer(dist = resolution))
             }
               else .)
             }
             else
               i_geom <- map(minutes, ~st_point(x=c(une_pos$X, une_pos$Y)) %>%
                               st_sfc(crs=4326) %>%
                               st_transform(3035) %>%
                               st_buffer(dist = .x*250/3))},
           circle={
             i_geom <- map(minutes,~st_point(x=c(une_pos$X, une_pos$Y)) %>%
                             st_sfc(crs=4326) %>%
                             st_transform(3035) %>%
                             st_buffer(dist = .x*250/3))
             },
           i_geom <- NULL)
      
      bboxs <- map(i_geom, ~alignExtent(xt_as_extent(.x), rrr, snap="out"))
      grids_cropped <- map(bboxs, ~raster::crop(rrr, .x, snap="out"))
      
      i_rst <- imap(i_geom, ~fasterize(.x %>% st_sfc %>% st_sf,
                                       grids_cropped[[.y]],
                                       fun = "any",
                                       background = FALSE))
      map(i_rst, function(r) {
        if (normalize) norm <- cellStats(r, sum) else norm <- 1
        cellStats(r*crop(rastered_var,r), sum)/norm
      })
    }
    else
      map(minutes, ~NA_real_)
  })
  bind_cols(on_pos %>% as_tibble, purrr::transpose(kernels) %>% as_tibble) %>% mutate_at(str_c("iso", minutes, "m"), unlist)
}

# parallèlisation de kernel_isochronique
# permet de sotcker temporairement les rasters sur le disque

future_kernel_isochronique <- function (sf, positions, param, res_fac=1L, n_split=8, progress=TRUE, tempraster=NULL,
                                        otpserver=NULL, resolution=round(sqrt(min(as.numeric(st_area(sf))))))
{
  tic()
  pos_splitted <- positions %>%
    mutate(grp=rep(1:n_split, length.out=nrow(.))) %>%
    group_by(grp) %>%
    group_split(.keep=FALSE)

  index <- 1:length(pos_splitted)
  
  sessions <- unique(future_map_chr(1:(nbrOfWorkers()), ~future:::session_uuid()))
  sessions <- unique(append(sessions, future:::session_uuid()))
  load_bal <- if (is.null(otpserver)) 
    tibble(session=sessions, server=list(param$otpcon))
  else 
    tibble(session=sessions, server=rep_len(otpserver, length(sessions)))
  
  with_progress({
    pb <- progressr::progressor(steps=nrow(positions))
    out <- future_map_dfr(pos_splitted,
                           ~{l_param <- param
                             l_server <- (filter(load_bal, session==as.character(future:::session_uuid())) %>% pull(server))
                             if (!is_empty(l_server)) 
                               l_param$otpcon <- l_server %>% pluck(1)
                             else
                               l_param$otpcon <- param$otpcon
                             if(!is.null(tempraster)) rasterOptions(tmpdir=tempraster)
                             kernel_isochronique(sf=sf,
                                                 positions=.x,
                                                 param=l_param, 
                                                 res_fac=res_fac, 
                                                 progress=FALSE,
                                                 progress_fun=pb,
                                                 resolution)})
    },
    enable=TRUE)

  removeTmpFiles(h=0.1)
  gc()
  time <- toc(quiet = TRUE)
  dtime <- (time$toc - time$tic)
  message("{dtime%/%60}m {round(dtime-60*dtime%/%60)}s" %>% glue())
  return(out)
}

future_kernel_polychronique <- function (on_pos, sfdata, var, minutes=c(5,10,15), poids=NULL, 
                                         otp_p=NULL, fun=mean,
                                         resolution=50, kernel="circle", res_fac=1L,
                                         normalize=TRUE,
                                         progress=TRUE, n_split=2*future::nbrOfWorkers(), otpserver=NULL)
{
  tic()
  
  q_var <- enquo(var)
  IdINS_n  <- str_c("IdINS_", resolution)
  
  pos_splitted <- on_pos %>%
    mutate(grp=rep(1:n_split, length.out=nrow(.))) %>%
    group_by(grp) %>%
    group_split(.keep=FALSE)
  
  if (!(IdINS_n%in%names(sfdata)))
  {
    idinspire <- point_on_idINS(sfdata, resolution=resolution)
    local_data <- sfdata %>%
      selectt(!!q_var) %>% 
      dplyr::mutate(!!IdINS_n := idinspire) %>%
      drop_na()
  }
  else 
    local_data <- sfdata %>% 
    selectt(!!q_var, starts_with(IdINS_n)) %>% 
    drop_na()
  
  if (!is.null(otp_p)) {
    sessions <- unique(future_map_chr(1:(nbrOfWorkers()), ~future:::session_uuid()[[1]]))
    sessions <- unique(append(sessions, future:::session_uuid()[[1]]))
    servers <- if (is.null(otpserver)) otp_p$otpcon else otpserver
    load_bal <- tibble(session=sessions, server=rep_len(servers, length(sessions)))
  }
  
  handlers(handler_progress(
    format   = ":bar :percent",
    width    = 60,
    complete = "="))
  
  with_progress({
    pb <- progressr::progressor(steps=length(on_pos))
    out <- future_map_dfr(pos_splitted,
      ~{
        if (!is.null(otp_p)) 
          {
          l_param <- otp_p
          l_server <- (filter(load_bal, session==future:::session_uuid()[[1]]) %>% pull(server))[[1]]
          if (!is_empty(l_server)) l_param$otpcon <- l_server else l_param$otpcon <- otp_p$otpcon
          }
        else 
          l_param=NULL
        kernel_polychronique(on_pos=.x, sfdata=local_data, var=!!q_var,
                             otp_p=l_param, kernel=kernel, fun=fun,
                             resolution=resolution, minutes=minutes,
                             res_fac=res_fac, normalize=normalize, 
                             progress=FALSE, pb=pb)})})
    
  removeTmpFiles(h=0.1)
  gc()
  time <- toc(quiet = TRUE)
  dtime <- (time$toc - time$tic)
  message("{dtime%/%60}m {round(dtime-60*dtime%/%60)}s" %>% glue())
  return(out)
}

# nouvelle version de l'aggrègateur d'isochrone è partir de Rcpp

aggr_isochrone_rcpp <- function (positions, param, grid, progress = FALSE, timing = FALSE) {

  # compteurs de temps
  if (timing) tic()
  if (progress) pb <- progress_bar$new(total = nrow(positions))

  # maximise la mèmoire disponible
  rasterOptions(maxmemory=Inf, memfrac=0.9)

  # initialise les variables
  list_var <- names(positions) %>% discard(~.x %in% c("X", "Y"))
  n_var <- length(list_var)
  n_layer <- length(param$cutoffs)
  brick_zero <- brick(nl = n_layer, nrows = grid@nrows,
                      ncols = grid@ncols, grid@extent, crs = grid@crs)
  values(brick_zero) <- 0L
  resultat <- vector("list", n_var)
  for (i in 1:n_var) resultat[[i]] <- brick_zero
  names(resultat) <- list_var

  # accumulation d'une position i
  for (i in 1:nrow(positions)) {

    if(progress) pb$tick()

    une_pos <- positions %>% dplyr::slice(i)

    coef <- une_pos[list_var]
    names(coef) <- list_var

    # calcul des isochrones avec un serveur OTP
    isos <- do.call(otp_get_iso_mod, c(param, list(location = c(une_pos$Y, une_pos$X), format = "SF", fromLocation = TRUE)))

    if (isos$errorId == "OK") {
      # récupération des isochrones
      i_sf <- isos$response %>% st_transform(3035)
      names_i <- str_c(i_sf$time)
      i_sf <- i_sf$geometry
      names(i_sf) <- names_i

      # croppage des isochrones sur la partie informative puis rasterisation
      bboxs <- map(i_sf, ~alignExtent(xt_as_extent(.x), grid, snap="out"))
      grids_cropped <- map(bboxs, ~raster::crop(grid, .x, snap="out"))
      i_rst <- imap(i_sf, ~fasterize(.x %>% st_sfc %>% st_sf,
                                     grids_cropped[[.y]],
                                     fun = "any",
                                     background = FALSE))

      # accumulations effectuèes è l'aide d'une fonction en c++
      for (j in list_var)
        for (k in 1:n_layer)
          resultat[[j]][[k]] <- AddValue(i_rst[[k]]@data@values, i_rst[[k]]@extent[1], i_rst[[k]]@extent[2],
                                         i_rst[[k]]@extent[3], i_rst[[k]]@extent[4], i_rst[[k]]@ncols, i_rst[[k]]@nrows,
                                         resultat[[j]][[k]]@data@values, resultat[[j]][[k]]@extent[1], resultat[[j]][[k]]@extent[2],
                                         resultat[[j]][[k]]@extent[3], resultat[[j]][[k]]@extent[4],
                                         resultat[[j]][[k]]@ncols, resultat[[j]][[k]]@nrows,
                                         coef[[j]])
    }
  }

  # fin du chrono
  if (timing) {
    time <- toc(quiet = TRUE)
    dtime <- (time$toc - time$tic)
    message("{dtime%/%60}mn {round(dtime-60*dtime%/%60)}s" %>% glue())
  }

  # mise en forme du résultat
  timelist <- str_c("iso", round(names_i %>% as.numeric /60), "mn")
  for (v in 1:n_var) {
    names(resultat[[v]]) <- timelist
    # mise à jour du slot valeur min et max du raster
    for (k in 1:n_layer) resultat[[v]][[k]] <- setMinMax(resultat[[v]][[k]])
  }

  return (resultat)
}


# comparaison de rasterlayers
comp_raster <- function (r1, r2) {

  if ( !"RasterLayer" %in% class(r1) | !"RasterLayer" %in% class(r2)) stop("Les objets ne sont pas des RasterLayers")

  if (!compareRaster(r1, r2, res = TRUE, stopiffalse = FALSE)) stop("Les rasters ont des propriètès diffèrentes")

  rv1 <- getValues(r1)
  rv2 <- getValues(r2)

  if (all(rv1 == rv2)) return("les rasters sont identiques")

  return(tibble(diff=rv2 - rv1, rv1=rv1, rv2=rv2))

}
