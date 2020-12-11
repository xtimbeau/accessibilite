# sélectionne le centre d'une distribution

selxth <- function(vecteur, xth = 0.01) {
  if (is.numeric(vecteur))
    {
  pv <- quantile(vecteur, c(xth / 2 , 1 - xth / 2) , na.rm = TRUE)
  (!is.na(vecteur))&vecteur>=pv[[1]]&vecteur<=pv[[2]]
  }
  else
    rep(TRUE, length(vecteur))
}

# renvoie une séquence espacée logarithmiquement

lseq <- function(from = 1,
                 to = 100000,
                 length.out = 6) {
  # logarithmic spaced sequence blatantly stolen from library("emdbook"), because need only this (pareil) un accent ?
  round(10 ^ (seq(log10(from), log10(to), length.out = length.out)))
}

# enregistre un rÃ©sultat (avec un timsptanp dans le nom de fichier) dans le rÃ©pertoire resultats

save_result <- function(x, nom = NULL) {
  require("rlang", quietly=TRUE)
  require("glue", quietly=TRUE)
  require("qs", quietly=TRUE)
  require("lubridate", quietly=TRUE)
  
  n_x <- rlang::as_name(substitute(x))
  if (is.null(nom))
    obj <- n_x
  else
    obj <- nom
  filename <- glue(
    "{localdata}/{obj} {timestamp}.rda"
    ,
    timestamp = lubridate::stamp("le 15 janvier 2020 10h08m05s", orders = "dmy HMS", quiet=TRUE) (lubridate::now())
  )
  qs::qsave(x, file = filename, preset="fast", nthreads = 4)
  invisible(filename)
}

# lit un résultat

load_result <- function(x) {
  require("glue", quietly=TRUE)
  require("qs", quietly=TRUE)
  filename <- glue("{localdata}/{x}.rda")
  qs::qread(filename, nthreads=4)
}

# enregistre un objet R dans le répertoire DVF

load_DVF <- function(str, rep="Rda", local=FALSE) {
  require("glue", quietly=TRUE)
  require("qs", quietly=TRUE)
  require("purrr", quietly=TRUE)
  env <- parent.frame()
  str <- glue(str, .envir=env)
  rep <- if(local) localdata else glue("{DVFdata}/{rep}")
  filename <- glue("{rep}/{str}.rda")
  what <- qs::qread(filename, nthreads = 4)
  if ("mods"%in%names(what)) 
    {
    mods <- map_chr(what$mods, ~{
      fn <- tempfile("xgb", tmpdir=tempdir())
      xgbmsave(.x,fn)
      fn})
    what$bp$xgbmod <- mods
    what$mods <- NULL
    }
  what
}

# lit un objet R depuis le répertoire DVF

save_DVF <- function(x, nom = NULL, rep="Rda", local=FALSE, preset="fast") {
  require("rlang", quietly=TRUE)
  require("glue", quietly=TRUE)
  require("stringr", quietly=TRUE)
  require("qs", quietly=TRUE)
  require("purrr", quietly=TRUE)
  require("lubridate", quietly=TRUE)
  ex <- enquo(x)
  n_x <- as_name(ex)
  rep_u <- if(local) localdata else str_c(DVFdata,"/",rep)
  dir.create(rep_u, showWarnings = FALSE)
  if (is.null(nom))
    {
    filename <- "{rep_u}/{n_x}.rda" %>% glue
    }
  else
    {
    env <- parent.frame()
    nom <- glue(nom, .envir=env) 
    filename <- "{rep_u}/{nom}.rda" %>% glue
    }
  if("bp"%in%names(x))
    x$mods <- map(x$bp$xgbmod, xgbmload)
  if(is.list(x)) suppressWarnings(x <- rapply(x, readAll, classes=c("RasterBrick", "RasterLayer", "RasterStack"), how="replace"))
  else if(is.raster(x)) suppressWarnings(readAll(x))
  qs::qsave(x, file = filename, preset=preset, nthreads = 4)
}

xgbmsave <- function(mod, fn)
{ 
  require("qs", quietly=TRUE)
  qs::qsave(mod, file=fn, nthreads=4)
}

xgbmload <- function(fn)
{ 
  require("qs", quietly=TRUE)
  require("xgboost", quietly=TRUE)
  if (!"xgb.Booster" %in% class(fn))
  {
    rds <- qs::qread(file=fn, nthreads=4)
    xgboost::xgb.Booster.complete(rds)
  }
  else 
    fn
}

lsave_DVF <- function(xgb, nom = NULL)
{
  save_DVF(xgb, nom=nom, local=TRUE)
}

lload_DVF <- function(x)
{
  require("glue", quietly=TRUE)
  require("qs", quietly=TRUE)
  require("purrr", quietly=TRUE)
  filename <- glue("{localdata}/{x}.rda")
  xgb <- qs::qread(filename, nthreads=4)
  mods <- map(xgb$mods, ~{
    fn <- tempfile("xgb", tmpdir=tempdir())
    xgbmsave(.x,fn)
    fn})
  xgb$bp$xgbmod <- mods
  xgb$mods <- NULL
  xgb
}
# lorsque le tiblle contient une géometry (sf) select garde la géométrie t_select l'oublie

selectt <- function(.data, ...)
  {
  require("dplyr", quietly=TRUE)
  as_tibble(.data) %>% dplyr::select(...)
}

graph2svg <- function(graph, file, height=16, width=20, textratio=2) {
  require("glue", quietly=TRUE)
  require("svglite", quietly=TRUE)
  require("grid", quietly=TRUE)
  require("stringr", quietly=TRUE)
  
  env <- parent.frame()
  file <- glue(file, .envir=env)
  svglite::svglite(file = str_c(file, ".svg"), height=height, width=width, pointsize = 9)
  print(graph, vp=grid::viewport(gp=grid::gpar(cex=textratio)))
  invisible(dev.off())
}

graph2jpg <- function(graph, file, height=2160, width=3840) {
  require("glue", quietly=TRUE)
  require("stringr", quietly=TRUE)
  require("grDevices", quietly=TRUE)
  
  env <- parent.frame()
  file <- glue(file, .envir=env)
  grDevices::jpeg(file = str_c(file, ".jpg"), height=height, width=width, res=300)
  print(graph)
  dev.off()
}

graph2pdf <- function(graph, file, height=8, width=10) {
  require("glue", quietly=TRUE)
  require("stringr", quietly=TRUE)
  require("grDevices", quietly=TRUE)
  
  env <- parent.frame()
  file <- glue(file, .envir=env)
  grDevices::pdf(file = str_c(file, ".pdf"), height=height, width=width)
  print(graph)
  dev.off()
}


basecol <- function(i,max=3) {
  require("scales", quietly=TRUE)
  hue_pal()(max)[[i]]
}

partition_by <- function(tbl, by)
{
  require("purrr", quietly=TRUE)
  require("dplyr", quietly=TRUE)
  
  tbl_temp <- tbl %>% mutate(rows= rowid())
  map_int(unique(tbl %>% pull(!!by)), ~(tbl_temp %>% filter((!!by)==.x) %>% pull(rows)))
}

showMemoryUse <- function(sort="size", decreasing=TRUE, limit=10, envir=parent.frame()) {
  require("utils", quietly=TRUE)

  objectList <- ls(envir=envir)

  oneKB <- 1024
  oneMB <- 1048576
  oneGB <- 1073741824

  memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x), envir=envir))))

  memListing <- sapply(memoryUse, function(size) {
    if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
    else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
    else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
    else return(paste(size, "bytes"))
  })

  memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)

  if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),]
  else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
  print(dim(memListing))
  memListing <- memListing[1:min(nrow(memListing), limit),]

  print(memListing, row.names=FALSE)
  return(invisible(memListing))
}

is.equal2 <- function(ch) {function(x) as.character(ch)==as.character(x)}

copy_tbl_attr <-  function(target, source) {
  require("purrr", quietly=TRUE)
  attrs <- map(source, ~attributes(.x))
  for(n in names(target))
      attributes(target[[n]])<-attrs[[n]]
  target
}

table.percent <- function(...)
  {
  tab <- table(...)
  round(tab/sum(tab)*100,1)
}

duplicate_rows <- function(x)
{
  duplicates <- unique(x[duplicated(x)])
  match(x, duplicates) %>% is.na %>% not
}

factor2num <- function(f)
  {
    if (is.factor(f))
    {
      if (!any(is.na(suppressWarnings(as.numeric(levels(f))))))
       as.numeric(levels(f))[f]
      else 
        as.numeric(f)
    }
     else if (is.logical(f))
       as.numeric(f)
  else 
    as.numeric(f)
  }

sample_index <- function(.data, n, replace=TRUE, exclude=NULL)
{
  index <- seq(1:nrow(.data))
  index <- setdiff(index, exclude)
  base::sample(index, size=n, replace=replace)
}

percent_signif <- function(x, digits=2)
{
  require("stringr", quietly=TRUE)
  str_c(signif(x,digits)*100, "%")
}

make_formula <- function(y,x)
{
  require("stringr", quietly=TRUE)
  as.formula(str_c(y, "~", str_c(x, collapse="+")))}

list.brace <- function(x, brace="{}", sep=",")
{
  require("stringr", quietly=TRUE)
  if (is.atomic(x)&&length(x)==1)
    return(str_c(x))
  else
    str_c(str_sub(brace,1,1), str_c(map(x, list.brace) , collapse=sep), str_sub(brace,2,2))
}

list.brace1 <- function(x, brace="{}", sep=",")
{
  require("stringr", quietly=TRUE)
  x[is.na(x)] <- "NA"
  str_c(str_sub(brace,1,1), str_c(x, collapse=sep), str_sub(brace,2,2))
}

list.unbrace <- function(x)
{
  require("stringr", quietly=TRUE)
  require("jsonlite", quietly=TRUE)
  x1 <- str_replace_all(str_replace_all(x, "\\}", "]"),  "\\{", "[")
  str_replace_all(x1, pattern="([:alnum:])", replacement='\\"\\1\\"') %>% fromJSON()
}

tabl <- function(data, var)
{
  require("dplyr", quietly=TRUE)
  data %>% as_tibble() %>% transmute("{{var}}" := {{var}}) %>% table(useNA="ifany")
}

f2si2<-function (number,rounding=TRUE, digits=1, unit="median") 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24) 
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  ix <- switch(unit,
         median = median(ix, na.rm=TRUE),
         max = max(ix, na.rm=TRUE),
         multi = ix)
  if (rounding==TRUE) 
      sistring <- paste0(round(number/lut[ix], digits), pre[ix])
    else  
      sistring <- paste0(number/lut[ix], pre[ix])
  return(sistring)
}

f2si2df <- function(df, string="", unit="multi")
{
  require("purrr", quietly=TRUE)
  require("stringr", quietly=TRUE)
  
  purrr::map(df, ~str_c(f2si2(.x, unit=unit), string, sep=" "))
}

if2si2 <- function(text)
{
  require("stringr", quietly=TRUE)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "1", "k", 
           "M", "G", "T", "P", "E", "Z", "Y")
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24) 
  names(lut) <- pre
  value <- str_extract(text, "[:digit:]+\\.?[:digit:]*") %>%
    as.numeric()
  unit <- str_extract(text, "(?<=[:digit:])[:alpha:]")
  unit[is.na(unit)] <- "1"
  value * lut[unit]
  }

uf2si2<-function (number,rounding=TRUE, unit="median") 
{
  n_number <- length(number)
  digits <- 1
  f2 <- f2si2(number, digits=digits, unit=unit)
  while(length(unique(f2))<n_number)
  {
    digits <- digits+1
    f2 <- f2si2(number, digits=digits, unit=unit)
  }
  f2 
}

list2strs <-  function(list)
{
  require("purrr", quietly=TRUE)
  require("stringr", quietly=TRUE)
  if(is.null(names(list)))
    toString(list)
  else
  {
    list <- purrr::imap_chr(list, ~stringr::str_c(.y, "=", .x))
    stringr::str_c(list, collapse=", ")
  }
}

second2str <- function(temps)
{
  require("stringr", quietly=TRUE)
  temps <- round(temps)
  jours <- temps%/%(3600*24)
  temps <- temps %% (3600*24)
  heures <- temps%/%3600
  temps <- temps%%3600
  minutes <- temps%/%60
  secondes <- temps%%60
  s <- character(0)
  if(jours>0)
    s <- str_c(jours, "j ")
  if(heures>0)
    s <- str_c(s, heures, "h ")
  if(minutes>0)
    s <- str_c(s, minutes, "m ")
  if(secondes>0)
    s <- str_c(s, secondes, "s")
  str_replace(s, " $", "")
}

pmemuse <- function (...)
{
  require("memuse", quietly=TRUE)
  require("glue", quietly=TRUE)
  current <- Sys.procmem()[[1]]
  size <- mu.size(current)
  unit <- mu.unit(current)
  glue("<{signif(size, 2)}{unit}>")
}

apply_consistent_y_lims <- function(this_plot)
  {
  require("ggplot2", quietly=TRUE)
  require("patchwork", quietly=TRUE)
  num_plots <- length(this_plot$layers)
  if(num_plots>1) {
    y_lims <- lapply(1:num_plots, function(x) ggplot_build(this_plot[[x]])$layout$panel_scales_y[[1]]$range$range)
    min_y <- min(unlist(y_lims))
    max_y <- max(unlist(y_lims))
    this_plot & ylim(min_y, max_y)
  }
  else
    this_plot
}