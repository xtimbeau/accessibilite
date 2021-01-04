# sélectionne le centre d'une distribution

selxth <- function(vecteur, xth = 0.01) {
  if (is.numeric(vecteur)) {
    pv <- quantile(vecteur, c(xth / 2, 1 - xth / 2), na.rm = TRUE)
    (!is.na(vecteur)) & vecteur >= pv[[1]] & vecteur <= pv[[2]]
  }
  else {
    rep(TRUE, length(vecteur))
  }
}

# renvoie une séquence espacée logarithmiquement

lseq <- function(from = 1,
                 to = 100000,
                 length.out = 6) {
  # logarithmic spaced sequence blatantly stolen from library("emdbook"), because need only this (pareil) un accent ?
  round(10^(seq(log10(from), log10(to), length.out = length.out)))
}

# enregistre un rÃ©sultat (avec un timsptanp dans le nom de fichier) dans le rÃ©pertoire resultats

save_result <- function(x, nom = NULL) {
  n_x <- rlang::as_name(substitute(x))
  if (is.null(nom)) {
    obj <- n_x
  } else {
    obj <- nom
  }
  filename <- glue::glue(
    "{localdata}/{obj} {timestamp}.rda",
    timestamp = lubridate::stamp("le 15 janvier 2020 10h08m05s", orders = "dmy HMS", quiet = TRUE)(lubridate::now())
  )
  qs::qsave(x, file = filename, preset = "fast", nthreads = 4)
  invisible(filename)
}

# lit un résultat

load_result <- function(x) {
  filename <- glue::glue("{localdata}/{x}.rda")
  qs::qread(filename, nthreads = 4)
}

# enregistre un objet R dans le répertoire DVF

load_DVF <- function(file, rep = "Rda", local = FALSE) {
  env <- parent.frame()
  str <- glue::glue(file, .envir = env)
  rep <- if (local) localdata else glue::glue("{DVFdata}/{rep}")
  filename <- glue::glue("{rep}/{str}.rda")
  what <- qs::qread(filename, nthreads = 4)
  if ("mods" %in% names(what)) {
    mods <- purrr::map_chr(what$mods, ~ {
      fn <- tempfile("xgb", tmpdir = tempdir())
      xgbmsave(.x, fn)
      fn
    })
    what$bp$xgbmod <- mods
    what$mods <- NULL
  }
  what
}

# lit un objet R depuis le répertoire DVF

save_DVF <- function(x, nom = NULL, rep = "Rda", local = TRUE, preset = "fast") {
  ex <- rlang::enquo(x)
  n_x <- rlang::as_name(ex)
  rep_u <- if (local) {
    localdata
  } else {
    stringr::str_c(DVFdata, "/", rep)
  }
  dir.create(rep_u, showWarnings = FALSE)
  if (is.null(nom)) {
    filename <- glue::glue("{rep_u}/{n_x}.rda")
  }
  else {
    env <- parent.frame()
    nom <- glue::glue(nom, .envir = env)
    filename <- glue::glue("{rep_u}/{nom}.rda")
  }
  if ("bp" %in% names(x)) {
    x$mods <- purrr::map(x$bp$xgbmod, xgbmload)
  }
  if (is.list(x)) {
    suppressWarnings(x <- rapply(x, readAll, classes = c("RasterBrick", "RasterLayer", "RasterStack"), how = "replace"))
  } else if (is.raster(x)) {
    suppressWarnings(readAll(x))
  }
  qs::qsave(x, file = filename, preset = preset, nthreads = 4)
}

xgbmsave <- function(mod, fn) {
  qs::qsave(mod, file = fn, nthreads = 4)
}

xgbmload <- function(fn) {
  if (!"xgb.Booster" %in% class(fn)) {
    rds <- qs::qread(file = fn, nthreads = 4)
    xgboost::xgb.Booster.complete(rds)
  }
  else {
    fn
  }
}

lsave_DVF <- function(xgb, nom = NULL) {
  save_DVF(xgb, nom = nom, local = TRUE)
}

lload_DVF <- function(x) {
  load_DVF(x, local=TRUE)
}
# lorsque le tiblle contient une géometry (sf) select garde la géométrie t_select l'oublie

selectt <- function(.data, ...) {
  dplyr::as_tibble(.data) %>%
    dplyr::select(...)
}

graph2svg <- function(graph, file, height = 16, width = 20, textratio = 2) {
  env <- parent.frame()
  file <- glue::glue(file, .envir = env)
  svglite::svglite(file = stringr::str_c(file, ".svg"), height = height, width = width, pointsize = 9)
  print(graph, vp = grid::viewport(gp = grid::gpar(cex = textratio)))
  invisible(dev.off())
}

graph2jpg <- function(graph, file, height = 2160, width = 3840) {
  env <- parent.frame()
  file <- glue::glue(file, .envir = env)
  grDevices::jpeg(file = str_c(file, ".jpg"), height = height, width = width, res = 300)
  print(graph)
  dev.off()
}

graph2pdf <- function(graph, file, height = 8, width = 10) {
  env <- parent.frame()
  file <- glue::glue(file, .envir = env)
  grDevices::pdf(file = str_c(file, ".pdf"), height = height, width = width)
  print(graph)
  dev.off()
}


basecol <- function(i, max = 3) {
  scales::hue_pal()(max)[[i]]
}

partition_by <- function(tbl, by) {
  tbl_temp <- tbl %>% dplyr::mutate(rows = rowid())
  purrr::map_int(
    unique(tbl %>%
      dplyr::pull(!!by)),
    ~ (tbl_temp %>%
      dplyr::filter((!!by) == .x) %>%
      dplyr::pull(rows))
  )
}

showMemoryUse <- function(sort = "size", decreasing = TRUE, limit = 10, envir = parent.frame()) {
  objectList <- ls(envir = envir)

  oneKB <- 1024
  oneMB <- 1048576
  oneGB <- 1073741824

  memoryUse <- sapply(
    objectList,
    function(x) as.numeric(utils::object.size(eval(parse(text = x), envir = envir)))
  )

  memListing <- sapply(memoryUse, function(size) {
    if (size >= oneGB) {
      return(paste(round(size / oneGB, 2), "GB"))
    } else if (size >= oneMB) {
      return(paste(round(size / oneMB, 2), "MB"))
    } else if (size >= oneKB) {
      return(paste(round(size / oneKB, 2), "kB"))
    } else {
      return(paste(size, "bytes"))
    }
  })

  memListing <- data.frame(objectName = names(memListing), memorySize = memListing, row.names = NULL)

  if (sort == "alphabetical") {
    memListing <- memListing[order(memListing$objectName, decreasing = decreasing), ]
  } else {
    memListing <- memListing[order(memoryUse, decreasing = decreasing), ]
  } # will run if sort not specified or "size"
  print(dim(memListing))
  memListing <- memListing[1:min(nrow(memListing), limit), ]

  print(memListing, row.names = FALSE)
  return(invisible(memListing))
}

is.equal2 <- function(ch) {
  function(x) as.character(ch) == as.character(x)
}

copy_tbl_attr <- function(target, source) {
  attrs <- purrr::map(source, ~ attributes(.x))
  for (n in names(target)) {
    attributes(target[[n]]) <- attrs[[n]]
  }
  target
}

table.percent <- function(...) {
  tab <- table(...)
  round(tab / sum(tab) * 100, 1)
}

duplicate_rows <- function(x) {
  duplicates <- unique(x[duplicated(x)])
  match(x, duplicates) %>%
    is.na() %>%
    not()
}

factor2num <- function(f) {
  if (is.factor(f)) {
    if (!any(is.na(suppressWarnings(as.numeric(levels(f)))))) {
      as.numeric(levels(f))[f]
    } else {
      as.numeric(f)
    }
  }
  else if (is.logical(f)) {
    as.numeric(f)
  } else {
    as.numeric(f)
  }
}

sample_index <- function(.data, n, replace = TRUE, exclude = NULL) {
  index <- seq(1:nrow(.data))
  index <- setdiff(index, exclude)
  base::sample(index, size = n, replace = replace)
}

percent_signif <- function(x, digits = 2) {
  library("stringr", quietly = TRUE)
  stringr::str_c(signif(x, digits) * 100, "%")
}

make_formula <- function(y, x) {
  library("stringr", quietly = TRUE)
  as.formula(str_c(y, "~", str_c(x, collapse = "+")))
}

list.brace <- function(x, brace = "{}", sep = ",") {
  library("stringr", quietly = TRUE)
  if (is.atomic(x) && length(x) == 1) {
    return(str_c(x))
  } else {
    stringr::str_c(
      stringr::str_sub(brace, 1, 1),
      stringr::str_c(purrr::map(x, list.brace), collapse = sep), str_sub(brace, 2, 2)
    )
  }
}

list.brace1 <- function(x, brace = "{}", sep = ",") {
  library("stringr", quietly = TRUE)
  x[is.na(x)] <- "NA"
  stringr::str_c(
    stringr::str_sub(brace, 1, 1),
    stringr::str_c(x, collapse = sep),
    stringr::str_sub(brace, 2, 2)
  )
}

list.unbrace <- function(x) {
  x1 <- stringr::str_replace_all(
    stringr::str_replace_all(x, "\\}", "]"), "\\{", "["
  )
  stringr::str_replace_all(x1, pattern = "([:alnum:])", replacement = '\\"\\1\\"') %>% jsonlite::fromJSON()
}

tabl <- function(data, var) {
  data %>%
    dplyr::as_tibble() %>%
    dplyr::transmute("{{var}}" := {{ var }}) %>%
    table(useNA = "ifany")
}

f2si2 <- function(number, rounding = TRUE, digits = 1, unit = "median") {
  lut <- c(
    1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06,
    0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21,
    1e+24
  )
  pre <- c(
    "y", "z", "a", "f", "p", "n", "u", "m", "", "k",
    "M", "G", "T", "P", "E", "Z", "Y"
  )
  ix <- findInterval(number, lut)
  ix <- switch(unit,
    median = median(ix, na.rm = TRUE),
    max = max(ix, na.rm = TRUE),
    multi = ix
  )
  if (rounding == TRUE) {
    sistring <- paste0(round(number / lut[ix], digits), pre[ix])
  } else {
    sistring <- paste0(number / lut[ix], pre[ix])
  }
  return(sistring)
}

f2si2df <- function(df, string = "", unit = "multi") {
  purrr::map(df, ~ stringr::str_c(f2si2(.x, unit = unit), string, sep = " "))
}

if2si2 <- function(text) {
  library("stringr", quietly = TRUE)
  pre <- c(
    "y", "z", "a", "f", "p", "n", "u", "m", "1", "k",
    "M", "G", "T", "P", "E", "Z", "Y"
  )
  lut <- c(
    1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06,
    0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21,
    1e+24
  )
  names(lut) <- pre
  value <- stringr::str_extract(text, "[:digit:]+\\.?[:digit:]*") %>%
    as.numeric()
  unit <- stringr::str_extract(text, "(?<=[:digit:])[:alpha:]")
  unit[is.na(unit)] <- "1"
  value * lut[unit]
}

uf2si2 <- function(number, rounding = TRUE, unit = "median") {
  n_number <- length(number)
  digits <- 1
  f2 <- f2si2(number, digits = digits, unit = unit)
  while (length(unique(f2)) < n_number) {
    digits <- digits + 1
    f2 <- f2si2(number, digits = digits, unit = unit)
  }
  f2
}

list2strs <- function(list) {
  library("purrr", quietly = TRUE)
  library("stringr", quietly = TRUE)
  if (is.null(names(list))) {
    toString(list)
  } else {
    list <- purrr::imap_chr(list, ~ stringr::str_c(.y, "=", .x))
    stringr::str_c(list, collapse = ", ")
  }
}

second2str <- function(temps) {
  library("stringr", quietly = TRUE)
  temps <- round(temps)
  jours <- temps %/% (3600 * 24)
  temps <- temps %% (3600 * 24)
  heures <- temps %/% 3600
  temps <- temps %% 3600
  minutes <- temps %/% 60
  secondes <- temps %% 60
  s <- character(0)
  if (jours > 0) {
    s <- stringr::str_c(jours, "j ")
  }
  if (heures > 0) {
    s <- stringr::str_c(s, heures, "h ")
  }
  if (minutes > 0) {
    s <- stringr::str_c(s, minutes, "m ")
  }
  if (secondes > 0) {
    s <- stringr::str_c(s, secondes, "s")
  }
  str_replace(s, " $", "")
}

pmemuse <- function(...) {
  current <- memuse::Sys.procmem()[[1]]
  size <- memuse::mu.size(current)
  unit <- memuse::mu.unit(current)
  glue::glue("<{signif(size, 2)}{unit}>")
}

apply_consistent_y_lims <- function(this_plot) {
  num_plots <- length(this_plot$layers)
  if (num_plots > 1) {
    y_lims <- lapply(
      1:num_plots,
      function(x) ggplot2::ggplot_build(this_plot[[x]])$layout$panel_scales_y[[1]]$range$range
    )
    min_y <- min(unlist(y_lims))
    max_y <- max(unlist(y_lims))
    this_plot & ylim(min_y, max_y)
  }
  else {
    this_plot
  }
}
