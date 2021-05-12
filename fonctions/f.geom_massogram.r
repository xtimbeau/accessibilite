library(ggplot2)

geom_massogram <- function(mapping = NULL, data = NULL,
                            stat = StatMasso,
                            position = "stack",
                            ...,
                            na.rm = FALSE,
                            cuts = NULL,
                            lines = FALSE,
                            bins = 0,
                            trans=FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) 
{
  cuts_trans <- NULL
  if (!is.null(mapping$x) & is_function(cuts)) {
    xx <- quo_name(mapping$x)
    fcuts <- cuts(xx)
    if (!all(is.na(fcuts))) {
      cuts <- fcuts
      icuts <-str_extract(names(fcuts), pattern = "[:digit:]*\\.?[:digit:]*") %>% as.numeric()
      cuts_trans <- trans_new(
        "qtrans",
        approxfun(y=icuts, x=fcuts, rule=2),
        approxfun(x=icuts, y=fcuts, rule=2),
        breaks = extended_breaks(),
        minor_breaks = regular_minor_breaks(),
        format = format_format(),
        domain = c(min(fcuts), max(fcuts)))
    }
  }
  params <- list(
    na.rm = na.rm,
    bins = bins,
    cuts = cuts,
    lines = lines,
    trans= trans,
    ...
  )
  
  if(!is.null(cuts)) {
    
  }
  
  list(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomMassogram,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = params
    ),
    if(!is.null(cuts_trans)&trans) coord_trans(x=cuts_trans))
}

GeomMassogram <- ggplot2::ggproto(
  "GeomMassogram",
  GeomArea,
  setup_params = function(data, params) {
    params
  },
  
  extra_params = c("na.rm", "cuts", "lines", "bins", "trans"),
  
  draw_group = function(self, data, panel_params, coord, lines) {
    if (lines)
    {
      line <- data %>% transmute(colour, x, y, ymin=ymin, ymax=y, PANEL, group, fill, size, linetype, alpha)
      ll <- GeomArea$draw_panel(line, panel_params, coord)
    }
    else
    {
      line <- data %>%
        transmute(colour, x=x-dx/2, xend=x+dx, y, yend=y, PANEL, group, fill, size, linetype, alpha)
      line2 <- data %>% 
        arrange(x) %>% 
        transmute(colour, x=x+dx/2, xend=x, y, yend=lead(y), PANEL, group, fill, size, linetype, alpha)
      line <- bind_rows(line, line2)
      
      rects <- data %>% 
        transmute(colour=NA, xmin=x-dx/2, xmax=x+dx/2,
                  ymin = ymin, ymax = y, PANEL, group, fill, size, linetype, alpha)
      ll <- grid::gList(
        ggproto_parent(GeomRect, self)$draw_panel(rects, panel_params, coord),
        ggproto_parent(GeomSegment, self)$draw_panel(line, panel_params, coord))
    }
    ll
  },
  
  draw_key = draw_key_polygon,
  required_aes = c("x", "mass"),
  optional_aes = c("label", "w", "xend", "yend", "xmin", "xmax", "ymin", "ymax"),
  default_aes = plyr::defaults(
    aes( fill = NA, colour = "black", alpha = NA, size = 0.5),
    GeomArea$default_aes)
)

stat_massogram <- function(mapping = NULL, data = NULL,
                            geom = "area",
                            position = "identity",
                            ...,
                            na.rm = FALSE,
                            cuts = NULL,
                            lines = FALSE,
                            trans = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMasso,
    position=position,
    geom = geom,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      bins = bins,
      cuts = cuts,
      lines = lines,
      trans=trans,
      ...
    )
  )
}

# Function pour le cumpute group: bin les x et calcule les quantiles sur le x biné

StatMasso <- ggplot2::ggproto(
  "StatMasso", 
  Stat,
  required_aes = c("x", "mass"),
  default_aes = aes(y = after_stat(sum), fill = NA, w = 1, alpha = NA, colour = NA, size = 0.5),
  optional_aes = c("label", "ymin", "ymax", "w"),
  
  setup_params = function(self, data, params) {
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_mass <- !(is.null(data$mass) && is.null(params$mass))
    
    if (!has_x && !has_mass) {
      abort("stat_massogram() requires an x and mass aesthetic.")
    }
    has_cuts <- !is.null(params$cuts)
    has_weights <- !is.null(data$w)
    has_label <- !is.null(data$label)
    
    if (!has_cuts) {
      if (has_label) {
        xx <- tibble(x = data$x, label = data$label) %>%
          group_by(label) %>%
          summarize(x = median(x))
      }
      else {
        xx <- tibble(x = data$x)
      }
      if (has_weights) {
        w <- data$w
      } else {
        w <- 1
      }
      if (params$bins > 0) {
        cuts <- weighted_quantile_x(xx$x, w = w, prob = 0:params$bins / params$bins)
      } else {
        cuts <- sort(unique(xx$x))
      }
    }
    else {
      cuts <- params$cuts
    }
    delta_x <- (tail(cuts, -1) - head(cuts, -1))
    params$cuts <- cuts
    params$delta_x <- delta_x
    params$labels_x <- (tail(cuts, -1) + head(cuts, -1)) / 2
    params
  },
  extra_params = c("na.rm", "cuts", "lines", "bins", "trans", "labels_x", "delta_x"),
  setup_data = function(data, params) {
    x_cutted <- findInterval(data$x, params$cuts, all.inside = TRUE)
    data <- data %>%
      mutate(
        x_cutted = x_cutted,
        x = params$labels_x[x_cutted],
        dx = params$delta_x[x_cutted],
        gm = sum(mass)) %>%
      group_by(PANEL, group) %>% 
      mutate(ggm=sum(mass, na.rm=TRUE)) %>% 
      ungroup()
    return(data)
  },
  compute_group = function(data, scales, na.rm = FALSE, probs, trans, labels_x, delta_x) {
    compute_masso(data$x, data$mass, data$dx, data$ggm, trans = trans, labels_x=labels_x, delta_x=delta_x)
  }
)

compute_masso <- function(x, y, dx, ggm, trans=FALSE, labels_x, delta_x) {
  data <- data.table(x = x, y = y, dx = dx)
  data <- merge(data, data.table(x=labels_x, dx=delta_x), by=c("x", "dx"), all.x=TRUE, all.y=TRUE, sort=TRUE)
  data[is.na(y), y:=0]
  if(trans) 
    ydx <- mean(data$dx)
  else
    ydx <- data$dx
  data[, ydx := (ydx)]
  
  quansity1 <- data[,.(sum_gross = sum(y),
                       mass = sum(y),
                       dx=first(dx),
                       ydx=first(ydx)), by=x]
  setorder(quansity1, x)
  quansity1[, sum := sum_gross/ydx][, cumsum := cumsum(sum_gross)]
  quansity1[, `:=`(ymax=sum, ymin=0, groupmass = sum(sum_gross), grossgroupmass = ggm[[1]])]
  as_tibble(quansity1)
}