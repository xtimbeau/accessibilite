library(ggplot2)

geom_quantagram <- function(mapping = NULL, data = NULL,
                           stat = StatQuanta, 
                           position = "identity",
                           ...,
                           na.rm = FALSE,
                           cuts = NULL,
                           lines = FALSE,
                           bins = 0,
                           trans=FALSE,
                           se = TRUE,
                           probs = c(0.5),
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
    probs= probs,
    se= se,
    ...
  )
  
  if(!is.null(cuts)) {
    
  }
  
  list(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomQuantagram,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = params
    ),
    if(!is.null(cuts_trans)&trans) coord_trans(x=cuts_trans))
}

GeomQuantagram <- ggplot2::ggproto(
  "GeomQuantagram",
  GeomArea,
  required_aes = c("x", "y"),
  optional_aes = c("label",  "w"),
  non_missing_aes = c("ymin", "ymax", "xend", "yend"),
  default_aes = plyr::defaults(
    aes(fill = NA, colour = "black", alpha = NA, size = 0.5),
    GeomArea$default_aes),
  extra_params = c("na.rm", "cuts", "lines", "bins", "trans", "probs", "se", "delta_x", "labels_x"),
  draw_group = function(self, data, panel_params, coord, lines, probs, se) {
    np <- length(probs)
    alphas <- if(np>1) 
      c(rep(0.15/(np-1), np-1), 0.1)
    else
      0.25
    
    if (lines)
    {
      if (se)
      {ribbons <- map2(probs, alphas, ~ data %>% arrange(x) %>% 
                         transmute(x, PANEL, group, y = median, ymin = .data[[glue("y_{.x}_m")]], ymax = .data[[glue("y_{.x}_p")]],
                                   fill=colour, colour = NA, linetype, size, alpha = .y))
      rr <- map(ribbons, ~ GeomArea$draw_panel(.x, panel_params, coord))}
      else
        rr <- NULL
      line <- data %>% transmute(colour, x, y, ymin=ymin, ymax=y, PANEL, group, fill, size, linetype, alpha)
      ll <- GeomArea$draw_panel(line, panel_params, coord)
      ll <- append(ll, rr)
    }
    else
    {
      line <- data %>%
        transmute(colour, x=x-dx/2, xend=x+dx, y, yend=y, PANEL, group, fill, size, linetype, alpha)
      line2 <- data %>% 
        arrange(x) %>% 
        transmute(colour, x=x+dx/2, xend=x, y, yend=lead(y), PANEL, group, fill, size, linetype, alpha)
      line <- bind_rows(line, line2) %>% drop_na(x,xend,y,yend)
      if (se)
      {ribbons <- map2(probs, alphas, ~ data %>% arrange(x) %>% 
                         transmute(x, PANEL, group, y = median, ymin = .data[[glue("y_{.x}_m")]], ymax = .data[[glue("y_{.x}_p")]],
                                   colour = colour, linetype, size, alpha = .y))
      rr <- map(ribbons, ~ GeomLinerange$draw_panel(.x, panel_params, coord))}
      else
        rr <- NULL
      
      rects <- data %>%
        transmute(colour=NA, xmin=x-dx/2, xmax=x+dx/2,
                  ymin = ymin, ymax = y, PANEL, group, fill, size, linetype, alpha)
      
      ll <- list(
        GeomRect$draw_panel(rects, panel_params, coord),
        GeomSegment$draw_panel(line, panel_params, coord))
      ll <- append(ll, rr)
    }
    do.call(grid::gList,ll)
  },
  
  draw_key = draw_key_polygon
)

# Function pour le compute group: bin les x et calcule les quantiles sur le x biné

StatQuanta <- ggplot2::ggproto("StatQuanta", Stat,
  required_aes = c("x", "q"),
  default_aes = aes(y = after_stat(median), fill = NA, w = 1, alpha = NA, colour = NA, size = 0.5),
  optional_aes = c("label",  "w"),
  non_missing_aes = c("y", "ymin", "ymax"),
  
  setup_params = function(self, data, params) {
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_mass <- !(is.null(data$q) && is.null(params$q))
    
    if (!has_x && !has_mass) {
      abort("stat_massogram() requires an x and q aesthetic.")
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
  extra_params = c("na.rm", "cuts", "lines", "bins", "trans", "labels_x", "delta_x", "probs", "se"),
  setup_data = function(data, params) {
    x_cutted <- findInterval(data$x, params$cuts, all.inside = TRUE)
    data <- data %>%
      mutate(
        x_cutted = x_cutted,
        x = params$labels_x[x_cutted],
        dx = params$delta_x[x_cutted])
    return(data)
  },
  compute_group = function(data, scales, na.rm = FALSE, trans, labels_x, delta_x, probs, se) {
    compute_quanta(data$x, data$q, data$dx, trans = trans,
                   labels_x=labels_x, delta_x=delta_x, probs=probs, se=se)
  }
)

compute_quanta <- function(x, y, dx, trans, labels_x, delta_x, probs) {
  data <- data.table(x = x, y = y, dx = dx)
  data <- merge(data, data.table(x=labels_x, dx=delta_x), by=c("x", "dx"), all.x=TRUE, all.y=TRUE, sort=TRUE)
  data[is.na(y), y:=0]
  if(trans) 
    ydx <- mean(data$dx)
  else
    ydx <- data$dx
  data[, ydx := (ydx)]
  
  quansity1 <- data[,.(mean = mean(y),
                       median = median(y),
                       dx=first(dx),
                       ydx=first(ydx)), by=x]
  setorder(quansity1, x)
  quansities <- map(probs, ~{
    qm <- str_c("y_", .x, "_m")
    qp <- str_c("y_", .x, "_p")
    qq <- data[, .(
      q_m = quantile(x = y, probs = c(0.5 - .x / 2)),
      q_p = quantile(x = y, probs = c(0.5 + .x / 2))
    ), by=x]
    setnames(qq, c("q_p", "q_m"), c(qp, qm))
    setorder(qq, x)
    qq[, x:=NULL]})
  quansities <- do.call(cbind, quansities)
  if(length(probs)>0) quansities[, ':='(ymax=do.call(pmax,.SD), ymin=do.call(pmin,.SD))]
  as_tibble(cbind(quansity1, quansities))
}