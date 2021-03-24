library(ggplot2)

geom_quantogram <- function(mapping = NULL, data = NULL,
                            stat = StatQuanto, position = "identity",
                            ...,
                            na.rm = FALSE,
                            cuts = NULL,
                            probs = 0.5,
                            lines = TRUE,
                            bars = TRUE,
                            bins = 0,
                            trans=FALSE,
                            se = TRUE,
                            show.legend = NA,
                            inherit.aes = TRUE) 
  {   
  cuts_trans <- NULL
  if (!is.null(mapping$x) & is_function(cuts)) {
    xx <- quo_name(mapping$x)
    fcuts <- cuts(xx)
    if (!all(is.na(fcuts))) {
      cuts <- fcuts
      ncuts <- names(fcuts)
      icuts <-str_extract(ncuts, pattern = "[:digit:]*\\.?[:digit:]*") %>% as.numeric()
      cuts_trans <- trans_new(
        "qtrans",
        approxfun(y=icuts, x=fcuts),
        approxfun(x=icuts, y=fcuts),
        breaks = extended_breaks(),
        minor_breaks = regular_minor_breaks(),
        format = format_format(),
        domain = c(min(fcuts), max(fcuts)))
    } else {
      cuts <- NULL
    }
  }
  params <- list(
    na.rm = na.rm,
    bins = bins,
    cuts = cuts,
    probs = sort(probs),
    lines = lines,
    bars = bars,
    trans= trans,
    se=se,
    ...
  )

  if(!is.null(cuts)) {
    
  }
  
  list(
    ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuantogram,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
    ),
    if(!is.null(cuts_trans)&trans) coord_trans(x=cuts_trans))
}

GeomQuantogram <- ggplot2::ggproto("GeomQuantogram",
  Geom,
  setup_params = function(data, params) {
    params
  },
  required_aes = c("x", "y"),
  optional_aes = c("label",  "w"),
  non_missing_aes = c("ymin", "ymax", "xend", "yend"),
  default_aes = plyr::defaults(
    aes(fill = NA, colour = "black", alpha = NA, size = 0.5),
    GeomArea$default_aes),
  extra_params = c("na.rm", "cuts", "probs", "lines", "bins", "trans",
                   "se", "delta_x", "labels_x"),

  draw_group = function(data, panel_params, coord, probs = 0.5, lines = TRUE, bars = TRUE, se = TRUE, flipped_aes = FALSE) {
    np <- length(probs)
    alphas <- if(np>1) 
      c(rep(0.15/(np-1), np-1), 0.1)
    else
      0.25

    if(se){
      if (lines) {
        ribbons <- map2(probs, alphas, ~ data %>%
                          transmute(x,
                                    PANEL,
                                    group,
                                    y = median,
                                    ymin = .data[[glue("y_{.x}_m")]],
                                    ymax = .data[[glue("y_{.x}_p")]],
                                    fill = colour,
                                    colour = NA,
                                    linetype,
                                    size,
                                    alpha = .y
                          ))
      } else {
        ribbons <- map2(probs, alphas, ~ data %>% 
                          transmute(x,
                                    PANEL,
                                    group,
                                    y = median,
                                    ymin = .data[[glue("y_{.x}_m")]],
                                    ymax = .data[[glue("y_{.x}_p")]],
                                    colour = colour,
                                    linetype,
                                    size,
                                    alpha = .y
                          ))
      }
    }
    else 
      ribbons <- NULL
    
    # line <- data %>% transmute(colour, x, y, ymin = 0, ymax = y, PANEL, group, fill, size, linetype, alpha)
    
    line <- data %>%
      transmute(colour, x=x-dx/2, xend=x+dx, y, yend=y, PANEL, group, fill, size, linetype, alpha)
    line2 <- data %>% 
      arrange(x) %>% 
      transmute(colour, x=x+dx/2, xend=x, y, yend=lead(y), PANEL, group, fill, size, linetype, alpha)
    line <- bind_rows(line, line2) %>% drop_na(x,xend,y,yend)
    
    rects <- data %>% 
      arrange(x) %>% 
      group_by(PANEL, group) %>% 
      transmute(colour=NA, xmin=if_else(is.na(lag(x)), x, x-(x-lag(x))/2), xmax=if_else(is.na(lead(x)), x, x+(lead(x)-x)/2),
                ymin = 0, ymax = y, PANEL, group, fill, size, linetype, alpha) %>% 
      drop_na(xmin, xmax) %>% 
      ungroup()
    
    if (lines) {
      ll <- append(
        if (se) map(ribbons, ~ GeomArea$draw_panel(.x, panel_params, coord)),
        list(GeomArea$draw_panel(line, panel_params, coord))
      )
    } else {
      ll <- append(
        if (se) map(ribbons, ~ GeomLinerange$draw_panel(.x, panel_params, coord)),
        list(GeomSegment$draw_panel(line, panel_params, coord),
             GeomRect$draw_panel(rects, panel_params, coord))
             )
      }
    do.call(grid::gList, ll)
  },

  draw_key = draw_key_polygon
)

# stat_quantogram <- function(mapping = NULL, data = NULL,
#                             geom = "area", position = "identity",
#                             ...,
#                             na.rm = FALSE,
#                             cuts = NULL,
#                             probs = c(0.25, 0.75),
#                             lines = TRUE,
#                             bars = TRUE,
#                             trans = FALSE,
#                             show.legend = NA,
#                             inherit.aes = TRUE) {
#   ggplot2::layer(
#     data = data,
#     mapping = mapping,
#     stat = StatQuanto,
#     position=position,
#     geom = geom,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       na.rm = na.rm,
#       bins = bins,
#       cuts = cuts,
#       probs = sort(probs),
#       lines = lines,
#       bars = bars,
#       trans=trans,
#       ...
#     )
#   )
# }

# Function pour le cumpute group: bin les x et calcule les quantiles sur le x biné

StatQuanto <- ggplot2::ggproto("StatQuanto", Stat,
  required_aes = c("x", "mass"),
  default_aes = aes(y = after_stat(median), 
                    fill = NA, w = 1, alpha = NA, colour = NA, size = 0.5),
  optional_aes = c("label",  "w"),
  non_missing_aes = c("y", "ymin", "ymax"),
  setup_params = function(self, data, params) {
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_measure <- !(is.null(data$mass) && is.null(params$mass))

    if (!has_x && !has_measure) {
      abort("stat_quanto() requires an x and mass aesthetic.")
    }
    if (is.null(params$label)) params$label <- NULL
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
  extra_params = c("na.rm", "cuts", "probs", "lines", "bins", "trans", "labels_x", "delta_x", "se"),
  setup_data = function(data, params) {
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
        delta_x <- (tail(cuts, -1) - head(cuts, -1))
        delta_x <- c(delta_x[[1]], delta_x)
        data <- data %>% 
          mutate( dx = delta_x[which(x==cuts)])
        return(data)
      }
    }
    else {
      cuts <- params$cuts
    }
    labels_x <- (tail(cuts, -1) + head(cuts, -1)) / 2
    delta_x <- tail(cuts, -1) - head(cuts, -1)
    x_cutted <- findInterval(data$x, cuts, all.inside = TRUE)
    data <- data %>%
      mutate(
        x_cutted = x_cutted,
        x = labels_x[x_cutted],
        dx = delta_x[x_cutted])
    
    return(data)
  },
  compute_group = function(data, scales, na.rm = FALSE, bars, probs, trans, labels_x, delta_x) {
    if(!bars) probs <- NULL
    compute_quansity_dt(data$x, data$mass, data$dx, prob = probs,
                        trans = trans, labels_x=labels_x, delta_x=delta_x) 
  }
)

compute_quansity_dt <- function(x, y, dx, prob, trans, labels_x, delta_x) {
  # if(trans) 
  #   dx <- rep(sum(dx)/length(dx), length(dx))
  # data <- data.table(x = x, y = y, dx = dx)
  data <- data.table(x = x, y = y, dx = dx)
  data <- merge(data, data.table(x=labels_x, dx=delta_x), 
                by=c("x", "dx"),
                all.x=TRUE, all.y=TRUE, sort=TRUE)
  data[is.na(y), y:=0]
  if(trans) 
    ydx <- mean(data$dx)
  else
    ydx <- data$dx
  data[, ydx := (ydx)]
  
  quansity1 <- data[,.(median= median(y),
                         mean = mean(y),
                         n = .N,
                         dx=first(dx)), by=x]
  quansity1[, `:=`(mass = mean)]
  quansities <- map(prob, ~{
    qm <- str_c("y_", .x, "_m")
    qp <- str_c("y_", .x, "_p")
    qq <- data[, .(
      q_m = quantile(x = y, probs = c(0.5 - .x / 2)),
      q_p = quantile(x = y, probs = c(0.5 + .x / 2))
    ), by=x]
    setnames(qq, c("q_p", "q_m"), c(qp, qm))
    qq[, x:=NULL]})
  quansities <- do.call(cbind, quansities)
  if(length(prob)>0) quansities[, ':='(ymax=do.call(pmax,.SD), ymin=do.call(pmin,.SD))]
  as_tibble(cbind(quansity1, quansities))
}