library(ggplot2)

geom_quantogram <- function(mapping = NULL, data = NULL,
                         stat = "quanto", position = "identity",
                         ...,
                         na.rm = FALSE,
                         bins=100,
                         show.legend = NA,
                         inherit.aes = TRUE) 
  {
  
  params <- list(
    na.rm = na.rm,
    bins=bins,
    ...)
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuantogram,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
      ...
    )
}

GeomQuantogram <- ggplot2::ggproto("GeomQuantogram", 
                          Geom,
                          setup_params = function(data, params) {
                            params
                          },
                          extra_params = c("na.rm", "bins"),
                          setup_data = function(data, params) {
                            data
                          },
                          draw_group = function(data, panel_params, coord, flipped_aes = FALSE) {
                            ribbon_50 <- data %>% mutate(ymin=y_25, ymax=y_75, fill=colour, colour=NA, alpha=0.25)
                            line <- data %>% mutate(y=y, alpha=1)
                            grid::gList(
                              GeomRibbon$draw_group(ribbon_50, panel_params, coord),
                              GeomLine$draw_panel(line, panel_params, coord)
                            )
                          },
                          # draw_key = draw_key_xxx,
                          required_aes = c("x", "y"),
                          optional_aes = c("label", "ymin", "ymax"),
                          default_aes = plyr::defaults(
                            aes(fill = "grey60", colour = "black", alpha = NA),
                            GeomArea$default_aes
                          )
)

stat_quantogram <- function(mapping = NULL, data = NULL,
                         geom = "area", position = "identity",
                         ...,
                         bins = 100,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuanto,
    geom = geom,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      na.rm = na.rm,
      ...
    )
  )
}

# Function pour le cumpute group: bin les x et calcule les quantiles sur le x biné

StatQuanto <- ggplot2::ggproto("StatQuanto", Stat,
                       required_aes = c("x","y"),
                       optional_aes = c("label"),
                       setup_params = function(data, params) {
                         has_x <- !(is.null(data$x) && is.null(params$x))
                         has_y <- !(is.null(data$y) && is.null(params$y))
                         if (!has_x && !has_y) {
                           abort("stat_quanto() requires an x or y aesthetic.")
                         }
                         if(is.null(params$label)) params$label <- NULL
                         params
                       },
                       extra_params = c("na.rm", "bins"),
                       compute_group = function(data, scales, bins = 100, trim=FALSE, na.rm = FALSE) {
                         if (trim) {
                           range <- range(data$x, na.rm = TRUE)
                         } else {
                           range <- scales$x$dimension()
                         }
                         
                         compute_quansity(data$x, data$y, data$label, from = range[1],
                                                    to = range[2], n = bins)
                       }
                       
)

compute_quansity <- function(x, y, label, from, to, n = 100) {
  if(!is.null(label)) data <- tibble(x=x, label=label) %>%
    group_by(label) %>%
    summarize(x=mean(x))
  else 
    data <- tibble(x=x)
  qx <- quantile(data$x, 0:n/n, na.rm=TRUE)
  labels_x <- (tail(qx, -1)+head(qx,-1))/2
  cutx <- findInterval(x, qx, all.inside=TRUE)
  nx <- length(unique(cutx))
  quansity <- tibble(cutx=cutx, x=labels_x[cutx],y=y) %>% 
    group_by(cutx) %>% 
    summarise(
      x = first(x),
      y_50= median(y, na.rm=TRUE),
      y_m = mean(y, na.rm=TRUE), 
      y_25=quantile(y, 0.25, na.rm=TRUE),
      y_75=quantile(y, 0.75, na.rm=TRUE))
  vctrs::new_data_frame(list(
    x = quansity$x,
    y_m = quansity$y_m,
    y = quansity$y_50,
    y_25 = quansity$y_25,
    y_75 = quansity$y_75,
    ymin=quansity$y_25,
    ymax=quansity$y_75 ))
}