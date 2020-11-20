geom_massity <- function(mapping = NULL, data = NULL,
                         stat = "massity", position = "identity",
                         ...,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMassity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
}

GeomMassity <- ggproto("GeomMassity", GeomArea,
                       default_aes = plyr::defaults(
                         aes(fill = NA, mass = 1, colour = "black", alpha = NA),
                         GeomArea$default_aes
                       )
)

stat_massity <- function(mapping = NULL, data = NULL,
                         geom = "area", position = "stack",
                         ...,
                         bw = "nrd0",
                         adjust = 1,
                         kernel = "gaussian",
                         n = 512,
                         trim = FALSE,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatDensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

StatMassity <- ggproto("StatMassity", Stat,
                       required_aes = "x|y",
                       
                       default_aes = aes(x = after_stat(mass), y = after_stat(mass), fill = NA, mass = NULL),
                       
                       setup_params = function(data, params) {
                         params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)
                         
                         has_x <- !(is.null(data$x) && is.null(params$x))
                         has_y <- !(is.null(data$y) && is.null(params$y))
                         if (!has_x && !has_y) {
                           abort("stat_massity() requires an x or y aesthetic.")
                         }
                         
                         params
                       },
                       
                       extra_params = c("na.rm", "orientation"),
                       
                       compute_group = function(data, scales, bw = "nrd0", adjust = 1, kernel = "gaussian",
                                                n = 512, trim = FALSE, na.rm = FALSE, flipped_aes = FALSE) {
                         data <- flip_data(data, flipped_aes)
                         if (trim) {
                           range <- range(data$x, na.rm = TRUE)
                         } else {
                           range <- scales[[flipped_names(flipped_aes)$x]]$dimension()
                         }
                         
                         density <- compute_massity(data$x, data$mass, from = range[1],
                                                    to = range[2], bw = bw, adjust = adjust, kernel = kernel, n = n)
                         density$flipped_aes <- flipped_aes
                         flip_data(density, flipped_aes)
                       }
                       
)

compute_massity <- function(x, m, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian", n = 512) {
  nx <- length(x)
  if (is.null(m)) {
    w <- rep(1 / nx, nx)
    tm <- 1
  } else {
    w <- m / sum(m)
    tm <- sum(m)
  }
  
  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    warn("Groups with fewer than two data points have been dropped.")
    return(vctrs::new_data_frame(list(
      x = NA_real_,
      density = NA_real_,
      scaled = NA_real_,
      ndensity = NA_real_,
      count = NA_real_,
      n = NA_integer_
    ), n = 1))
  }
  
  dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
                         kernel = kernel, n = n, from = from, to = to)
  
  vctrs::new_data_frame(list(
    x = dens$x,
    density = dens$y,
    mass =  dens$y * tm,
    cummass = cumsum(dens$y*tm)*(max(dens$x)-min(dens$x))/n,
    total_mass = tm,
    n = nx
  ), n = length(dens$x))
}
