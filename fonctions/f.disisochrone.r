# Calcule et graphe les distributions isochroniques

disichrone <- function(data, dist, mass=Ind, by="", n=512, bw="nrd0", adjust=1)
  {
  library("rlang", quietly=TRUE)
  library("dplyr", quietly=TRUE)
  library("purrr", quietly=TRUE)

  qdist <- rlang::enquo(dist)
  ndist <- rlang::quo_name(qdist)
  qmass <- rlang::enquo(mass)
  nmass <- rlang::quo_name(qmass)
  
  data <- as_tibble(data)
  data <- data %>% drop_na({{dist}}, {{mass}})
  max_dist <- max(data[[ndist]])
  min_dist <- min(data[[ndist]])
  range <- max_dist-min_dist
  total_mass <- sum(data[[nmass]])
  
  data_density <- density(data[[ndist]],
                          weight=data[[nmass]]/total_mass, 
                          from=min_dist, 
                          to=max_dist,
                          n=n,
                          bw=bw, 
                          adjust=adjust)

    
  if(!missing(by))
  {
    qby <- rlang::enquo(by)
    nby <- rlang::quo_name(qby)
    groupes <- unique(data[[nby]])
    
    grp_res <- map_dfr(groupes, ~{
      gdata <- data %>% filter({{by}}==.x)
      grp_mass <- sum(gdata[[nmass]])
      grp_density <- density(
        gdata[[ndist]],
        weight=gdata[[nmass]]/grp_mass,
        from=min_dist, 
        to=max_dist,
        bw=data_density$bw,
        n=n, 
        adjust=1)
      tibble(dist=grp_density$x, 
             density=grp_density$y*grp_mass/total_mass,
             mass=grp_density$y*grp_mass, 
             relative2=.x, 
             grp_mass=grp_mass)
      }
      )
    res <- grp_res
    res <- res %>% 
      arrange(desc(grp_mass), dist) %>% 
      mutate(relative2=factor(relative2,levels=unique(relative2))) %>% 
      select(-grp_mass)
    res <- res %>% 
      group_by(relative2) %>% 
      mutate(cum_mass=cumsum(mass)*range/n) %>%
      ungroup()
    names(res) <- c(ndist, "density", nmass, nby, str_c("cum_", nmass))
    } # is.null(by)
  else 
    {
      res <- tibble(dist=data_density$x,
                    density=data_density$y, 
                    mass=data_density$y*total_mass)
      res <- res %>% 
        arrange(dist) %>% 
        mutate(cum_mass=cumsum(mass)*range/n)
      names(res) <- c(ndist, "density", nmass, str_c("cum_", nmass))
    }
  res
  } 
