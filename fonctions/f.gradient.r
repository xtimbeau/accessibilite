# Calcule et plot des gradients
# en entrée des données, un x, une variable y (ou deux pour faire la différence)
# quelques options (lissage ou binage)

gradientize <- function(data, x, y, nbins=100L, binwidth=NULL, fun="sum", quants = c(0.1, 0.9))
{
  qx <- rlang::enquo(x)
  qy <- rlang::enquo(y)
  grps <- dplyr::group_vars(data)
   
  vecx <- data %>% transmute(!!rlang::quo_name(qx) := !!qx) %>% pull()
  vecy <- data %>% transmute(!!rlang::quo_name(qy) := !!qy) %>% pull()
  
  ldata <- tibble(x=vecx, y=vecy)
  
  if(length(grps!=0))
     ldata <- bind_cols(ldata, data %>%
                          dplyr::select(all_of(grps)))
  
  xmin <-  min(ldata$x, na.rm=TRUE)
  xmax <-  max(ldata$x, na.rm=TRUE)
  if(is.null(binwidth)) 
    binwidth <- (xmax-xmin)/(nbins)
  
  breaks <- seq(round(min(ldata$x, na.rm=TRUE))-binwidth/2, round(max(ldata$x, na.rm=TRUE))+binwidth/2, binwidth)
  labels <- (head(breaks,-1)+tail(breaks,-1))/2
  cutx <- cut(ldata$x,breaks, include.lowest=TRUE,labels=labels)
  ldata <- ldata %>% 
    mutate(cutx=as.numeric(!!cutx)*binwidth)
  ccc <- syms(c("cutx", grps))
  if(fun=="sum")
  {
    bindata <- ldata %>%
      group_by(!!!ccc) %>%
      summarize(y = sum(y, na.rm=TRUE)) %>%
      ungroup() %>% 
      complete(!!!ccc, fill=list(y=0)) %>% 
      group_by_at(grps) %>% 
      mutate(cum_y = cumsum(y))
  }
  else if (fun=="mean")
   {
    bindata <- ldata %>%
      group_by(!!!ccc) %>%
      summarize(se_y=sd(y, na.rm=TRUE),
                ymin=quantile(y, quants[[1]], na.rm=TRUE),
                ymax=quantile(y, quants[[2]], na.rm=TRUE),
                y = mean(y, na.rm=TRUE)) %>%
      ungroup() %>% 
      complete(!!!ccc, fill=list(y=0, se_y=0)) %>% 
      group_by_at(grps)
  }
  bindata
}

plot_gradient <- function(data, x, y, nbins=100L, binwidth=NULL, fun="sum", facet="", quants=c(0.1,0.9), position="stack", cum=FALSE)
{
  data <- data %>% as_tibble()
  grps <- dplyr::group_vars(data)
  
  if(missing(facet))
  {
    fff <- list()
    }
  else 
    {
      qf <- enquo(facet)
      nqf <- quo_name(qf)
      fff <- list(facet_wrap(vars({{facet}})))
      grps <- setdiff(grps, nqf)
      data <- data %>%
        group_by_at(grps) %>%
        group_by(!!qf,.add=TRUE)
    }
  if(length(grps)==1) 
    aes_grp <- aes(col=.data[[grps]], fill=.data[[grps]], group=.data[[grps]])
  else
    aes_grp <- aes()
  
  
  
  bd <- gradientize(data, {{x}}, {{y}}, nbins=nbins, binwidth=binwidth, fun=fun, quants=quants)
  
  if(cum&(fun=="sum"))
    bd <- bd %>% mutate(y=cum_y)
  
  if("se_y"%in%names(bd))
  {
    position <- "identity"
    ribs <- list(geom_pointrange(aes(x=cutx, y=y, ymin=ymin, ymax=ymax), size=0.2, alpha=0.25))
  }
  else 
    ribs <- list()

  ggplot(bd, mapping=aes_grp)+
    # geom_col(aes(x=cutx, y=y), position=position, col=NA, alpha=0.25, width=binwidth, size=0.05)+
    geom_step(aes(x=cutx, y=y), position=position, alpha=1, size=0.25, direction="mid")+
    ribs+
    fff+
    xlab(quo_name(enquo(x)))+
    ylab(quo_name(enquo(y)))
}

plot_diff_gradient <- function(data1, data2, x, y, 
                               binwidth=1, fun="sum", facet_by="", quants=c(0.1,0.9),
                               stack_by="", cum=FALSE)
{
  grp <- dplyr::group_vars(data1)
  if(length(grp)!=0) sgrp <- sym(grp)
  
  if(!missing(stack_by)&fun=="sum")
  {
    stack_by <- enquo(stack_by)
    grpstack <- unique(c(grp, quo_name(stack_by)))
    data1 <- data1 %>% group_by_at(grpstack)
  }
  else 
    grpsstack <- grp
  
  data2 <- data2 %>% group_by_at(grpsstack)
  
  if(missing(facet_by))
  {
    fff <- list()
  }
  else 
  {
    qf <- enquo(facet)
    nqf <- quo_name(qf)
    fff <- list(facet_wrap(vars({{facet}})))
    grp <- setdiff(grp, nqf)
  }
  
  bd1 <- gradientize(data1, {{x}}, {{y}}, binwidth=binwidth, fun=fun, quants=quants)
  bd2 <- gradientize(data2, {{x}}, {{y}}, binwidth=binwidth, fun=fun, quants=quants)
  
  if(cum&(fun=="sum"))
  {
    bd1 <- bd1 %>% mutate(y=cum_y)
    bd2 <- bd2 %>% mutate(y=cum_y)
  }
  
  bd <- merge(bd1, bd2, by=c("cutx", grpsstack), suffix=c(".1", ".2"))
  bd <- bd %>% 
    mutate(dy = y.1-y.2)
  
  if(fun=="sum")
  {
    bd_stacked <- bd %>%
      group_by_at(c("cutx", grp)) %>% 
      summarise(dy=sum(dy))
    steps <- geom_step(data=bd_stacked, aes(x=cutx, y=dy), 
                       position="identity", alpha=0.25, size=0.25, direction="mid", col="red")
    if(length(grp)==1)
      stacks <- map(unique(data1[[quo_name(grps)]]),
                    ~geom_col(data=bd %>% filter(!!grps==.x),aes(x=cutx, y=dy, fill=!!stack_by), 
                              position="stack", col=NA, alpha=0.5, width=binwidth))
    else
      stacks <- geom_col(data=bd,aes(x=cutx, y=dy, fill=!!stack_by), 
                         position="stack", col=NA, alpha=0.5, width=binwidth)
    ribs <- list()
  }
  else if(fun=="mean")
  {
    if(length(grp)==1)
    {
      stacks <- geom_col(data=bd,aes(x=cutx, y=dy, group=!!sgrp, col=!!sgrp, fill=!!sgrp),
                       position="identity", col=NA, alpha=0.25, width=binwidth)
      steps <- geom_step(data=bd, aes(x=cutx, y=dy, group=!!sgrp, col=!!sgrp),
                         position="identity", alpha=0.25, size=0.25, direction="mid")
      ribs <- geom_pointrange(data=bd, aes(x=cutx, y=dy, ymin=ymin.1-y.1+dy, ymax=ymax.1-y.1+dy, group=!!sgrp, col=!!sgrp), lwd=0.5, alpha=0.25)
    }
    else
    {
      stacks <- geom_col(data=bd,aes(x=cutx, y=dy),
                         position="identity", col=NA, alpha=0.25, width=binwidth)
      steps <- geom_step(data=bd, aes(x=cutx, y=dy),
                         position="identity", alpha=0.25, size=0.25, direction="mid")
      ribs <- geom_pointrange(data=bd, aes(x=cutx, y=dy, ymin=ymin.1-y.1+dy, ymax=ymax.1-y.1+dy), lwd=0.5, alpha=0.25)
    }
  }
  
  ggplot()+
    stacks+ribs + steps + fff +
    xlab(quo_name(enquo(x))) +
    ylab(str_c(quo_name(enquo(y))))
}
