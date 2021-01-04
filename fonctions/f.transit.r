get_dest <-function(from, data) {
  data %>% filter(stop_id==!!from) %>% pull(to_stop)
}

add_dest <- function(line, data)
  {
  d <- get_dest(tail(line,1), data)
  ok <- is.na(d)
  newlines <- map(d, ~if(!is.na(.x)) c(line, .x) else line)
  list(lines=newlines, ok=ok)
  }

build_line <-  function(start, data)
{
  ok <- FALSE
  lines <- list(start)
  result <- list()
  while(!all(ok))
    {
    ll <- map(lines, ~add_dest(.x, data))
    lll <- map(ll, ~{
      ok <- .x$ok
      result <<- append(result, .x$lines[ok])
      list(l=.x$lines[!ok], ok=ok)
    })
    ok <- map(lll,"ok") %>% flatten_lgl
    lines <- map(lll, "l") %>% flatten
  }
  result
}

get_line <- function(route_id, gtfs)
{
  route <- gtfs$routes %>% 
    filter(route_id==!!route_id) 
  couleur <- route %>% 
    pull(route_color) %>% str_c("#", .)
  trips <- gtfs$trips %>% filter(route_id==!!route_id, direction_id==0)
  stop_times <- left_join(trips, gtfs$stop_times , by="trip_id")
  troncons <- stop_times %>% 
    group_by(trip_id) %>% 
    arrange(stop_sequence) %>% 
    summarize(stop_id = list(stop_id)) %>% 
    distinct(stop_id)
  if(nrow(troncons)==0) return(list(NULL))
  noninclus <- troncons$stop_id
  i <- 1
  for(l in troncons$stop_id)
  {
    sans_l <- noninclus[-i]
    inclu <- map_lgl(sans_l, ~length(setdiff(l,.x))==0)
    if(any(inclu)) noninclus <- sans_l else i <- i+1
  }
  troncons <- troncons %>% 
    filter(stop_id %in% noninclus) %>% 
    mutate(troncon_id=1:n()) %>%
    unnest(stop_id)
  
  troncons <- troncons %>% 
    left_join(gtfs$stops %>%
                select(stop_name, stop_id, lat=stop_lat, lon=stop_lon),
              by="stop_id") %>% 
    st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
    st_transform(3035)
  lines <- troncons %>%
    group_by(troncon_id) %>% 
    summarize(do_union=FALSE) %>% st_cast("LINESTRING") %>% 
    rename(id=troncon_id) %>% 
    mutate(route_id=route$route_id, 
           name=route$route_short_name, 
           route_type=route$route_type, 
           route_color=route$route_color,
           id=as.character(id),
           line=TRUE)
  stops <- troncons %>%
    group_by(stop_id) %>%
    summarize(name=first(stop_name)) %>% 
    rename(id=stop_id) %>% 
    mutate(route_id=route$route_id, route_type=route$route_type, route_color=route$route_color, line=FALSE)
  bind_rows(lines, stops)
  }