source("DVF.r")

# equipements <- read.dbf(file="{DVFdata}/Equipements/bpe_ensemble_xy.dbf" %>% glue)
# equipements <- filter(equipements, dep%in%depIdf)
# save_DVF(equipements)

load_DVF("equipements")

codes_equipements <- read_excel("{DVFdata}/equipements/codes equiments.xlsx" %>% glue)
equipements <- left_join(equipements, codes_equipements, by=c("typequ"="code_equipement"))

eq.sf <- st_as_sf(equipements %>% filter(qualite_xy%in%c("Bonne", "Acceptable", "Mauvaise")), coords=c("lambert_x", "lambert_y"), crs=2154)
eq.sf <- st_transform(eq.sf, 3035)
eq.sf <- eq.sf %>% mutate(x=st_coordinates(eq.sf)[,"X"], y=st_coordinates(eq.sf)[, "Y"])
XYeq <- eq.sf %>% as_tibble %>% select(x,y, code_equcat)
XYeqs <- XYeq %>% filter(!is.na(code_equcat)) %>% group_by(code_equcat) %>% group_split()
XYeqs_groups <- map(XYeqs, ~(.$code_equcat[[1]])) %>% unlist
XYeqs_m <- map(XYeqs, function(d) (d %>% select(x,y) %>% as.matrix))
names(XYeqs_m) <- XYeqs_groups
ecoles <- eq.sf %>% filter(typequ%in%c("C101","C102", "C104", "C105", "C201", "C301", "C302", "C303", "C304", "C305"))
colleges <- eq.sf %>% filter(typequ%in%c("C201"))
rayons_eq <- list( ALIM=300, AUTO=1000, BANQ=800, 
                   CD=300, CQ=300, CR=400, CUL=2000,
                   EDU=400, LOI=400, PA=800, PARAMED=300, MED=300, PHAR=300,
                   POSTE=800, REST=300, SAN=500, 
                   SECU=2000, SOC=600, SP=2000, TOUR=800, TRA=300)
load_DVF("c200idf")
c200idf <- st_transform(c200idf, 3035)
grid.r <- raster(xt_as_extent(c200idf), crs=st_crs(c200idf)$proj4string,  resolution=33)
plan("multiprocess", workers=8)
 
r_foc <- function (eq, n_eq, r, grid) 
{
  local_ras <-
    raster::rasterize(
      x = eq,
      y = grid,
      field = 1,
      fun = "count",
      background=0
    )
  raster::focal(local_ras,
                focalWeight(local_ras, r[[n_eq]], "Gauss"),
                na.rm = TRUE,
                padValue = 0,
                pad=TRUE)
}

safe_r_foc <- safely(r_foc)

load_DVF("t_idf")
load_DVF("dvfplus")

t_idf.co <- t_idf %>% st_as_sf %>% st_transform(3035) %>% st_coordinates
dvfplus.co <- dvfplus %>% st_as_sf %>% st_transform(3035) %>% st_coordinates

tic();cat_equ.kgs <- future_imap(XYeqs_m, ~ safe_r_foc(eq=.x, n_eq=.y, r=rayons_eq, grid=grid.r), .progress= TRUE); toc()
kgs <- map(cat_equ.kgs, ~.$result)
names(kgs) <- str_c("eq_", XYeqs_groups)

imap_dfr(XYeqs_m, ~ list(n=.y,l=.x %>% length, r=rayons_eq[[.y]]))

iwalk(kgs, function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co) ])
iwalk(kgs, function(x,y) dvfplus <<- mutate(dvfplus, !!y:= raster::extract(!!(x), dvfplus.co)))

save_DVF(dvfplus)
save_DVF(t_idf)
# 
# tmap_options(max.raster = c(plot = 1e+8, view = 1e+7))
# 
# g2 <- 
#   tm_shape(uu851.sf, bbox=bb851)+tm_borders(lwd=0.25)+tm_fill(col = "gray80", alpha=0.5)+
#   tm_shape(didf.sf, bbox=bb851)+tm_borders(lwd=0.25, col="gray50")+
#   tm_shape(raster::mask(cat_equ.kgs$eq_REST, st_sf(uu851.sf) %>% st_transform(3035)))+
#   tm_raster(col="layer",title="Restaurants", palette= "-Spectral", n=10, style="cont")
# graph2vector(x=g2, file=str_c(prepath, "eq_REST"), type="svg")
# tmap_mode("view")
# g2

# gares pondérées par Maxime

load_DVF("centralite")

garespond <- st_as_sf(centralite, crs=4326, coords=c("lon", "lat")) %>% st_transform(3035) %>% mutate(lclose=log(clos_index))

tm_shape(garespond)+tm_bubbles(col="lclose", size="lclose", style="cont", n=10)

grid.r <- raster(xt_as_extent(c200idf), crs=st_crs(c200idf)$proj4string,  resolution=50)
m_logit <- focal_w_logit(grid.r, 600, 1.25)
rwgares <- raster::rasterize(x=garespond,y=grid.r,field = "lclose", fun= function(x,...) sum(x), background=0)
tic();rwgares.kgs <- raster::focal(rwgares,m_logit, na.rm = TRUE, padValue = 0, pad=TRUE); toc()

tmap_options(max.raster = c(plot = 10e+6, view = 10e+6)) 

load_DVF("iidf.sf")
iidf.sf <- st_transform(iidf.sf,3035)
uu851.sf <- iidf.sf %>% filter(UU2010=="00851") %>% st_union

tmap_mode("view")
tm_shape(raster::mask(rwgares.kgs, st_sf(uu851.sf) %>% st_transform(3035)))+tm_raster(n=20, palette=terrain.colors(20, rev=TRUE))+
   tm_shape(garespond)+tm_dots(col="lclose", size=0.01)
t_idf[, gareswk:= raster::extract(rwgares.kgs, t_idf.co) ]
dvfplus <- dvfplus %>% mutate(gareswk= raster::extract(rwgares.kgs, dvfplus.co))

ggplot(dvfplus %>% filter(UU2010==851))+geom_histogram(aes(x=gareswk), bins=1000)
ggplot(garespond)+geom_histogram(aes(x=log(clos_index)), bins=50)


save_DVF(dvfplus)

gares <-st_read("{DVFdata}/transports/STATION_TRANSPORT.shp" %>% glue, stringsAsFactors=FALSE)
gares <- st_transform(gares, 3035)

colleges.sf <- st_as_sf(colleges)
# calcul des distances entre les transactions et les écoles et les transports

t_idf.inseen <- st_as_sf(t_idf[(i_inseen)]) %>% st_transform(3035)

nncollege <- st_nearest_feature(t_idf.inseen, colleges.sf)
l_ncollege <-st_coordinates(colleges %>% slice(nncollege))-st_coordinates(t_idf.inseen)
d_ncollege <- apply(l_ncollege, 1, function(x) sqrt(x[1]^2+x[2]^2))
t_idf[(i_inseen), plus_proche_college := d_ncollege ]

nngare <- st_nearest_feature(t_idf.inseen, gares)
l_ngare <-st_coordinates(gares %>% slice(nngare))-st_coordinates(t_idf.inseen)
d_ngare <- apply(l_ngare, 1, function(x) sqrt(x[1]^2+x[2]^2))
t_idf[(i_inseen), plus_proche_gare := d_ngare ]

dvfplus <- st_as_sf(dvfplus)
nncollege <- st_nearest_feature(dvfplus, colleges.sf)
l_ncollege <-st_coordinates(colleges.sf %>% slice(nncollege))-st_coordinates(dvfplus)
dvfplus <- dvfplus %>% mutate(plus_proche_college = sqrt((l_ncollege[,"X"])^2+(l_ncollege[,"Y"])^2))

nngare <- st_nearest_feature(dvfplus, gares)
l_ngare <-st_coordinates(gares %>% slice(nngare))-st_coordinates(dvfplus)
dvfplus <- dvfplus %>% mutate(plus_proche_gare = sqrt((l_ngare[,"X"])^2+(l_ngare[,"Y"])^2))

# on ajoute la distance au centre

centre_de_paris <- st_centroid(st_union(iris %>% 
                                          st_transform(3035) %>%
                                          filter(INSEE_COM%in%c("75101","75102","75104","75105","75106", "75107", "75108"))))
d_c <- st_distance(st_as_sf(t_idf) %>% st_transform(3035), centre_de_paris)
t_idf[ , distance_centre := d_c]
d_c <- st_distance(dvfplus, centre_de_paris)
dvfplus <- dvfplus %>% mutate(distance_centre = d_c)

save_DVF(t_idf)
save_DVF(dvfplus)