source("init.r") 

#fichier_paca

iris15 <- load_DVF("iris15")
paca <- iris15 %>% filter(UU2010=="17601") %>% st_buffer(20000) %>% st_union
uu601 <- iris15 %>% filter(UU2010=="17601") %>% st_union
iris15_paca <- iris15 %>% select(EMP09, P15_POP) %>% filter(st_within(.,paca, sparse=FALSE)) %>% st_centroid()

paca.csv <- iris15_paca %>% 
  st_transform(4326) %>%  
  mutate(lat=st_coordinates(.)[,1],lon=st_coordinates(.)[,2]) %>% st_drop_geometry()
fwrite(paca.csv, "./Arij/paca.csv")

# carreaux sélectionnés pour le calcul de la grille
# On ne calcule pas les isochrones pour des carreaux inhabités
# Pour l'INSEE le nombre de ménages par carreau est supérieur à 10

c200 <- load_DVF("c200") 
c200_601 <- c200 %>% filter(st_within(., uu601, sparse=FALSE))

tr_r5_larochelle <- routing_setup_r5(path="G:/Mon Drive/DVFdata/r5r_data/larochelle/r5" %>% glue, mode=c("WALK", "TRANSIT"), time_window=60,montecarlo = 100,percentiles = 5L,n_threads=4, date="10-01-2020 8:00:00")

iso_transit_50_r5_larochelle <- iso_accessibilite(
  quoi=iris15_paca, 
  ou=c200_601,  
  resolution=50, 
  tmax=120, 
  pdt=1, 
  routing=tr_r5_larochelle) 
install.packages("")
save_DVF(iso_transit_50_r5_larochelle)


#Visualisation du résultat
tm_shape(iso_transit_50_r5_larochelle$EMP09$iso45m)+tm_raster(style="cont", palette=heatrg)
