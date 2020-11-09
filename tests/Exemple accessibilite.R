source("dvf.r")
# devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

plan("multiprocess", workers=8)

# source des données d'opportunité
iris15 <- load_DVF("iris15")
# sélection géographique des données d'opportunité à l'aire urbaine+20km histoire de ne manquer personne
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union
opp_idf <- uu851 %>% st_buffer(20000)

iris15_idf <- iris15 %>% select(EMP09, P15_POP) %>% filter(st_within(., opp_idf, sparse=FALSE))

# carreaux sélectionnés pour le calcul de la grille
# l'avantage est de ne pas calculer les isochrones pour des carreaux inhabités
# par construction le nombre de ménages par carreau est supérieur à 10

c200_idf <- load_DVF("c200idf") %>% st_transform(3035)  %>% st_as_sf # toute l'IDF
paris <- iris15 %>% filter(DEP=="75") %>% st_union
c200_75 <- c200_idf %>% filter(st_within(., paris, sparse=FALSE)) # Paris
montreuil <- iris15 %>% filter(COM=="93048") %>% st_union
c200_mt <- c200_idf %>% filter(st_within(., montreuil, sparse=FALSE)) # Montreuil

# Moteur r5, en voiture ou en transit
# attention la voiture est lente, surout pour des temps importants

# car_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFM" %>% glue, mode="CAR")
tr_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFM" %>% glue, mode=c("WALK", "TRANSIT"), n_threads = 4)
foot_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFM", mode=c("WALK"), n_threads=4)
car_r5 <- routing_setup_r5(path="{DVFdata}/r5r_data/IDFM", mode=c("CAR"), n_threads=4)

iso_ff_50_r5 <- iso_accessibilite2(
  quoi=iris15_idf,                      
  ou=c200_idf %>% filter(dep=="75"),                           
  resolution=50,                        
  tmax=15,                              
  pdt=1,                                
  routing=foot_r5, ttm_out = TRUE)

#on précalcule les matrices
dt_50_r5_mt <- iso_accessibilite2(
  quoi=iris15_idf,                      
  ou=c200_mt,                           
  resolution=200,                        
  tmax=120,                              
  pdt=5,                                
  routing=car_osrm)     

# on peut comparer
iso_tr_50_r5_pc <- iso_accessibilite2(
  quoi=iris15_idf,                      
  ou=c200_mt,                           
  resolution=50,                        
  tmax=15,                              
  pdt=5,                                
  routing=dt_50_r5_mt)                      

tm_shape(iso_transit_50_r5$EMP09$iso20m)+tm_raster(style="cont", palette=heatrg)

# moteur OSRM
# pas de transports en commun
# rapide pour la voiture, y compris pour des temps longs

car_osrm <- routing_setup_osrm(server="5003", profile="driving")
foot_osrm <- routing_setup_osrm(server="5002", profile="walk")
plan("multiprocess", workers=8)
iso_car_50_osrm <- iso_accessibilite2(quoi=iris15_idf, # les variables d'opportunité
                                       ou=c200_mt, # la grille cible
                                       resolution=50, # la résolution finale (le carreau initial est de 200m, il est coupé en 16 pour des carreaux de 50m)
                                       tmax=15, # le temps max des isochrones en minutes
                                       pdt=1, # le pas de temps pour retourner le résultat en minute
                                       routing=foot_osrm, future=TRUE) # moteur de routing

tm_shape(iso_car_50_osrm$EMP09)+tm_raster(style="cont", palette=heatrg)

iso_foot_50_osrm <- iso_accessibilite(quoi=iris15_idf, # les variables d'opportunité
                                     ou=c200_75, # la grille cible
                                     resolution=50, # la résolution finale (le carreau initial est de 200m, il est coupé en 16 pour des carreaux de 50m)
                                     tmax=15, # le temps max des isochrones en minutes
                                     pdt=1, # le pas de temps pour retourner le résultat en minute
                                     routing=foot_osrm) # moteur de routing

tm_shape(iso_foot_50_osrm$EMP09)+tm_raster(style="cont", palette=heatrg)

# Exemple pour Marseille --------------------
source("dvf.r")
iris15 <- load_DVF("iris15")
paca <- iris15 %>% filter(UU2010=="00759") %>% st_buffer(10000) %>% st_union
uu759 <- iris15 %>% filter(UU2010=="00759") %>% st_union
iris15_paca <- iris15 %>% select(EMP09, P15_POP) %>% filter(st_within(.,paca, sparse=FALSE))
c200_paca <- load_DVF("c200Paca") %>% st_transform(3035)
c200_paca <- c200_paca %>% filter(st_within(., uu759, sparse=FALSE)) 

# Moteur r5, en transit
tr_r5_Marseille <- routing_setup_r5(path="{DVFdata}/r5r_data/Marseille/r5" %>% glue, mode=c("WALK", "TRANSIT"))

iso_transit_50_r5_Marseille <- iso_accessibilite2(quoi=iris15_paca, 
                                                  ou=c200_paca, 
                                                  resolution=50, 
                                                  tmax=60, 
                                                  pdt=5, 
                                                  routing=tr_r5_Marseille)

save_DVF(iso_transit_50_r5_Marseille)

tm_shape(iso_transit_50_r5_Marseille$EMP09)+tm_raster(style="cont", palette=heatrg)
# moteur OSRM
car_osrm_Marseille <- routing_setup_osrm(server="5013", profile="driving")

iso_car_50_osrm_Marseille <- iso_accessibilite2(quoi=iris15_paca, 
                                                ou=c200_paca, 
                                                resolution=50,
                                                tmax=60, 
                                                pdt=5,
                                                routing=car_osrm_Marseille)



# Pour mettre en place r5 ---------------

# il faut installer le sdk java v11 (https://www.oracle.com/java/technologies/javase-jdk11-downloads.html) ,
# puis le package R rJava
# la version de développement de r5

# devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

# il faut ensuite créer un dossier où se trouvent le pbf (OSM découpé) et le ou les GTFS pour le transport
# c'est ce dossier que lon passe ensuite à routing_setup_r5



# Pour mettre en place OSRM --------------

# il faut installer Docker et WSL2
# il faut ensuite créer un dossier avec le pdf d'OSM découpé
# Puis il faut en se plaçant dans le dossier et en ouvrant powershell lancer les commandes suivantes:
# 
# Normalement charge de docker hub les fichiers nécessaires
# on défini ici le profil de déplacement (/opt/car.lua pour la voiture, /opt/foot.lua pour la marche, etc...)
# si on modifie le profile car.lua, il faut le placer dans le dossier et changer /opt/car.lua en /data/car.lua
# c'est ce qui est fait ici le car.lua a été modifé pour augmenter la pénalité de feu rouge de 2s à 20s
# dans cet exemple le pdf s'appelle Idf-latest.osm.pdf

# docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-extract -p /data/car.lua /data/Idf-latest.osm.pbf
# docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-partition /data/Idf-latest.osrm
# docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-customize /data/Idf-latest.osrm

# fabrique le coneneur, que lon' peut relancer à chaque fois (pas besoin de le reconstruire)
# on lui donne un nom, pour le reconnaitre et un numéro de port (qui transfère 5002 vers 5000 qui est utilisé dans le conteneur) pour avoir plusieurs
# conteneurs qui correspondent à plusieurs options (voiture, voiture avec pénalité feu rouge, à pied, en vélo)
# il faut vérifier que le conteneur est lancé dans docker et le relancer sinon

# docker run -t -i --name=osrm_idf_car_t -p 5002:5000 -v "${PWD}:/data" osrm/osrm-backend osrm-routed --algorithm mld --max-table-size 1000000 --threads 8 /data/Idf-latest.osrm


aa <- merge(g1600_4326[, .(g1600, idg=id)], gq_4326[, .(g1600, idq=id)], by="g1600")
map2_dfr(aa$idg,aa$idq, ~gg_ttm[fromId==(.x)&toId==(.y),])
