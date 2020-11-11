source("access.r")

# utile pour OSRM
plan("multiprocess", workers=8)

# source des données d'opportunité
iris15 <- load_DVF("iris15")

# sélection géographique des données d'opportunité à l'aire urbaine+20km histoire de ne manquer personne
bre <- iris15 %>% filter(UU2010=="35701") %>% st_buffer(5000) %>% st_union
uu35701 <- iris15 %>% filter(UU2010=="35701") %>% st_union
iris15_bre <- iris15 %>% select(EMP09, P15_POP) %>% filter(st_within(.,bre, sparse=FALSE)) %>% st_centroid()

# carreaux sélectionnés pour le calcul de la grille
# l'avantage est de ne pas calculer les isochrones pour des carreaux inhabités
# par construction le nombre de ménages par carreau est supérieur à 10

c200 <- load_DVF("c200") 
c200_35701 <- c200 %>% filter(st_within(., uu35701, sparse=FALSE))

rm(c200, iris15)

# Moteur r5, en voiture ou en transit
# attention la voiture est lente, surout pour des temps importants

car_r5_Rennes <- routing_setup_r5(path="{DVFdata}/r5r_data/Rennes/r5" %>% glue, mode="CAR")
tr_r5_Rennes <- routing_setup_r5(path="{DVFdata}/r5r_data/Rennes/r5" %>% glue, mode=c("WALK", "TRANSIT"),
                                time_window=60,montecarlo = 100,percentiles = 5L,n_threads=4)

iso_transit_50_r5_Rennes <- iso_accessibilite(quoi=iris15_bre, # les variables d'opportunité
                                       ou=c200_35701, # la grille cible (plus long sur c200 que sur c200_mt)
                                       resolution=50, # la résolution finale (le carreau initial est de 200m, il est coupé en 16 pour des carreaux de 50m)
                                       tmax=60, # le temps max des isochrones en minutes
                                       pdt=5, # le pas de temps pour retourner le résultat en minute
                                       routing=tr_r5_Rennes) # moteur de routing

save_DVF(iso_transit_50_r5_Rennes)

tm_shape(iso_transit_50_r5_Rennes$EMP09)+tm_raster(style="cont", palette=heatrg)




# moteur OSRM
# pas de transports en commun
# rapide pour la voiture, y compris pour des temps longs

car_osrm_Rennes <- routing_setup_osrm(server="5002", profile="driving")
foot_osrm_Rennes <- routing_setup_osrm(server="5001", profile="walk")

iso_car_50_osrm_Rennes <- iso_accessibilite2(quoi=iris15_bre, # les variables d'opportunité
                                       ou=c200_bre, # la grille cible
                                       resolution=50, # la résolution finale (le carreau initial est de 200m, il est coupé en 16 pour des carreaux de 50m)
                                       tmax=90, # le temps max des isochrones en minutes
                                       pdt=5, # le pas de temps pour retourner le résultat en minute
                                       routing=car_osrm_Rennes) # moteur de routing

save_DVF(iso_car_50_osrm_Rennes)

tm_shape(iso_car_50_osrm_Rennes$EMP09)+tm_raster(style="cont", palette=heatrg)

iso_foot_50_osrm_Rennes <- iso_accessibilite2(quoi=iris15_bre, # les variables d'opportunité
                                     ou=c200_bre, # la grille cible
                                     resolution=50, # la résolution finale (le carreau initial est de 200m, il est coupé en 16 pour des carreaux de 50m)
                                     tmax=15, # le temps max des isochrones en minutes
                                     pdt=1, # le pas de temps pour retourner le résultat en minute
                                     routing=foot_osrm_Rennes) # moteur de routing

save_DVF(iso_foot_50_osrm_Rennes)

tm_shape(iso_foot_50_osrm_Rennes$EMP09)+tm_raster(style="cont", palette=heatrg)

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

