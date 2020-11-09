
# il faut lancer R et ouvrir le projet RDVF (File/Open Project)
# le répertoire de travail sera mis en conséquence

source("dvf.r") # lance les programmes et charge les variables globales du projet
# attention il faut ajout configuré les variables d'environnement
# (Paramètres de Windows, chercher "environnement, puis trouver les variables d'environnement, en créer 2 GOOGLE_DRIVE et LOCAL_DATA)

iso <- load_DVF("iso_car_50") # charge un indice d'accessibilité à l'emploi déjà calculé, en voiture
# iso_transit_50 contient la même chose mais je suis en train de le refaire 

str(iso) # permet de voir de quoi il est fait
# bricks contient les rasters (un raster est une image, ou encore un tableau de pixel, associé à des coordonnées, c'est donc une image de carte)
# regarder https://rspatial.org/raster/index.html
# ces rasters ont été calculés pour différentes variables (emploi, EMP09), (population, P15_POP), ou (surface, cste)
# le raster associe à chaque poiunt de la grille (un pixel donc), le nombre d'emploi (ou de pop, ou de surface) que l'on peut atteindre en un temps donné
# chaque raster contient des couches (layers) pour chaque temps (10, 20, 30, 40, 50 minutes)

# vars contient la somme totale (sur l'ensemble de la zone, ie l'IDF) de chaque variable (pour EMP09 c'est donc l'emploi de l'IDF)
# c'est le maximum que l'on puisse atteindre


isonorm <- iso$bricks$EMP09/iso$vars$EMP09
# cette instruction fait une opération sur le raster 
# chaque point de chaque couche est divisé par le nombre iso$vars$EMP09, et cela donne pour chaque point un pourcentage des emplois accessible en un temps donné

# on peut regarderce qu'il contient:
plot(isonorm)

# ou mieux essayer:
tm_shape(isonorm[["iso30mn"]])+tm_raster(style="kmeans")
tm_shape(isonorm[["iso30mn"]])+tm_raster(style="cont")
tm_shape(isonorm[["iso50mn"]])+tm_raster(style="cont")

# raster_parcsetjardins_50 est un raster sur l'acces aux parcs et jardins (à pied)
# regarder et essayer de comprendre ce que cela raconte

# creches_isoac_10 et rest_isoac_10 pour les creches et restaurants
# hauteur un autre
# dans makeDVF il ya les programmes qui les contruisent, en les regardant, vous pouvez essayer de comprendre les hypothèses pour leur construction

# Dans makeDVF/1.3 isocar&isotransit se trouve le lancement du calcul de l'iso transit pour l'IDF
# c'est le point de départ pour faire la même chose sur une autre ville
# attention, il faut installer java sur l'ordinateur

# les étapes seront 
# 1. créer un répertoire DVF_lyon
# 2. modifier quelques variables spécifiques à l'IDF (uu851 par exemple dans le fichier DVF, mais aussi c200) pour les changer vers lyon
# 3. fabriquer ce qu'il faut pour qu'OTP fonctionne sur lyon
# 3.1 faire un découpage OSM pour lyon et ses environs
# 3.2 trouvers les GTFS de lyon (par exmeple opendata.gouv.fr)
# 3.3 faire un répertoire lyon équivalent à celui d'IDF1 dans le répertoire otp_idf/graph et le remplir avec les bonnes infos (aps la peine de créer un répertoire otp_lyon)
# un tuto : https://github.com/marcusyoung/otp-tutorial
# 3.4 construire le graph (les commandes java sont rappelées dans Max_otp_server.cmd)
# quand le graphe se construit 90% du boulot est fait
# vérifier que les programmes R ne garde pas de références à l'IDF 
# 3.5 lancer l'équivalement de makeDVF/1.3 isocar&isotransit
# pour commencer augementer la résolution (200 au lieu de 50) et surtout réduire le temps maximal (pour les voitures au lieu de 3000s mettre 600s)
# le temps de calcul est très élevé quand les temps d'isochrone s'accroissent 
