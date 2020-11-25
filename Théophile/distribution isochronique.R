source("access.r")

c200 <- load_DVF("c200") %>% st_drop_geometry() %>% as.data.table()

# méthode 2, en utilisant geom_massity comme geom_density, mais avec un calcul en plus: mass et cummass
# on définit les données et les distances à grapher (dans l'ordre que l'on souhaite)

idf_dt <- r2dt(load_DVF("iso200/ttrr5_emp09_200"), 200)
idf_dt <- merge(idf_dt, c200[, .(idINS200, Ind)], by="idINS200")
lyon_dt <- r2dt(load_DVF("ttr_r5_emp09_Lyon"), 200) # le 200 indique qu'on recalcule le raster en résolution 200
lyon_dt <- merge(lyon_dt, c200[, .(idINS200, Ind)], by="idINS200")
marseille_dt <- r2dt(load_DVF("ttr_r5_emp09_Marseille"), 200)
marseille_dt <- merge(marseille_dt, c200[, .(idINS200, Ind)], by="idINS200")

bordeaux_dt <- r2dt(load_DVF("ttr_r5_emp09_Bordeaux"), 200)
bordeaux_dt <- merge(bordeaux_dt, c200[, .(idINS200, Ind)], by="idINS200")
toulouse_dt <- r2dt(load_DVF("ttr_r5_emp09_Toulouse"), 200)
toulouse_dt <- merge(toulouse_dt, c200[, .(idINS200, Ind)], by="idINS200")
lille_dt <- r2dt(load_DVF("ttr_r5_emp09_Lille"), 200)
lille_dt <- merge(lille_dt, c200[, .(idINS200, Ind)], by="idINS200")
nantes_dt <- r2dt(load_DVF("ttr_r5_emp09_Nantes"), 200)
nantes_dt <- merge(nantes_dt, c200[, .(idINS200, Ind)], by="idINS200")

distances <-c("to25k", "to50k","to100k","to150k","to200k","to250k")


# melt transforme idf_dt en format long
idf_dtm <- idf_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
lyon_dtm <- lyon_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
bordeaux_dtm <- bordeaux_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
toulouse_dtm <- toulouse_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
marseille_dtm <- marseille_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
lille_dtm <- lille_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
nantes_dtm <- nantes_dt[, .SD, .SDcols=c(distances, "Ind")] %>% melt(measure.vars=distances, variable.name="seuil", value.name="dist")
# on peut utiliser seuil comme couleur
isotime_seuil_idf <- ggplot(idf_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("idf")
graph2svg(isotime_seuil_idf, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/idf" %>% glue)

isotime_seuil_lyon <- ggplot(lyon_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Lyon")
graph2svg(isotime_seuil_lyon, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/lyon" %>% glue)

isotime_seuil_marseille <- ggplot(marseille_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Marseille")
graph2svg(isotime_seuil_marseille, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/marseille" %>% glue)

isotime_seuil_bordeaux <- ggplot(bordeaux_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Bordeaux")
graph2svg(isotime_seuil_bordeaux, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/bordeaux" %>% glue)

isotime_seuil_toulouse <- ggplot(toulouse_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Toulouse")
graph2svg(isotime_seuil_toulouse, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/toulouse" %>% glue)

isotime_seuil_lille <- ggplot(lille_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Lille")
graph2svg(isotime_seuil_lille, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/lille" %>% glue)

isotime_seuil_nantes <- ggplot(nantes_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil), alpha=0.5)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Nantes")
graph2svg(isotime_seuil_nantes, file="{DVFdata}/presentation/theophile/transit_emp09/seuils/nantes" %>% glue)

# mais aussi comme facette
isotime_facette_idf <- ggplot(idf_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("idf")
graph2svg(isotime_facette_idf, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/idf" %>% glue)

isotime_facette_lyon <- ggplot(lyon_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Lyon")
graph2svg(isotime_facette_lyon, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/lyon" %>% glue)

isotime_facette_marseille <- ggplot(marseille_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Marseille")
graph2svg(isotime_facette_marseille, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/marseille" %>% glue)

isotime_facette_bordeaux <- ggplot(bordeaux_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Bordeaux")
graph2svg(isotime_facette_bordeaux, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/bordeaux" %>% glue)

isotime_facette_toulouse <- ggplot(toulouse_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Toulouse")
graph2svg(isotime_facette_toulouse, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/toulouse" %>% glue)

isotime_facette_lille <- ggplot(lille_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Lille")
graph2svg(isotime_facette_lille, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/lille" %>% glue)

isotime_facette_nantes <- ggplot(nantes_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Nantes")
graph2svg(isotime_facette_nantes, file="{DVFdata}/presentation/theophile/transit_emp09/facettes/nantes" %>% glue)


# mais aussi comme facette et cumulé
isotime_cumule_idf <- ggplot(idf_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("idf")
graph2svg(isotime_cumule_idf, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/idf" %>% glue)

isotime_cumule_lyon <- ggplot(lyon_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Lyon")
graph2svg(isotime_cumule_lyon, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/lyon" %>% glue)

isotime_cumule_marseille <- ggplot(marseille_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Marseille")
graph2svg(isotime_cumule_marseille, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/marseille" %>% glue)

isotime_cumule_bordeaux <- ggplot(bordeaux_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Bordeaux")
graph2svg(isotime_cumule_bordeaux, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/bordeaux" %>% glue)

isotime_cumule_toulouse <- ggplot(toulouse_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Toulouse")
graph2svg(isotime_cumule_toulouse, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/toulouse" %>% glue)

isotime_cumule_lille <- ggplot(lille_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Lille")
graph2svg(isotime_cumule_lille, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/lille" %>% glue)

isotime_cumule_nantes <- ggplot(nantes_dtm)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass), col=seuil, fill=seuil))+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Nantes")
graph2svg(isotime_cumule_nantes, file="{DVFdata}/presentation/theophile/transit_emp09/facettes_cumulees/nantes" %>% glue)

# ou en mélangeant lyon et paris
data <- rbind(idf_dtm[, ville:="paris"], bordeaux_dtm[, ville:="bordeaux"],lyon_dtm[, ville:="lyon"], marseille_dtm[, ville:="marseille"], toulouse_dtm[, ville:="toulouse"], lille_dtm[, ville:="lille"], nantes_dtm[, ville:="nantes"])
isotime_superposes_parisplus6 <- ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Paris_plus_6")
graph2svg(isotime_superposes_parisplus6, file="{DVFdata}/presentation/theophile/transit_emp09/paris_plus_6" %>% glue)

data <- rbind(bordeaux_dtm[, ville:="bordeaux"],lyon_dtm[, ville:="lyon"], marseille_dtm[, ville:="marseille"], toulouse_dtm[, ville:="toulouse"], lille_dtm[, ville:="lille"], nantes_dtm[, ville:="nantes"])
isotime_superposes_6sansparis <- ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("6_sans_Paris")
graph2svg(isotime_superposes_6sansparis, file="{DVFdata}/presentation/theophile/transit_emp09/6_sans_paris" %>% glue)

# on peut vouloir des fonctions en %
data <- rbind(idf_dtm[, ville:="paris"], bordeaux_dtm[, ville:="bordeaux"],lyon_dtm[, ville:="lyon"], marseille_dtm[, ville:="marseille"], toulouse_dtm[, ville:="toulouse"], lille_dtm[, ville:="lille"], nantes_dtm[, ville:="nantes"])
isotime_paris_plus_6_pourcentage <- ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass)/after_stat(total_mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Paris_plus_6_cumule")
graph2svg(isotime_paris_plus_6_pourcentage, file="{DVFdata}/presentation/theophile/transit_emp09/paris_plus_6_pourcentage" %>% glue)

data <- rbind(bordeaux_dtm[, ville:="bordeaux"],lyon_dtm[, ville:="lyon"], marseille_dtm[, ville:="marseille"], toulouse_dtm[, ville:="toulouse"], lille_dtm[, ville:="lille"], nantes_dtm[, ville:="nantes"])
isotime_6_sans_paris_pourcentage <- ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(mass)/after_stat(total_mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("6_sans_Paris_cumule")
graph2svg(isotime_6_sans_paris_pourcentage, file="{DVFdata}/presentation/theophile/transit_emp09/6_sans_paris_pourcentage" %>% glue)

# on peut vouloir des fonctions en % et en cumul
data <- rbind(idf_dtm[, ville:="paris"], bordeaux_dtm[, ville:="bordeaux"],lyon_dtm[, ville:="lyon"], marseille_dtm[, ville:="marseille"], toulouse_dtm[, ville:="toulouse"], lille_dtm[, ville:="lille"], nantes_dtm[, ville:="nantes"])
isotime_paris_plus_6_cumule_pourcentage <- ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass)/after_stat(total_mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("Paris_plus_6_cumule_pourcentage")
graph2svg(isotime_paris_plus_6_cumule_pourcentage, file="{DVFdata}/presentation/theophile/transit_emp09/paris_plus_6_cumule_pourcentage" %>% glue)

data <- rbind(bordeaux_dtm[, ville:="bordeaux"],lyon_dtm[, ville:="lyon"], marseille_dtm[, ville:="marseille"], toulouse_dtm[, ville:="toulouse"], lille_dtm[, ville:="lille"], nantes_dtm[, ville:="nantes"])
isotime_6_sans_paris_cumule_pourcentage <- ggplot(data)+geom_massity(aes(x=dist, mass=Ind, y=after_stat(cummass)/after_stat(total_mass), col=ville, fill=ville), alpha=0.5)+facet_wrap(~seuil)+theme(plot.title=(element_text(size=40)), legend.title=(element_text(size=40)), legend.text = element_text(size=30))+ggtitle("6_sans_Paris_cumule_pourcentage")
graph2svg(isotime_6_sans_paris_cumule_pourcentage, file="{DVFdata}/presentation/theophile/transit_emp09/6_sans_paris_cumule_pourcentage" %>% glue)