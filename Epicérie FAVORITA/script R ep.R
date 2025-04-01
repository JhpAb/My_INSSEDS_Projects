# I - DEFINITION DES DIFFERENTES FONCTIONS----
library(abjp)
#---------------------------------------------------------------------------#

#---------------------------------------------------------------------------#
# tableau.Qt()  # tableau statistique de variable quantitative
# graph.Qt()    # graphiques de variable quantitative
# resume.Numeric.Qt()    # resume numerique de variable quantitative
# tableau.Ql()   # tableau statistique de variable qualitative
# graph.Ql()     # graphique de variable qualitative
#---------------------------------------------------------------------------#
# liaison.2Qt()   # liaison entre deux variables quantitatives
# tableau.2Ql()    # tableaux statistiques de deux variables qualitatives
# graph.2Ql()      # graphiques de deux variables qualitatives
# liaison.2Ql()    # liaison entre deux variables qualitatives
# liaison.Qt.Ql # liaison entre une variable quantitative et une variable qualitative
#---------------------------------------------------------------------------#

# II - IMPORTATION DES DONNEES----


# 1- Chemin d'accès du jeux de données
setwd("C:/Users/hp/Desktop/FORMATION INSSEDS/jeudonnees")
# 2- Chargement du jeux de données
epicerie = read.table("epicerie.csv",
                     header=TRUE, 
                     sep=",", 
                     check.names=FALSE,
                     row.names=1,
                     stringsAsFactors = TRUE
) 

# III - EXPLORATION DES DONNEES----

# 1- Affichage du jeu données 
print(epicerie)

# 2- Afficher la structure du jeu de données
str(epicerie)
summary(epicerie)
# 3- Création du Dictionnaire des données
library("dplyr")
dictionnaireDonnees <- tibble::tribble(
  ~VARIABLE, ~NATURE, ~DESCRIPTION,  ~MODALITES,
  "PB", "Quantitative",  "Prix du Baril", "Numérique décimal",
  "BC", "Qualitative",  "Balance Commerciale",  "• favorable 
  • défavorable",
  "TXCH", "Quantitative",  "Taux de Chômage", "Numérique décimal",
  "PIBH", "Quantitative",  "PIB par Habitant",  "Numérique décimal",
  "CN",  "Quantitative",  "Cours du Naira",  "Numérique décimal",
  "SB", "Qualitative", "Solde Budgétaire", "• excédent 
  • déficit",
  "TXCA",  "Quantitative",  "Taux de Croissance Annuel",  "Numérique décimal",
  "BP", "Qualitative", "Balance des paiements", "• équilibre
  • déséquilibré",
)
## Afficher le dictionnaire des données
dictionnaireDonnees

## Afficher le dictionnaire des donnée sous forme de tableau
library(rhandsontable)
rhandsontable(dictionnaireDonnees,
              rowHeaders = NULL,
              digits = 3,
              useTypes = FALSE,
              search = FALSE,
              width = NULL,
              height = NULL
)


# 4- visualiser les données manquantes
library(visdat)

## Visualiser le jeu de données entier 
## avec les valeurs manquantes en 
## pourcentage de NA pour chaque variable et global
vis_miss(epicerie)

## Visualiser la position des données manquantes
vis_dat(epicerie)


# IV - PRETRAITEMENT DES DONNEES----


# 1- Traitement des doublons


# 2- Identifier le nombre d'individus ayant des donnees manquantes
# affiche les individus avec les valeurs manquantes
epicerie[!complete.cases(epicerie),]

# Calcul du nombre d'individu ayant des valeurs manquantes
nrow(epicerie[!complete.cases(epicerie),]) 

# Calcul du nombre total de lignes du jeu données manquantes
nrow(epicerie)

# 3- Calcul de la proportion des individus avec des valeurs manquantes
pourcentageDonneeManquant=round(nrow(epicerie[!complete.cases(epicerie),]) / nrow(epicerie),2)*100
pourcentageDonneeManquant

# 4- Traitement des valeurs manquantes
# Remplir les valeurs inconnues en explorant 
# les similitudes entre les cas
library(DMwR2)
epicerie <- knnImputation(epicerie, k = 10, scale = TRUE, meth = "median")
summary(epicerie)
epicerie

# Visualisation des données
library(visdat)
vis_dat(epicerie)
vis_miss(epicerie)

# 5 - Traitement des valeurs aberrantes et extrêmes
# a- Affichage des boites à moustache
par(mfrow=c(2,3), mar=c(3,3,3,3))
boxplot(epicerie$store_nbr , main = "store_nbr", col = "red",las=1)
boxplot(epicerie$sales, main = "sales", col = "blue",las=1)
boxplot(epicerie$onpromotion, main = "onpromotion", col = "red",las=1)
par(mfrow=c(1,1), mar=c(3,3,3,3))

# b- Correction des valeurs aberrantes
# et extrêmes par la technique de la winzorisation
summary(epicerie)

library(DescTools)
epicerie$store_nbr <- Winsorize(epicerie$store_nbr)
epicerie$sales <- Winsorize(epicerie$sales)
epicerie$onpromotion <- Winsorize(epicerie$onpromotion)


summary(epicerie)

# c- Affichage à nouveau des boîtes à moustache
par(mfrow=c(2,3), mar=c(3,3,3,3))
boxplot(epicerie$store_nbr , main = "store_nbr", col = "red",las=1)
boxplot(epicerie$sales, main = "sales", col = "red",las=1)
boxplot(epicerie$onpromotion, main = "onpromotion", col = "red",las=1)
par(mfrow=c(1,1), mar=c(3,3,3,3))







# 1-1 Etude de la variable sales
# organiser les ventes totales par mois
epicerie_sales.mois= subset (epicerie,select= c(1,4)) 
#Chargement des packages
library(readr)
library(dplyr)
library(xts)
library(ggplot2)
library(plotly)
# Sélectionner la colonne sales
sales_epicerie <- epicerie %>%
  select(date, sales)

# Définir le fuseau horaire (par exemple, "UTC" pour le temps universel coordonné)
Sys.setenv(TZ = "UTC")

# Conversion de la colonne "date" en format de date
sales_epicerie$date <- as.Date(sales_epicerie$date, format = "%Y-%m-%d")

# Création de la série temporelle des ventes totales par mois
sales_ts <- xts(sales_epicerie$sales, 
             order.by = sales_epicerie$date)

# Graphique de série temporelle des ventes totales par mois
sales_ts.plot= ggplot(data = ventes_par_mois, aes(x = Periode)) +
  geom_line(aes(y = Ventes_Totales)) +
  labs(title = "Evolution des Ventes", x = "Années", y = "Ventes")
ggplotly(sales_ts.plot)

# Calcul de la moyenne
mean_sales <- mean(ventes_par_mois$Ventes_Totales)

# Calcul de la médiane
median_sales <- median(ventes_par_mois$Ventes_Totales)

# Calcul de l'écart type
sd_sales <- sd(ventes_par_mois$Ventes_Totales)

# Création du graphique de dispersion
plot(sales ~ date, data = epicerie, main = "Tendance linéaire", xlab = "Années", ylab = "Ventes")

# Tendance linéaire (régression linéaire)
linear_trend0 <- lm(sales ~ date, data = epicerie)

# Affichage de la tendance linéaire
abline(linear_trend0, col = "red")

# Interprétation de la régression linéaire
if (coef(linear_trend0)[2] > 0) {
  cat("La tendance linéaire montre une augmentation des ventes au fil du temps.")
} else if (coef(linear_trend0)[2] < 0) {
  cat("La tendance linéaire montre une diminution des ventes au fil du temps.")
} else {
  cat("La tendance linéaire suggère que la tendance des ventes est relativement stable.")
}

# Affichage des statistiques descriptives
cat("Moyenne des ventes :", mean_sales, "milliards de dollars\n")
cat("Médiane des ventes :", median_sales, "milliards de dollars\n")
cat("Écart type des ventes :", sd_sales, "milliards de dollars\n")
