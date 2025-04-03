# A - IMPORTATION DE DONNEES#----
data <- read.csv("D:/INSSEDS/datasets/segment.csv")

# A-1-EXPLORATION DES DONNEES#
#.......................#
str(data)
summary(data)

# A-2-RECODAGE DES VARIABLES
#.......................#
data$AcceptedCmp1<-factor(data$AcceptedCmp1, labels=c("Non","Oui"))
data$AcceptedCmp2<-factor(data$AcceptedCmp2, labels=c("Non","Oui"))
data$AcceptedCmp3<-factor(data$AcceptedCmp3, labels=c("Non","Oui"))
data$AcceptedCmp4<-factor(data$AcceptedCmp4, labels=c("Non","Oui"))
data$AcceptedCmp5<-factor(data$AcceptedCmp5, labels=c("Non","Oui"))
data$Complain<-factor(data$Complain, labels=c("Non","Oui"))
data$Response<-factor(data$Response, labels=c("Non","Oui"))

# Convertir la colonne 'Dt_Customer' en format Date
# Cette étape est cruciale car nous devons manipuler les dates 
# correctement pour les analyses temporelles
data$Dt_Customer <- as.Date(data$Dt_Customer, format = "%d-%m-%Y")

# LES STATISTIQUES SOMMAIRES SUR LES VARIABLES
str(data)
summary(data)


# A-3-DICTIONNAIRE DES DONNEES#
#............................#
library("dplyr")
dico_donnees <- tibble::tribble(
  ~VARIABLE, ~NATURE, ~DESCRIPTION,  ~MODALITES,
  "ID", "Identifiant",  "Identifiant unique", "Numérique entier",
  "Year_Birth", "Qualitative",  "Année de naissance de l’individu",  "Année",
  "Education", "Qualitative",  " Le niveau d’éducation le plus élevé atteint par l’individu", "2n Cycle
  Basic
  Graduation
  Master
  PhD",
  "Marital_Status", "Qualitative",  "L’état matrimonial de l’individu",  "Absurd
  Alone
  Divorced
  Married
  Single
  Together
  Widow
  YOLO",
  "Income",  "Quantitative",  "Le revenu annuel de l’individu",  "Numérique entier",
  "Kidhome", "Quantitative", "Le nombre de jeunes enfants dans le ménage", "Numérique entier",
  "Teenhome", "Quantitative",  "Le nombre d'adolescents dans le foyer", "Numérique entier",
  "Dt_Customer",  "Date",  "Date à laquelle le client a été inscrit pour la première fois ou est devenu une partie de la base de données de 
   l'entreprise",  "Année/Mois/Jour",
  "Recency",  "Quantitative",  "Le nombre de jours depuis le dernier achat ou la dernière interaction",  "Numérique entier",
  "MntWines",  "Quantitative",  "Le montant dépensé en vins",  "Numérique entier",
  "MntFruits",  "Quantitative",  "Le montant dépensé en fruits",  "Numérique entier",
  "MntMeatProducts",  "Quantitative",  "Le montant dépensé en produits carnés",  "Numérique entier",
  "MntFishProducts",  "Quantitative",  "Le montant dépensé en produits de la pêche",  "Numérique entier",
  "MntSweetProducts",  "Quantitative",  "Le montant dépensé en produits sucrés",  "Numérique entier",
  "MntGoldProds",  "Quantitative",  "Le montant dépensé en produits aurifères",  "Numérique entier",
  "NumDealsPurchases",  "Quantitative",  "Le nombre d'achats effectués avec une remise ou dans le cadre d'une offre",  "Numérique entier",
  "NumWebPurchases",  "Quantitative",  "Le nombre d'achats effectués via le site Web de l'entreprise",  "Numérique entier",
  "NumCatalogPurchases",  "Quantitative",  "Le nombre d'achats effectués via des catalogues",  "Numérique entier",
  "NumStorePurchases",  "Quantitative",  "Le nombre d'achats effectués dans les magasins physiques",  "Numérique entier",
  "NumWebVisitsMonth",  "Quantitative",  "Le nombre de visites sur le site Web de l'entreprise en un mois",  "Numérique entier", 
  "AcceptedCmp1",  "Qualitative",  "Indicateur binaire (1 ou 0) indiquant si l'individu a accepté la première campagne marketing",  "Non
  Oui",
  "AcceptedCmp2",  "Qualitative",  "Indicateur binaire (1 ou 0) indiquant si l'individu a accepté la deuxième campagne marketing",  "Non
  Oui",
  "AcceptedCmp3",  "Qualitative",  "Indicateur binaire (1 ou 0) indiquant si l'individu a accepté la troisième campagne marketing",  "Non
  Oui",
  "AcceptedCmp4",  "Quantitative",  "Indicateur binaire (1 ou 0) indiquant si l'individu a accepté la quatrième campagne marketing",  "Non
  Oui",
  "AcceptedCmp5",  "Qualitative",  "Indicateur binaire (1 ou 0) indiquant si l'individu a accepté la cinquième campagne marketing",  "Non
  Oui",
  "Complain",  "Qualitative",  "Indicateur binaire (1 ou 0) indiquant si la personne a déposé une plainte",  "Numérique entier",
  "Z_CostContact",  "Quantitative",  "Un coût constant associé à la prise de contact avec un client",  "Numérique entier",
  "Z_Revenue",  "Quantitative",  "Un revenu constant associé à une réponse de campagne réussie",  "Numérique entier",
  "Response",  "Qualitative",  "Indicateur binaire (1 ou 0) indiquant si l'individu a répondu à la campagne marketing",  "Non
  Oui",
)
   
dico_donnees

library(rhandsontable)
rhandsontable(dico_donnees, rowHeaders = NULL,
              digits = 3, useTypes = FALSE, search = FALSE,
              width = NULL, height = NULL)

# A-4-TRAITEMENT DES VALEURS MANQUANTES
#......................................#


# Visualiser les valeurs manquantes

library(Amelia)  # Pour visualiser les NA

# Visualisation avec un graphique

missmap(data, main = "Valeurs manquantes dans le jeu de données")

# AFFICHER LES INDIVIDUS AVEC LES VALEURS MANQUANTES

data[!complete.cases(data),]

# IDENTIFIER LE NOMBRE D'INDIVIDUS AYANT DES DONNES MANQUANTES

nrow(data[!complete.cases(data),])

# CALCUL DE LA PROPORTION DES DONNEES MANQUANTES

nrow(data[!complete.cases(data),])/nrow (data)

# Visualiser le jeu de données entier avec les valeurs manquantes 
# en pourcentage de NA pour chaque variable et global

library(visdat)
vis_miss(data)

# Visualiser la position des données manquantes

vis_dat(data)

# Traitement des valeurs manquantes
# Pour ce cas, nous allons utiliser la moyenne pour les colonnes numériques
data <- data
for(i in 1:ncol(data)) {
  if(is.numeric(data[, i])) {
    data[is.na(data[, i]), i] <- median(data[, i], na.rm = TRUE)
  }
}

# Vérification après traitement
sum(is.na(data))

#Suppression des lignes des donnees manquantes
#data <- na.omit(data)

# Visualiser le jeu de données entier

library(visdat)
vis_miss(data)

vis_dat(data)

# A-5-TRAITEMENT DES VALEURS ABERRANTES OU EXTREMES#
#.................................................#
# BOITE A MOUSTACHES POUR ANALYSER LES VALEURS EXTREMES
par(mfrow=c(2,4), mar=c(4,4,4,4))
boxplot (data$Year_Birth, main="Year_Birth",col="lightblue")
boxplot (data$Income, main="Income",col="#A52A2A")
boxplot (data$Kidhome, main="Kidhome",col="#8B8378")
boxplot (data$Teenhome, main="Teenhome",col="#00008B")
boxplot (data$Recency, main="Recency",col="orange")
boxplot (data$MntWines, main="MntWines",col="green")
boxplot (data$MntFruits, main="MntFruits",col="#EE6A50")
boxplot (data$MntMeatProducts, main="MntMeatProducts",col="#CDC673")
boxplot (data$MntFishProducts, main="MntFishProducts",col="#FFFF00")
boxplot (data$MntSweetProducts, main="MntSweetProducts",col="#FF3030")
boxplot (data$MntGoldProds, main="MntGoldProds",col="#CD8162")
boxplot (data$NumDealsPurchases, main="NumDealsPurchases",col="#FFFFE0")
boxplot (data$NumCatalogPurchases, main="NumCatalogPurchases",col="#00868B")
boxplot (data$NumStorePurchases, main="NumStorePurchases",col="#EE5C42")
boxplot (data$NumWebVisitsMonth, main="NumWebVisitsMonth",col="#8B7B8B")

# WINZORISATION DES VALEURS EXTREMES
library(DescTools)
data$Year_Birth<- Winsorize(data$Year_Birth)
data$Income<- Winsorize(data$Income)
data$Kidhome<- Winsorize(data$Kidhome)
data$Teenhome<- Winsorize(data$Teenhome)
data$Recency<- Winsorize(data$Recency)
data$MntWines<- Winsorize(data$MntWines)
data$MntFruits<- Winsorize(data$MntFruits)
data$MntMeatProducts<- Winsorize(data$MntMeatProducts)
data$MntFishProducts<- Winsorize(data$MntFishProducts)
data$MntSweetProducts<- Winsorize(data$MntSweetProducts)
data$MntGoldProds<- Winsorize(data$MntGoldProds)
data$NumDealsPurchases<- Winsorize(data$NumDealsPurchases)
data$NumCatalogPurchases<- Winsorize(data$NumCatalogPurchases)
data$NumWebVisitsMonth<- Winsorize(data$NumWebVisitsMonth)

# BOITE A MOUSTACHES DES VALEURS EXTREMES WINZORISES
par(mfrow=c(2,4), mar=c(4,4,4,4))
boxplot (data$Year_Birth, main="Year_Birth",col="lightblue")
boxplot (data$Income, main="Income",col="#A52A2A")
boxplot (data$Kidhome, main="Kidhome",col="#8B8378")
boxplot (data$Teenhome, main="Teenhome",col="#00008B")
boxplot (data$Recency, main="Recency",col="orange")
boxplot (data$MntWines, main="MntWines",col="green")
boxplot (data$MntFruits, main="MntFruits",col="#EE6A50")
boxplot (data$MntMeatProducts, main="MntMeatProducts",col="#CDC673")
boxplot (data$MntFishProducts, main="MntFishProducts",col="#FFFF00")
boxplot (data$MntSweetProducts, main="MntSweetProducts",col="#FF3030")
boxplot (data$MntGoldProds, main="MntGoldProds",col="#CD8162")
boxplot (data$NumDealsPurchases, main="NumDealsPurchases",col="#FFFFE0")
boxplot (data$NumCatalogPurchases, main="NumCatalogPurchases",col="#00868B")
boxplot (data$NumStorePurchases, main="NumStorePurchases",col="#EE5C42")
boxplot (data$NumWebVisitsMonth, main="NumWebVisitsMonth",col="#8B7B8B")


# B - ANALYSE DESCRIPTIVE----
  
 #B.1 Analyse descriptive sur les variables quantitatives

#1. Analyse base des ventes mensuelles par année.

# 1.1. Convertir la colonne Dt_Customer en format Date

data$Dt_Customer <- as.Date(data$Dt_Customer, format = "%d-%m-%Y")

# 1.2. Extraire l'année et ajouter à la dataframe

data$Year <- format(data$Dt_Customer, "%Y")

data$YearMonth <- format(data$Dt_Customer, "%Y-%m")# Créer une colonne avec Année-Mois au format "YYYY-MM"

# 1.3. Tableau des statistiques descriptives 



# 1.4. Statistiques descriptives sur les années (summary_by_year)

# 1.4.1. Résumé Numérique des ventes

summary_by_year_sales <- aggregate(cbind(MntWines, MntFruits, 
                                   MntMeatProducts, MntFishProducts, 
                                   MntSweetProducts, MntGoldProds) ~ Year, 
                                   data = data, summary)

# 1.4.2. Résumé Numérique  des achats

summary_by_year_purchase <- aggregate(cbind(NumDealsPurchases, NumWebPurchases, 
                                            NumCatalogPurchases, NumStorePurchases) ~ Year, 
                                            data = data, summary)

# 1.4.3. Résumé Numérique  des var quali

summary_by_year_Var_ql <- aggregate(cbind(Education, Marital_Status, 
                                   AcceptedCmp1, AcceptedCmp2, 
                                   AcceptedCmp3, AcceptedCmp4, 
                                   AcceptedCmp5, Complain, Response) ~ Year, 
                                   data = data, summary)


# 2. Visualisation des ventes totales mensuelles par année


# 2.1. 

#Étape 1 : Calcul des ventes mensuelles par catégorie

# Regrouper par Année-Mois et calculer la somme pour chaque catégorie
library(dplyr)
sales_by_category <- data %>%
  group_by(YearMonth) %>%
  summarise(
    MntWines = sum(MntWines, na.rm = TRUE),
    MntFruits = sum(MntFruits, na.rm = TRUE),
    MntMeatProducts = sum(MntMeatProducts, na.rm = TRUE),
    MntFishProducts = sum(MntFishProducts, na.rm = TRUE),
    MntSweetProducts = sum(MntSweetProducts, na.rm = TRUE),
    MntGoldProds = sum(MntGoldProds, na.rm = TRUE)
  )
# Sauvegarde des données
#write.csv(sales_by_category, "sales_by_category.csv", row.names = FALSE)
#Étape 1 : Ajouter les mois manquants

# Générer une séquence complète des mois en partant de juillet 2012 jusqu'à la dernière date
all_months <- seq.Date(from = as.Date("2012-07-01"), to = max(data$Dt_Customer), by = "month")
all_months_df <- data.frame(YearMonth = format(all_months, "%Y-%m"))

# Fusionner avec les ventes pour inclure les mois manquants
sales_by_category_complete <- merge(all_months_df, sales_by_category, by = "YearMonth", all.x = TRUE)

# Remplacer les NA par 0 pour les ventes manquantes
sales_by_category_complete[is.na(sales_by_category_complete)] <- 0
#Étape 3 : Reshaper les données pour un graphique empilé

library(reshape2)
sales_by_category_long <- melt(sales_by_category_complete, id.vars = "YearMonth", 
                               variable.name = "Catégories", value.name = "Sales")
#write.csv(sales_by_category_long, "sales_by_category_long.csv", row.names = FALSE)
#Étape 4 : Création du graphique empilé

library(ggplot2)
ggplot(sales_by_category_long, aes(x = YearMonth, y = Sales, fill = Catégories)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Ventes mensuelles par catégorie de marchandises",
    x = "Mois et Année", 
    y = "Ventes totales"
  ) +
  scale_fill_brewer(palette = "Set2") + # Palette de couleurs harmonieuses
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_discrete(breaks = unique(sales_by_category_complete$YearMonth)[seq(1, nrow(sales_by_category_complete), by = 3)]) # Afficher une étiquette tous les 3 mois

#
ggplot(sales_by_category_long, aes(x = YearMonth, y = Sales, fill = Catégories)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Ventes mensuelles par année", x = "Mois (Année-Mois)", y = "Ventes totales", fill = "Catégories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Création de la courbe d'évolution


library(ggplot2)

ggplot(sales_by_category_long, aes(x = YearMonth, y = Sales, color = Catégories, group = Catégories)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Évolution mensuelle des ventes par catégorie",
    x = "Mois et Année", 
    y = "Ventes totales"
  ) +
  theme_minimal() +
  theme(
    # Centrer le titre
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    # Placer l'étiquette de l'axe Y juste au-dessus de l'axe
    axis.title.y = element_text(vjust = 2),
    # Incliner les étiquettes de l'axe X
    axis.text.x = element_text(angle = 45, hjust = 1), 
    # Positionner la légende
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_discrete(breaks = unique(sales_by_category_complete$YearMonth)
                   [seq(1, nrow(sales_by_category_complete), by = 3)]) + # Espacement des étiquettes
  scale_color_brewer(palette = "Set2")


# Calculer les ventes totales par année (somme des ventes de différents produits)
sales_by_year <- aggregate(cbind(MntWines, MntFruits, 
                                 MntMeatProducts, MntFishProducts, 
                                 MntSweetProducts, MntGoldProds) ~ Year, data = data, sum)
#write.csv(sales_by_year, "sales_by_year.csv", row.names = FALSE)
# Visualisation des ventes totales par année

sales_by_year_long <- reshape2::melt(sales_by_year, id.vars = "Year")
library(ggplot2)
library(scales) # Pour formater les nombres

ggplot(sales_by_year_long, aes(x = Year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Ventes totales par année", x = "Année", y = "Ventes totales", fill = "Catégorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma())  # Formater les chiffres en entier


# 2.2
#Étape 1 : Calcul des ventes mensuelles par plateforme

# Regrouper par Année-Mois et calculer la somme des achats selon la plateforme
library(dplyr)

NB_by_purch <- data %>%
  group_by(YearMonth) %>%
  summarise(
    NumDealsPurchases = sum(NumDealsPurchases, na.rm = TRUE),
    NumWebPurchases = sum(NumWebPurchases, na.rm = TRUE),
    NumCatalogPurchases = sum(NumCatalogPurchases, na.rm = TRUE),
    NumStorePurchases = sum(NumStorePurchases, na.rm = TRUE),
    
  )
# Sauvegarde des données
#write.csv(NB_by_purch, "NB_by_purch.csv", row.names = FALSE)
#Étape 2 : Fusionner avec les ventes pour inclure les mois manquants


NB_by_purch_complete <- merge(all_months_df, NB_by_purch, by = "YearMonth", all.x = TRUE)

# Remplacer les NA par 0 pour les ventes manquantes

NB_by_purch_complete[is.na(NB_by_purch_complete)] <- 0

#Étape 3 : Reshaper les données pour un graphique empilé

library(reshape2)
NB_by_purch_long <- melt(NB_by_purch_complete, id.vars = "YearMonth", 
                         variable.name = "Catégories", value.name = "Achats")
#Étape 4 : Création du graphique empilé

library(ggplot2)
ggplot(NB_by_purch_long, aes(x = YearMonth, y = Achats, fill = Catégories)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Achats mensuels selon la plateforme",
    x = "Mois et Année", 
    y = "Achats totales"
  ) +
  scale_fill_brewer(palette = "Set2") + # Palette de couleurs harmonieuses
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_discrete(breaks = unique(NB_by_purch_complete$YearMonth)[seq(1, nrow(NB_by_purch_complete), by = 3)]) # Afficher une étiquette tous les 3 mois
#
ggplot(NB_by_purch_long, aes(x = YearMonth, y = Achats, fill = Catégories)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Achats mensuels selon la plateforme", x = "Mois (Année-Mois)", y = "Total d'achats", fill = "Catégories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Création de la courbe d'évolution


library(ggplot2)

ggplot(NB_by_purch_long, aes(x = YearMonth, y = Achats, color = Catégories, group = Catégories)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Évolution mensuelle des achats  selon la plateforme",
    x = "Mois (Année-Mois)", 
    y = "Total d'achats"
  ) +
  theme_minimal() +
  theme(
    # Centrer le titre
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    # Placer l'étiquette de l'axe Y juste au-dessus de l'axe
    axis.title.y = element_text(vjust = 2),
    # Incliner les étiquettes de l'axe X
    axis.text.x = element_text(angle = 45, hjust = 1), 
    # Positionner la légende
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_discrete(breaks = unique(NB_by_purch_complete$YearMonth)
                   [seq(1, nrow(NB_by_purch_complete), by = 3)]) + # Espacement des étiquettes
  scale_color_brewer(palette = "Set2")


# Calculer les achats totales par année (somme des achats de différents produits)
NB_by_year <- aggregate(cbind(NumDealsPurchases, NumWebPurchases, 
                              NumCatalogPurchases, NumStorePurchases) ~ Year, data = data, sum)
#write.csv(NB_by_year, "NB_by_year.csv", row.names = FALSE)
# Visualisation des achats totales par année

NB_by_year_long <- reshape2::melt(NB_by_year, id.vars = "Year")
library(ggplot2)
library(scales) # Pour formater les nombres

ggplot(NB_by_year_long, aes(x = Year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Achats totals par année", x = "Année", y = "Achats totals", fill = "Catégorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma())  # Formater les chiffres en entier


 #B.2 Analyse descriptive sur les variables qualitatives

# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(gridExtra)

# Fonction pour créer un graphique à barres pour une variable qualitative
plot_bar <- function(variable, data) {
  ggplot(data, aes_string(x = variable)) +
    geom_bar(fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = variable, x = NULL, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Liste des variables qualitatives
variables <- c("Marital_Status", "Education", "AcceptedCmp1", "AcceptedCmp2",
               "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

# Générer les graphiques pour chaque variable
plots <- lapply(variables, plot_bar, data = data)

# Afficher les graphiques dans une grille (par exemple, 3 lignes et 3 colonnes)
grid.arrange(grobs = plots, ncol = 3)



# Fonction pour calculer les fréquences et les pourcentages d'une variable qualitative
calculate_frequencies_and_percentages <- function(variable, data) {
  freq <- table(data[[variable]])
  df <- as.data.frame(freq)
  colnames(df) <- c("Category", "Frequency")  # Renommer la colonne de fréquence
  
  # Calcul des pourcentages
  total <- sum(df$Frequency)  # Total des fréquences
  df$Percentage <- (df$Frequency / total) * 100  # Calcul du pourcentage
  
  return(df)
}

# Calcul des fréquences et pourcentages pour Marital_Status
marital_status_freq <- calculate_frequencies_and_percentages("Marital_Status", data)

# Calcul des fréquences et pourcentages pour Education
education_freq <- calculate_frequencies_and_percentages("Education", data)

# Liste des autres variables qualitatives
other_variables <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", 
                     "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

# Calcul des fréquences et pourcentages pour les autres variables
other_frequencies_list <- lapply(other_variables, function(var) {
  df <- calculate_frequencies_and_percentages(var, data)
  colnames(df)[2] <- paste0(var, "_Frequency")  # Renommer la colonne de fréquence en fonction du nom de la variable
  colnames(df)[3] <- paste0(var, "_Percentage")  # Renommer la colonne de pourcentage
  return(df)
})

# Fusionner les résultats pour créer un tableau unique pour les autres variables
frequencies_other <- Reduce(function(x, y) merge(x, y, by = "Category", all = TRUE), other_frequencies_list)

# Afficher les résultats
print("Fréquences Marital_Status :")
print(marital_status_freq)

print("Fréquences Education :")
print(education_freq)

print("Fréquences et pourcentages des autres variables :")
print(frequencies_other)




# Suppression des colonnes Year et YearMonth de data avec l'index des colonnes
data <- data[, -c(30, 31)]


# C - Segmentation des clients----

data$Year_Birth <- 2024 - data$Year_Birth
names(data)[names(data) == "Year_Birth"] <- "Age"

# Créer une colonne 'FamilySize'
data$FamilySize <- data$Kidhome + data$Teenhome

# Compter le nombre d'enfants par familles 
table(data$FamilySize)

#chargement des packages 
library(shiny)
library(FactoInvestigate)
library(FactoMineR)
library(Factoshiny)

 # C - 1 - ACP

#graphs
dfaux <- data.frame(data[,-c(4,8,21,22,23,24,25)],
                    Education_Complain=paste0(data[,'Education'],
                                              data[,'Complain']))
res.PCA<-PCA(dfaux,quali.sup=c(3,19,22,24),
             quanti.sup=c(4,23),graph=FALSE)

plot.PCA(res.PCA,choix='var',habillage = 'contrib',select='cos2  0.6',unselect=0,
         title="Graphe des variables",cex=1.4,cex.main=1.4,cex.axis=1.4,col.quanti.sup='#0000FF')
plot.PCA(res.PCA,invisible=c('ind.sup'),select='cos2  0.7',habillage=24,
         title="Graphe des individus",
         cex=1.8,cex.main=1.8,cex.axis=1.8,label =c('ind','quali'))

#values
summary(res.PCA)

#automatic description of axes
dimdesc(res.PCA)

#
res.PCA<-PCA(dfaux,ncp=3,quali.sup=c(3,19,22,24),
             quanti.sup=c(4,23),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,
               consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',
          title='Arbre Hiérarchique')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,
          title='Plan Factoriel')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,
          angle=60,title='Arbre Hiérarchique sur le Plan Factoriel')

#
summary(res.HCPC)


# Extraire les individus avec leurs clusters
clusters_data <- res.HCPC$data.clust

# Vérifiez les colonnes disponibles
head(clusters_data)
# Ajouter les coordonnées factorielles des individus
clusters_data$Dim.1 <- res.PCA$ind$coord[, 1]  # Première dimension
clusters_data$Dim.2 <- res.PCA$ind$coord[, 2]  # Deuxième dimension

library(ggplot2)

# Visualisation des clusters sur le plan factoriel

# Calculer les centres des clusters
centers <- aggregate(cbind(Dim.1, Dim.2) ~ clust, data = clusters_data, FUN = mean)

# Visualisation avec les centres
ggplot(clusters_data, aes(x = Dim.1, y = Dim.2, color = as.factor(clust))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = centers, aes(x = Dim.1, y = Dim.2), 
             color = "black", size = 4, shape = 17) +
  labs(title = "Clusters avec Centres",
       x = "Dim 1",
       y = "Dim 2",
       color = "Cluster") +
  theme_minimal() +
  theme(text = element_text(size = 14))


## Visualiser les clusters sur le graphique avec l'âge et le revenu
ggplot(clusters_data, aes(x =Income , y =Age , color = as.factor(clust))) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Segmentation des clients (CAH)", 
       x = "Revenu", 
       y = "Âge", 
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "bottom")


## Répartition (histogramme) des Clients par Cluster

ggplot(clusters_data, aes(x = clust)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des Clients par Cluster", x = "Cluster", y = "Nombre de Clients")


#

ggplot(clusters_data, aes(x = Income, fill = as.factor(clust))) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribution des Revenus par Cluster",
       x = "Revenu",
       y = "Densité",
       fill = "Cluster") +
  theme_minimal() +
  theme(text = element_text(size = 14))
#

ggplot(clusters_data, aes(x = as.factor(clust), y = Income, fill = as.factor(clust))) +
  geom_boxplot() +
  labs(title = "Boxplot des Revenus par Cluster",
       x = "Cluster",
       y = "Revenu",
       fill = "Cluster") +
  theme_minimal() +
  theme(text = element_text(size = 14))

#analyse descriptive des clusters
#moyenne

mean_clust = clusters_data %>% 
  group_by(clust) %>% 
  summarise(
    Nb_Clients = n(),
    Avg_Age = mean(Age, na.rm = TRUE),
    Avg_Income = mean(Income, na.rm = TRUE),
    Avg_FamilySize = mean(FamilySize, na.rm = TRUE)
  )
#median

median_clust = clusters_data %>%
  group_by(clust) %>%
  summarise(
    Nb_Clients = n(),
    med_Age = median(Age, na.rm = TRUE),
    med_Income = median(Income, na.rm = TRUE),
    med_FamilySize = median(FamilySize, na.rm = TRUE)
  )
# Suppression des colonnes dim1 et dim2 de clusters_data avec l'index des colonnes
clusters_data <- clusters_data[, -c(26, 27)]
# Sauvegarde des données
#write.csv(median_clust , "clusters_segment.csv", row.names = FALSE)
