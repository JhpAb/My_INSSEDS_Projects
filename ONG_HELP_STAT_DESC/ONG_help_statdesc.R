# Charger le jeu de données
data <- read.csv("D:/INSSEDS/datasets/help_international.csv", 
                 header = TRUE,
                 sep = ";") # Remplacez par le chemin correct

# Vérifier les premières lignes du jeu de données
head(data)

summary(data)

# Vérification des valeurs manquantes
sum(is.na(data))
# Visualiser le jeu de données entier avec les valeurs manquantes en pourcentage de NA pour chaque variable et global
library(visdat)
vis_miss(data)


#data_cleaned <- na.omit(data)
data <- na.omit(data)

# Visualiser le jeu de données 
library(visdat)
vis_miss(data)



library(dplyr)

# Séparer les variables quantitatives et qualitatives
quantitative_data <- data %>% select(where(is.numeric))

# Créer un boxplot pour chaque variable quantitative avant Winsorization
for (column in colnames(quantitative_data)) {
  boxplot(quantitative_data[[column]],
          main = paste("Boxplot de", column, "avant Winsorization"),
          ylab = column,
          col = "lightblue",
          border = "darkblue")
  
  # Ajouter une pause pour permettre de voir chaque graphique (si nécessaire)
  Sys.sleep(2) # Ajustez cette valeur pour contrôler la durée d'affichage de chaque graphique
}

# Fonction pour winsoriser une variable
winsorize <- function(x, lower_quantile = 0.01, upper_quantile = 0.99) {
  q_lower <- quantile(x, lower_quantile, na.rm = TRUE) # Ajout de na.rm = TRUE pour ignorer les NAs
  q_upper <- quantile(x, upper_quantile, na.rm = TRUE)
  x[x < q_lower] <- q_lower
  x[x > q_upper] <- q_upper
  return(x)
}

# Appliquer la fonction winsorize à toutes les colonnes quantitatives
quantitative_data[] <- lapply(quantitative_data, function(col) winsorize(col))

# Créer un boxplot pour chaque variable quantitative après Winsorization
for (column in colnames(quantitative_data)) {
  boxplot(quantitative_data[[column]],
          main = paste("Boxplot de", column, "après Winsorization"),
          ylab = column,
          col = "red",
          border = "darkblue")
  
  # Ajouter une pause pour permettre de voir chaque graphique (si nécessaire)
  Sys.sleep(2) # Ajustez cette valeur pour contrôler la durée d'affichage de chaque graphique
}

# Stat des quantis

# Calcul des statistiques descriptives pour chaque colonne
stats <- data.frame(
  mean = sapply(quantitative_data, mean, na.rm = TRUE),
  sd = sapply(quantitative_data, sd, na.rm = TRUE),
  min = sapply(quantitative_data, min, na.rm = TRUE),
  max = sapply(quantitative_data, max, na.rm = TRUE),
  median = sapply(quantitative_data, median, na.rm = TRUE),
  IQR = sapply(quantitative_data, function(x) IQR(x, na.rm = TRUE))
)

# Affichage du tableau formaté
library(knitr)
kable(stats)



# Graphique des quantis

library(ggplot2)
library(gridExtra)
library(dplyr)

# Créer une liste de graphiques pour chaque variable quantitative
plots <- lapply(names(quantitative_data), function(var) {
  ggplot(data, aes(x = .data[[var]])) +  # Correction ici
    geom_histogram(bins = 15, fill = "skyblue", color = "black") +
    labs(title = paste("Histogramme de", var), x = var, y = "Fréquence") +
    theme_minimal()
})

# Afficher les graphiques (ajuster selon le nombre de variables)
do.call(grid.arrange, c(plots, ncol = 2))  # Organiser en 2 colonnes




# Graphique des qualis
library(countrycode)
# Ajouter une colonne "continent" à votre jeu de données
data$continent <- countrycode(data$pays, 'country.name', 'continent')
table(data$continent)
# Convertir la variable qualitative en facteur
data$continent <- as.factor(data$continent)

library(ggplot2)
# Créer un dataframe pour la visualisation
df_continent <- as.data.frame(table(data$continent))
colnames(df_continent) <- c("Continent", "Nombre_Pays")

# Graphique en barres
ggplot(df_continent, aes(x = Continent, y = Nombre_Pays, fill = Continent)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Nombre de pays par continent", x = "Continent", y = "Nombre de pays") +
  theme(legend.position = "none")




# Ajouter une colonne pour les labels
df_continent$fraction <- df_continent$Nombre_Pays / sum(df_continent$Nombre_Pays)
df_continent$label <- paste(df_continent$Continent, round(df_continent$fraction * 100, 1), "%")

# Graphique en secteurs
ggplot(df_continent, aes(x = "", y = Nombre_Pays, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Répartition des pays par continent") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))
