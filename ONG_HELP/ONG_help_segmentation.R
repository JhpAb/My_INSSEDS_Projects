# Charger les bibliothèques nécessaires
library(FactoMineR)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(fpc)
library(missMDA)
library(Factoshiny)
library(FactoInvestigate)


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






# Séparer les variables quantitatives et qualitatives
quantitative_data <- data %>% select(where(is.numeric))
qualitative_data <- data %>% select(continent)

# Vérifier que quantitative_data n'est pas vide
if (ncol(quantitative_data) == 0) {
  stop("Aucune colonne numérique trouvée dans le jeu de données.")
}

# Standardisation des données quantitatives
data_scaled <- scale(quantitative_data)

# Convertir en data frame et restaurer les noms de colonnes
data_scaled <- as.data.frame(data_scaled)
colnames(data_scaled) <- colnames(quantitative_data)

# Ajouter la variable qualitative au dataset standardisé
data_scaled$continent <- data$continent  # On rajoute `continent` après standardisation

# Trouver l'index de la variable qualitative dans data_scaled
qualitative_index <- which(names(data_scaled) == "continent")


res.PCA<-PCA(data_scaled,ncp=3,quali.sup=c(10),graph=FALSE)
plot.PCA(res.PCA)

plot.PCA(res.PCA,invisible=c('ind.sup'),
         select='contrib  20',
         habillage='contrib',
         title="Graphe des individus",cex=1.25,
         cex.main=1.25,cex.axis=1.25,
         label =c('ind','quali'))

# Visualisation des individus avec un ajustement des chevauchements d'étiquettes
plot.PCA(res.PCA, invisible = c('ind.sup'), select = 'contrib 20',
         habillage = "continent", cex = 0.95, cex.main = 0.95, cex.axis = 0.95, 
         title="Graphe des Individus",
         label = c('ind', 'quali'),
         ggrepel.max.overlaps = 20)  # Augmenter le nombre de chevauchements autorisés


# Visualisation des variables
plot.PCA(res.PCA,choix='var',habillage = 'contrib',select='contrib  20',
         unselect=0,cex=1.5,cex.main=1.5,cex.axis=1.5,
         title="Graphe des Variables")

fviz_pca_var(res.PCA, repel = TRUE, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title= "Graphe des Variables")

# Visualisation des éboulis (variance expliquée par composante)
fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50), 
         ylab = "Pourcentage des Variance Expliquées",
         title = "Variance expliquée par composante")



summary(res.PCA)
dimdesc(res.PCA)

res.PCA$var$coord # Contribution des variables aux axes
res.PCA$ind$coord  # Coordonnées des individus dans le nouvel espace réduit.
res.PCA$var$contrib
res.PCA$eig # Variance expliquée par chaque composante.

# Afficher les coordonnées des individus sur chaque dimension
head(res.PCA$ind$coord)







res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Arbre Hierarchique')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Plan Factoriel')

summary(res.HCPC)



# Afficher les données avec les clusters attribués
head(res.HCPC$data.clust)


# Ajouter les clusters au dataframe initial
data_scaled_with_clusters <- cbind(data_scaled, Cluster = res.HCPC$data.clust$clust)

# Afficher les premiers individus avec leur cluster
head(data_scaled_with_clusters)


#write.csv(data_scaled_with_clusters, "country_clust.csv")

# Créer une table croisée entre les clusters et la variable continent
cluster_continent_table <- table(res.HCPC$data.clust$clust, data$continent)

# Afficher la table croisée
print(cluster_continent_table)


library(ggplot2)

# Convertir les variables en facteur
data_scaled_with_clusters$Cluster <- as.factor(data_scaled_with_clusters$Cluster)
data_scaled_with_clusters$continent <- as.factor(data_scaled_with_clusters$continent)

# Créer le graphique
ggplot(data_scaled_with_clusters, aes(x = Cluster, fill = continent)) +
  geom_bar(position = "dodge") +  # Barres côte à côte
  labs(title = "Répartition des Clusters selon le Continent",
       x = "Cluster",
       y = "Nombre d'individus",
       fill = "Continent") +
  theme_minimal() +
  scale_fill_manual(values = c("Africa" = "#00AFBB", "Americas" = "#FC4E07", "Asia" = "#E7B800", "Europe" = "#00AFBB", "Oceania" = "#FC4E07"))  # Couleurs personnalisées



