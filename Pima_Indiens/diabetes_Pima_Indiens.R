# 1. Chargement des données et préparation----

# Chargement des packages nécessaires
library(tidyr)
library(dplyr)
library(ggplot2)
library(DescTools)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(FactoInvestigate)

# Charger le jeu de données
data <- read.csv("D:/INSSEDS/datasets/PimaIndiansDiabetes.csv", 
                 header = TRUE,
                 sep = ";") # Remplacez par le chemin correct

# Aperçu des données
head(data)
summary(data)

# Vérification des valeurs manquantes
sum(is.na(data))
# Visualiser le jeu de données entier avec les valeurs manquantes en pourcentage de NA pour chaque variable et global
library(visdat)
vis_miss(data)


# A-5-TRAITEMENT DES VALEURS ABERRANTES OU EXTREMES#
#.................................................#
# BOITE A MOUSTACHES POUR ANALYSER LES VALEURS EXTREMES
par(mfrow=c(2,4), mar=c(4,4,4,4))
boxplot (data$nb_grossesse, main="nb_grossesse",col="lightblue")
boxplot (data$glucose, main="glucose",col="#A52A2A")
boxplot (data$pression_art, main="pression_art",col="#8B8378")
boxplot (data$triceps, main="triceps",col="#00008B")
boxplot (data$insuline, main="insuline",col="orange")
boxplot (data$imc, main="imc",col="green")
boxplot (data$pedigree, main="pedigree",col="#EE6A50")
boxplot (data$age, main="age",col="#CDC673")
# BOITE A MOUSTACHES DES VALEURS EXTREMES WINZORISES
# WINZORISATION DES VALEURS EXTREMES

data$nb_grossesse<- Winsorize(data$nb_grossesse)
data$glucose<- Winsorize(data$glucose)
data$pression_art<- Winsorize(data$pression_art)
data$triceps<- Winsorize(data$triceps)
data$insuline<- Winsorize(data$insuline)
data$imc<- Winsorize(data$imc)
data$pedigree<- Winsorize(data$pedigree)
data$age<- Winsorize(data$age)


# BOITE A MOUSTACHES APRES WINZORISATION DES VALEURS EXTREMES
par(mfrow=c(2,4), mar=c(4,4,4,4))
boxplot (data$nb_grossesse, main="nb_grossesse",col="lightblue")
boxplot (data$glucose, main="glucose",col="#A52A2A")
boxplot (data$pression_art, main="pression_art",col="#8B8378")
boxplot (data$triceps, main="triceps",col="#00008B")
boxplot (data$insuline, main="insuline",col="orange")
boxplot (data$imc, main="imc",col="green")
boxplot (data$pedigree, main="pedigree",col="#EE6A50")
boxplot (data$age, main="age",col="#CDC673")

# 2. Statistique descriptive----
summary(data)
# 3.1. Statistique descriptive univariée
# Vérification des colonnes quantitatives
quant_vars <- names(data)[names(data) != "diabete"] # Exclure la variable catégorique 'diabete'

# Transformation au format long
data_long <- data %>%
  pivot_longer(cols = all_of(quant_vars), 
               names_to = "Variable", 
               values_to = "Valeur")

# Création des graphiques
#graphe des densités
ggplot(data_long, aes(x = Valeur, fill = Variable)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution des variables quantitatives",
       x = "Valeurs",
       y = "Densité")

#boxplot
ggplot(data_long, aes(x = Variable, y = Valeur, fill = Variable)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Set3") + # Palette de couleurs distinctes
  theme_minimal() +
  labs(title = "Distribution des variables quantitatives",
       x = "Variable",
       y = "Valeurs") +
  theme(legend.position = "none") # Supprime la légende si non nécessaire


# Proportion des diabétiques
ggplot(data, aes(diabete, fill = diabete)) + 
  geom_bar() +
  theme_bw() +
  labs(title = "Proportion des diabétiques", x = "Diabetes") +
  theme(plot.title = element_text(hjust = 0.5))


# 3.2. Statistique descriptive bivariée
library(ggplot2)
library(patchwork)

# Fonction bivar_graph pour graphique à barres
bivar_graph <- function(bivar_name, data, output_var) {
  ggplot(data, aes_string(x = bivar_name, fill = output_var)) +
    geom_bar(position = "dodge") + 
    labs(title = paste("Bivarié: ", bivar_name), x = bivar_name, y = "Count") +
    theme_minimal()
}

# Générer la liste des graphiques
graph_list <- list()
for (x in 1:(ncol(data) - 1)) {
  # Utiliser les bonnes données pour chaque graphique
  graph_list[[x]] <- bivar_graph(bivar_name = names(data)[x], 
                                 data = data, 
                                 output_var = 'diabete')
}

# Combiner et afficher les graphiques
wrap_plots(graph_list, ncol = 3)  # Ajustez `ncol` selon vos besoins


# Densité des variables en fonction du diabète

univar_graph <- function(univar_name, univar, data, output_var) {
  
  g_1 <- ggplot(data, aes(x=univar)) + 
    geom_density() + 
    xlab(univar_name) + 
    theme_bw()
  
  g_2 <- ggplot(data, aes(x=univar, fill=output_var)) + 
    geom_density(alpha=0.4) + 
    xlab(univar_name) + 
    theme_bw()
  
  gridExtra::grid.arrange(g_1, g_2, ncol=2, top = paste(univar_name,"variable", "/ [ Skew:",timeDate::skewness(univar),"]"))
  
}

for (x in 1:(ncol(data)-1)) {
  univar_graph(univar_name = names(data)[x], univar = data[,x], data = data, output_var = data[,'diabete'])
}


# 3. Analyse en Composantes Principales (ACP) et Classification----
library(FactoMineR)
library(dplyr)

# Convertir la variable qualitative en facteur
data$diabete <- as.factor(data$diabete)

# Séparer les variables quantitatives et qualitatives
quantitative_data <- data %>% select(where(is.numeric))
qualitative_data <- data %>% select(diabete)

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
data_scaled$diabete <- data$diabete  # On rajoute `diabete` après standardisation

# Trouver l'index de la variable qualitative dans data_scaled
qualitative_index <- which(names(data_scaled) == "diabete")


res.PCA<-PCA(data_scaled,ncp=3,quali.sup=c(9),graph=FALSE)

# Visualisation des individus
plot.PCA(res.PCA, invisible = c('ind.sup'), select = 'contrib 5',
         habillage = "diabete", cex = 0.95, cex.main = 0.95, cex.axis = 0.95, 
         title="Graphe des Individus",
         label = c('ind', 'quali'))

# Visualisation des variables
plot.PCA(res.PCA,choix='var',habillage = 'contrib',select='contrib  5',
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


#write.csv(data_scaled_with_clusters, "diabetes_clust.csv")

# Créer une table croisée entre les clusters et la variable diabete
cluster_diabete_table <- table(res.HCPC$data.clust$clust, data$diabete)

# Afficher la table croisée
print(cluster_diabete_table)


library(ggplot2)

# Convertir les variables en facteur
data_scaled_with_clusters$Cluster <- as.factor(data_scaled_with_clusters$Cluster)
data_scaled_with_clusters$diabete <- as.factor(data_scaled_with_clusters$diabete)

# Créer le graphique
ggplot(data_scaled_with_clusters, aes(x = Cluster, fill = diabete)) +
  geom_bar(position = "dodge") +  # Barres côte à côte
  labs(title = "Répartition des Clusters selon le Statut Diabétique",
       x = "Cluster",
       y = "Nombre d'individus",
       fill = "Statut Diabétique") + 
  theme_minimal() + 
  scale_fill_manual(values = c("neg" = "#00AFBB", "pos" = "#FC4E07"))  # Couleurs personnalisées




