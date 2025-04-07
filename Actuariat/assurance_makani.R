#--------------------------------#
#A-CHARGEMENT ET APERCU DES DONNEES#
#--------------------------------

# A.1. Charger le jeu de données
data <- read.csv("D:/INSSEDS/datasets/assurance_auto_makani.csv", 
                 header = TRUE,
                 sep = ";") # Remplacez par le chemin correct



# A.2.Vérifier des lignes, du résumé et et de la structure du jeu de données

head(data)

summary(data)

str(data)

#--------------------------------
#B-APUREMENT DONNEES#
#--------------------------------

# B.1. Conversion de certaines variables en qualitatives

# Charger les bibliothèques nécessaires
library(dplyr)

# Recodage des variables
data <- data %>%
  mutate(
    Sexe = factor(Sexe, levels = c(0, 1), labels = c("Femme", "Homme")),
    Vehicle_Age = factor(Vehicle_Age, levels = c("< 5", "> 5"), labels = c("Moins de 5 ans", "Plus de 5 ans")),
    transmission = factor(transmission, levels = c("man", "auto"), labels = c("Manuelle", "Automatique")),
    SEVERITY = as.character(SEVERITY),
    trajet = as.character(trajet),
    light_conditions = as.character(light_conditions),
    weather_conditions = as.character(weather_conditions),
    road_surface_conditions = as.character(road_surface_conditions),
    manv = as.character(manv)
  )

# Afficher la structure des données pour vérifier les modifications
str(data)



# B.2. Vérification des valeurs manquantes

sum(is.na(data))

# B.3. Visualiser le jeu de données entier avec les valeurs manquantes en pourcentage de NA pour chaque variable et global

# Charger les bibliothèques nécessaires
library(visdat)
library(dplyr)

# Échantillonner un sous-ensemble de données
data_sample <- data %>% slice_sample(n = 1000)  # Échantillonner 1000 lignes

# Visualiser les valeurs manquantes
vis_miss(data_sample)



# Séparer les variables quantitatives et qualitatives
library(dplyr)
quantitative_data <- data %>% select(where(is.numeric))

# B.4. Créer un boxplot pour chaque variable quantitative avant Winsorization
for (column in colnames(quantitative_data)) {
  boxplot(quantitative_data[[column]],
          main = paste("Boxplot de", column, "avant Winsorization"),
          ylab = column,
          col = "lightblue",
          border = "darkblue")
  
  # Ajouter une pause pour permettre de voir chaque graphique
  Sys.sleep(2) # Ajustez cette valeur pour contrôler la durée d'affichage de chaque graphique
}

# B.5. Fonction pour winsoriser une variable
winsorize <- function(x, lower_quantile = 0.01, upper_quantile = 0.99) {
  q_lower <- quantile(x, lower_quantile, na.rm = TRUE) # Ajout de na.rm = TRUE pour ignorer les NAs
  q_upper <- quantile(x, upper_quantile, na.rm = TRUE)
  x[x < q_lower] <- q_lower
  x[x > q_upper] <- q_upper
  return(x)
}

# B.6. Appliquer la fonction winsorize à toutes les colonnes quantitatives
quantitative_data[] <- lapply(quantitative_data, function(col) winsorize(col))

# B.7. Créer un boxplot pour chaque variable quantitative après Winsorization
for (column in colnames(quantitative_data)) {
  boxplot(quantitative_data[[column]],
          main = paste("Boxplot de", column, "après Winsorization"),
          ylab = column,
          col = "red",
          border = "darkblue")
  
  # Ajouter une pause pour permettre de voir chaque graphique
  Sys.sleep(2) # Ajustez cette valeur pour contrôler la durée d'affichage de chaque graphique
}

#--------------------------------
#C-STAT DESCRIPTIVE#
#--------------------------------

# C.1. Stat des quantis

# C.1.1 Calcul des statistiques descriptives pour chaque colonne
stats <- data.frame(
  mean = round(sapply(quantitative_data, mean, na.rm = TRUE), 2),
  sd = round(sapply(quantitative_data, sd, na.rm = TRUE), 2),
  min = sapply(quantitative_data, min, na.rm = TRUE),
  max = sapply(quantitative_data, max, na.rm = TRUE),
  median = sapply(quantitative_data, median, na.rm = TRUE),
  IQR = sapply(quantitative_data, function(x) IQR(x, na.rm = TRUE))
)

# Affichage du tableau formaté
library(knitr)
kable(stats)


# C.1.2. Graphiques des quantis

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

# C.2. Stat des qualis

library(ggplot2)
library(dplyr)

# Séparer les variables quantitatives et qualitatives (sauf ACCIDENT)
qual_vars <- data %>% select(where(~ is.factor(.) || is.character(.))) %>% select(-ACCIDENT)

# Variable cible
target_var <- "ACCIDENT"

# Pour les variables qualitatives
for (var in names(qual_vars)) {
  p <- ggplot(data, aes_string(x = var, fill = target_var)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Répartition de", var, "par rapport à", target_var),
         x = var, y = "Nombre d'observations", fill = target_var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# Pour les variables quantitatives
for (var in names(quantitative_data)) {
  p <- ggplot(data, aes_string(x = target_var, y = var, fill = target_var)) +
    geom_boxplot() +
    labs(title = paste("Distribution de", var, "selon", target_var),
         x = target_var, y = var, fill = target_var) +
    theme_minimal()
  print(p)
}



# Tableau statistique pour la variable "ACCIDENT"
library(dplyr)
library(tidyr)

# Variables qualitatives
qual_vars <- data %>% select(where(~ is.factor(.) || is.character(.))) %>% select(-ACCIDENT)

cat_stat_tables <- lapply(names(qual_vars), function(var) {
  data %>%
    group_by(!!sym(var), ACCIDENT) %>%
    summarise(Effectif = n(), .groups = "drop") %>%
    group_by(!!sym(var)) %>%
    mutate(Fréquence = round(100 * Effectif / sum(Effectif), 1)) %>%
    mutate(Variable = var) %>%
    rename(Level = !!sym(var))
}) %>% bind_rows()

# Réorganisation du tableau
cat_stat_table <- cat_stat_tables %>%
  select(Variable, Level, ACCIDENT, Effectif, Fréquence)

# Variables quantitatives
quant_vars <- data %>% select(where(is.numeric))

quant_stat_table <- quantitative_data %>%
  mutate(ACCIDENT = data$ACCIDENT) %>%
  pivot_longer(cols = -ACCIDENT, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, ACCIDENT) %>%
  summarise(
    Moyenne = round(mean(Value, na.rm = TRUE), 2),
    Médiane = round(median(Value, na.rm = TRUE), 2),
    Écart_type = round(sd(Value, na.rm = TRUE), 2),
    Min = round(min(Value, na.rm = TRUE), 2),
    Max = round(max(Value, na.rm = TRUE), 2),
    .groups = "drop"
  )

library(knitr)
library(kableExtra)

# Variables qualitatives
kable(cat_stat_table, caption = "Tableau de contingence des variables qualitatives avec ACCIDENT") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Variables quantitatives
kable(quant_stat_table, caption = "Statistiques descriptives des variables quantitatives selon ACCIDENT") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))




# Transformer les données pour les rendre compatibles avec facet_wrap
data_long <- data %>%
  select(all_of(names(qual_vars)), all_of(target_var)) %>%
  pivot_longer(cols = -all_of(target_var), names_to = "variable", values_to = "value")

# Créer le plot avec facet_wrap
p <- ggplot(data_long, aes(x = value, fill = !!sym(target_var))) +
  geom_bar(position = "dodge") +
  labs(title = "Répartition des variables qualitatives par rapport à ACCIDENT",
       x = "Valeurs des variables", y = "Nombre d'observations", fill = "ACCIDENT") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ variable, scales = "free_x")

print(p)






# Transformer les données pour les rendre compatibles avec facet_wrap
data_long <- data %>%
  select(all_of(names(quantitative_data)), all_of(target_var)) %>%
  pivot_longer(cols = -all_of(target_var), names_to = "variable", values_to = "value")

# Créer le plot avec facet_wrap
p <- ggplot(data_long, aes(x = !!sym(target_var), y = value, fill = !!sym(target_var))) +
  geom_boxplot() +
  labs(title = "Distribution des variables quantitatives selon ACCIDENT",
       x = "ACCIDENT", y = "Valeurs des variables", fill = "ACCIDENT") +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y")

print(p)




#--------------------------------
#D-MODELISATION DE "accident"
#--------------------------------


#  1. Binarisation de la variable cible fréquence

data$accident <- ifelse(data$frequence >= 2, 1, 0)
data$accident <- as.factor(data$accident)
str(data$accident)

# 2. Création du modèle de régression logistique

modele <- glm(accident ~ Age + Sexe + Vehicle_Age, data = data, family = binomial)

summary(modele)


# CALCUL DES OOD RATIO ET DES EFFETS MARGINAUX

# affichage des paramètres estimés
PARAMETRES = coefficients(modele)
PARAMETRES

# Calculons les intervalles de confiance
confint(modele)

# calcul des odd ratio
ODD_RATIO = exp(coefficients(modele))
ODD_RATIO


# Calcul des effets marginaux
library("margins")
margins_model <- margins(modele)
summary(margins_model)



# Analyse des résidus
res.m <- rstudent(modele) 
plot(res.m,pch=15,cex=.5,ylab="Residus",ylim=c(-3,3))
abline(h=c(-2,0,2),lty=c(2,1,2))


# CALCULER LE TMC : taux de mauvais classement et Matrice de Confusion

# calcul des probabilités
data$PROBABILITE_PREDITE <- predict(modele, data, 
                                    type="response")


# Transformer les probabilité en modalite predites
data$MODALITE_PREDITE <- ifelse(data$PROBABILITE_PREDITE < 0.5, "no accident" , "accident")


# Caclculer le taux de mauvais classement à partir de la matrice de confusion
Matrice_confusion = table(data$accident, data$MODALITE_PREDITE)
Matrice_confusion 

#L'AUC

library(pROC)
library(ggplot2)

# Calculer la courbe ROC
roc_curve <- roc(data$accident, data$PROBABILITE_PREDITE)

# Calculer l'AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Créer un dataframe pour ggplot
roc_df <- data.frame(
  specificity = 1 - roc_curve$specificities,
  sensitivity = roc_curve$sensitivities
)

# Visualiser la courbe ROC
ggplot(roc_df, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Courbe ROC", x = "1 - Spécificité", y = "Sensibilité") +
  theme_minimal()


# PREDICTION DE LA PROBABILITE DE FAIRE UN ACCIDENT

# affichage des paramètres estimés
PARAMETRES = coefficients(modele)
PARAMETRES


# calcul des probabilités sous R
data$PROBABILITE_PREDITE <- predict(modele, data, 
                                    type="response")
head(data)


#-----------------------------------------
#E-MODELISATION POISSONNIENE DE "frequence"
#-----------------------------------------

# Faisons la représentation graphique
plot(table(data$frequence))


library(ggplot2)

# Créer un tableau de fréquences
freq_table <- table(data$frequence)

# Convertir le tableau de fréquences en data.frame pour ggplot
freq_df <- as.data.frame(freq_table)

# Visualiser la distribution des fréquences avec ggplot
ggplot(freq_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution de la fréquence", x = "Catégories", y = "Fréquence") +
  theme_minimal()


#1.Calculer la moyenne empirique (λ estimé) :
lambda_hat <- mean(data$frequence)
cat("Lambda estimé :", lambda_hat)
#2. réer un tableau de fréquence observée :

observed_freq <- table(data$frequence)
#3. créer la fréquence théorique sous hypothèse de loi de Poisson :

# Transformer le tableau en data.frame pour faciliter la manipulation
obs_df <- as.data.frame(observed_freq)
colnames(obs_df) <- c("frequence", "obs")

# Convertir la variable frequence en entier
obs_df$frequence <- as.numeric(as.character(obs_df$frequence))

# Calcul des fréquences théoriques (même n et même lambda)
n_total <- sum(obs_df$obs)
obs_df$theo <- dpois(obs_df$frequence, lambda_hat) * n_total
#4. isualiser la comparaison graphique :
library(ggplot2)

ggplot(obs_df, aes(x = frequence)) +
  geom_bar(aes(y = obs), stat = "identity", fill = "blue", alpha = 0.7, width = 0.6) +
  geom_line(aes(y = theo), color = "red", size = 1.2) +
  geom_point(aes(y = theo), color = "red", size = 2) +
  labs(title = "Comparaison de la distribution observée avec une loi de Poisson",
       x = "Fréquence",
       y = "Nombre d'observations") +
  theme_minimal()

chisq.test(x = obs_df$obs, p = obs_df$theo / sum(obs_df$theo))

mean(data$frequence)
var(data$frequence)

#Ici la moyenne n’est pas très loin de la variance, on peut utiliser la régression
#de poisson. Si par contre la Variance est très supérieur à la moyenne :
#ce phénomène est appelé «sur-dispersion».


#Test d’adéquation à la loi de poisson
# H0 :la distribution ne suit pas la loi X
# H1 :la distribution suit la loi X
#Si p-value < 0.05, on rejette H0.
#Si p-value > 0.05, on ne peut rejeter H0.
# chargement du package
library(fitdistrplus)
# test d’adéquation avec une distribution de poisson
#1. Estimation des paramètres par la méthode du maximum de vraisemblance
fpois <- fitdist(data$frequence, "pois")
summary(fpois)
#2. Tests d’adéquation
fpois <- fitdist(data$frequence, "pois")
gofstat(fpois)


#NB : p.value inférieur à 5%, les données suivent une loi de poisson


# II.Construction du modèle BINOMOALE NEGATIVE

library(MASS)
# II.1.Spécification du modèle 

fnb <- glm.nb(frequence ~ Age+Sexe+Vehicle_Age+ 
                fuel_type+ manufacture_year, data = data)
summary(fnb)  # Résumé du modèle ajusté

# II.2. AIC et BIC 

aic_val <- AIC(fnb)
aic_val

bic_val <- BIC(fnb)
bic_val

# II.2.Vérification des hypothèses du modèle

# Chargement des packages nécessaires
library(MASS)       # pour glm.nb
library(car)        # pour vif
library(DHARMa)     # pour les résidus simulés


# 1. Résidus simulés pour vérifier la distribution, l'hétéroscédasticité, l'indépendance
sim_res <- simulateResiduals(fittedModel = fnb)
plot(sim_res)  # Génère 4 graphiques de vérification

# 2. Multicolinéarité via VIF
vif_vals <- vif(fnb)
print(vif_vals)
