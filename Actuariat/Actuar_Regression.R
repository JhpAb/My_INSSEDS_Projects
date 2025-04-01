#--------------------------------#
#A-CHARGEMENT ET APERCU DES DONNEES#
#--------------------------------

# A.1. Charger le jeu de données
data <- read.csv("D:/INSSEDS/datasets/actuarNV1.csv", 
                 header = TRUE,
                 sep = ";") # Remplacez par le chemin correct



# A.2.Vérifier des lignes, du résumé et et de la structure du jeu de données

head(data)

summary(data)

str(data)

#--------------------------------
#B-APUREMENT DONNEES#
#--------------------------------

# B.1. Conversion de certaines variables en qualitatives (character)

data$nocontrat <- as.character(data$nocontrat)
data$no <- as.character(data$no)
data$region <- as.character(data$region)
data$marque <- as.character(data$marque)
str(data)


# B.2. Vérification des valeurs manquantes

sum(is.na(data))

# B.3. Visualiser le jeu de données entier avec les valeurs manquantes en pourcentage de NA pour chaque variable et global

library(visdat)
vis_miss(data)


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

# C.2.1. Créer le graphique en barres empilées
ggplot(data, aes(x = zone, fill = garantie)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution des garanties par zone",
       x = "Zone",
       y = "Proportion") +
  theme_minimal()



# C.2.2. Graphique en barres pour la variable "zone"
ggplot(data, aes(x = zone)) +
  geom_bar() +
  labs(title = "Distribution des zones",
       x = "Zone",
       y = "Fréquence") +
  theme_minimal()

# C.2.3. Graphique en barres pour la variable "garantie"
ggplot(data, aes(x = garantie)) +
  geom_bar() +
  labs(title = "Distribution des garanties",
       x = "Garantie",
       y = "Fréquence") +
  theme_minimal()



# C.2.4. Graphique en barres pour la variable "zone"
p1 <- ggplot(data, aes(x = zone)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution des zones",
       x = "Zone",
       y = "Fréquence") +
  theme_minimal()

# C.2.4. Graphique en barres pour la variable "garantie"
p2 <- ggplot(data, aes(x = garantie)) +
  geom_bar(fill = "salmon") +
  labs(title = "Distribution des garanties",
       x = "Garantie",
       y = "Fréquence") +
  theme_minimal()

# C.2.5. Combiner les deux graphiques en une seule image
grid.arrange(p1, p2, ncol = 2)


# Tableau statistique pour la variable "zone"
table_zone <- table(data$zone)
table_zone

# Tableau statistique pour la variable "garantie"
table_garantie <- table(data$garantie)
table_garantie




#--------------------------------
#D-MODELISATION DU COUT
#--------------------------------

# Charger les bibliothèques nécessaires
library(ggplot2)
library(car)
library(lmtest)
library(corrplot)
# Calculer la matrice de corrélation
correlation_matrix <- cor(quantitative_data)

# Afficher la matrice de corrélation
correlation_matrix


# Créer le corrplot
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Calculer la matrice de corrélation
cor_matrix <- cor(quantitative_data, use = "complete.obs")

# Pour une visualisation plus détaillée avec ggplot2
library(reshape2)
melted_cor <- melt(cor_matrix)
ggplot(data = melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson Correlation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

#D-1-SPECIFICATION DU MODELE

# D.1. Ajouter une petite constante pour éviter les valeurs nulles ou négatives
summary(data$cout)
table(data$cout <= 0)  # Compte le nombre de valeurs négatives ou nulles

epsilon <- 1e-10
data$log_cout <- log(abs(data$cout) + epsilon)

# Vérifier les valeurs transformées
summary(data$log_cout)


data$log_cout <- log(data$puissance + data$nbre +data$exposition + data$agevehicule + 1)


# D.2. Modèle de régression avec la variable transformée

model_log_cout <- lm(log_cout~ nbre + puissance + exposition + agevehicule, data = data)

# Résumé du modèle
summary(model_log_cout)


# R²
r_squared <- summary(model_log_cout)$r.squared

# RMSE
observed_values <- data$log_cout
predicted_values <- fitted(model_log_cout)
mse <- mean((observed_values - predicted_values)^2)
rmse <- sqrt(mse)

# MAE
mae <- mean(abs(observed_values - predicted_values))

# Affichage des résultats
cat("R²:", r_squared, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")



#D-2-VERIFICATION DES HYPOTHESE DU MODELE

# D.2.1. Vérification de la linéarité 
# (Vérification de l'homoscédasticité des résidus Graphique des résidus et test de Breusch-Pagan)
df_residuals <- data.frame(fitted_values = fitted(model_log_cout), residuals = residuals(model_log_cout))

ggplot(df_residuals, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Graphique des résidus vs valeurs ajustées", 
       x = "Valeurs ajustées", y = "Résidus") +
  theme_minimal()

# D.2.2. Vérification de l'autocorrélation (Durbin-Watson)
library(lmtest)
dwtest(model_log_cout)


# D.2.3. Vérification de la normalité des erreurs (Q-Q plot et test de Shapiro-Wilk)
ggplot(data.frame(residuals = residuals(model_log_cout)), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(col = "red") +
  labs(title = "Q-Q plot des résidus") +
  theme_minimal()

# D.2.4. Vérification de la multi-colinéarité (Variance Inflation Factor - VIF)
library(car)
vif(model_log_cout)


# Calcul du VIF et visualisation
vif_values <- vif(model_log_cout)
vif_df <- data.frame(variables = names(vif_values), vif = vif_values)

# Graphique avec ggplot2
ggplot(vif_df, aes(x = reorder(variables, vif), y = vif)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variance Inflation Factor (VIF)", x = "Variables", y = "VIF") +
  theme_minimal() +
  coord_flip()


# D.2.5. Examen des outliers et points influents (Leverage et Cook's distance)
# Identifie les points influents
influence_plot <- influence.measures(model_log_cout)
summary(influence_plot)

# Visualisation de Cook's Distance
plot(cooks.distance(model_log_cout), type = "h", main="Cook's Distance", ylab="Cook's Distance")


  # Créer un dataframe pour Cook's distance
df_cooks <- data.frame(index = seq_along(cooks.distance(model_log_cout)), 
                       cooks_dist = cooks.distance(model_log_cout))

# Graphique avec ggplot2
ggplot(df_cooks, aes(x = index, y = cooks_dist)) +
  geom_bar(stat = "identity") +
  labs(title = "Cook's Distance", x = "Index", y = "Cook's Distance") +
  theme_minimal()


# D.2.6. Autres tests de robustesse 
# Test de White
library(sandwich)
white_test <- bptest(model_log_cout, studentize = FALSE) 
summary(white_test)

# D.2.7. Prédiction et validation croisée (si nécessaire)
# Installation du package si nécessaire
# install.packages("caret")

library(caret)

# Validation croisée (par exemple, 10-fold cross-validation)
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(log_cout ~ nbre + puissance + exposition + agevehicule, 
                  data = data, method = "lm", trControl = train_control)
print(model_cv)


#--------------------------------
#E-MODELISATION DU LA FREQUENCE DES SINISTRES
#--------------------------------

#E-SPECIFICATION DU MODELE

# Modèle de Poisson pour la fréquence des sinistres
model_nbre <- glm(formula = nbre ~ exposition + agevehicule + bonus + marque + 
                    carburant, family = poisson, data = data)


# Résumé du modèle
summary(model_nbre)


#E-VERIFICATION DES HYPOTHESES DU MODELE

# 1. Vérification de l'indépendance des erreurs avec le test de Durbin-Watson
dw_test <- dwtest(model_nbre)
print(dw_test)

# Si la p-value est proche de 0, cela signifie qu'il y a autocorrélation.
# Sinon, on peut supposer que les erreurs sont indépendantes.

# 2. Vérification de la distribution des résidus
# Utilisation de ggplot2 pour un histogramme et un QQ-plot.

# Histogramme des résidus
ggplot(data.frame(residus = residuals(model_nbre)), aes(x = residus)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogramme des résidus", x = "Résidus", y = "Fréquence") +
  theme_minimal()

# QQ-plot des résidus
ggplot(data.frame(residus = residuals(model_nbre)), aes(sample = residus)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ-plot des résidus") +
  theme_minimal()

# 3. Vérification de la surdispersion
# Calcul de la dispersion
dispersion <- sum(residuals(model_nbre, type = "pearson")^2) / model_nbre$df.residual
print(paste("Dispersion : ", dispersion))

# Si la dispersion est bien supérieure à 1, il y a probablement surdispersion.

# 4. Vérification de la relation entre les variables explicatives et la variable dépendante
# Utilisation de ggplot2 pour visualiser les relations.

# Par exemple, tracer la relation entre 'exposition' et 'nbre' de sinistres
ggplot(data, aes(x = exposition, y = nbre)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Relation entre exposition et sinistres", x = "Exposition", y = "Nombre de sinistres") +
  theme_minimal()

# Répéter pour d'autres variables explicatives si nécessaire.
# Exemple pour 'agevehicule' :
ggplot(data, aes(x = agevehicule, y = nbre)) +
  geom_point(color = "green", alpha = 0.6) +
  labs(title = "Relation entre agevehicule et sinistres", x = "Age du véhicule", y = "Nombre de sinistres") +
  theme_minimal()

# 5. Vérification de la multicolinéarité avec le Variance Inflation Factor (VIF)
# Un VIF élevé (> 10) indique une multicolinéarité entre les variables explicatives.

vif_values <- vif(model_nbre)
print(vif_values)

# Si une ou plusieurs variables ont un VIF supérieur à 10, cela peut indiquer un problème de multicolinéarité.
# Vous pouvez envisager de supprimer ou combiner ces variables.



#--------------------------------
#F-MODELISATION DE LA VARIABLE NOMBRE
#--------------------------------

#F-SPECIFICATION DU MODELE

# Dichotomiser la variable 'nbre'
data$nbre_dichotomise <- ifelse(data$nbre >= 2, 1, 0)

summary(data$nbre_dichotomise)
table(data$nbre_dichotomise)

# Modèle de régression logistique pour la probabilité de survenance des sinistres
model_proba <- glm(nbre_dichotomise ~ exposition + agevehicule , family = binomial, data = data)

# Résumé du modèle
summary(model_proba)


#F-VERIFICATION DES HYPOTHESES DU MODELE

# Charger les bibliothèques nécessaires
library(car)
library(pROC)
library(ggplot2)

# 1. Vérification de la linéarité de la relation entre les variables indépendantes et le log des cotes
# Calculer le logit (log des cotes)
logit <- log(predict(model_proba, type = "response") / (1 - predict(model_proba, type = "response")))

# Tracer la relation entre chaque variable continue et le logit avec ggplot2
ggplot(data, aes(x = exposition, y = logit)) +
  geom_point() +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Exposition vs Logit", x = "Exposition", y = "Logit") +
  theme_minimal()

ggplot(data, aes(x = agevehicule, y = logit)) +
  geom_point() +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Age du véhicule vs Logit", x = "Age du véhicule", y = "Logit") +
  theme_minimal()

# 2. Vérification de la multicolinéarité avec le VIF (Variance Inflation Factor)
# Calculer les VIF
vif_values <- vif(model_proba)
print(vif_values)
# Si VIF > 10, cela indique une forte multicolinéarité entre les variables

# 3. Identification des points influents avec la distance de Cook
cook_dist <- cooks.distance(model_proba)

# Tracer la distance de Cook avec ggplot2
cook_data <- data.frame(ID = 1:length(cook_dist), CookDistance = cook_dist)
ggplot(cook_data, aes(x = ID, y = CookDistance)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = 4 / nrow(data), color = "red", linetype = "dashed") +
  labs(title = "Distance de Cook", x = "ID des observations", y = "Distance de Cook") +
  theme_minimal()

# 4. Vérification de l’homoscedasticité des résidus
# Calculer les résidus de deviance
residuals_dev <- residuals(model_proba, type = "deviance")

# Tracer les résidus par rapport aux valeurs prédites
predicted_values <- predict(model_proba, type = "response")
residuals_data <- data.frame(Predicted = predicted_values, Residuals = residuals_dev)

ggplot(residuals_data, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Résidus de deviance vs Valeurs prédites", x = "Valeurs prédites", y = "Résidus de deviance") +
  theme_minimal()

# 5. Vérification de la spécification du modèle (tester un modèle avec interactions)
# Essayer un modèle avec interaction entre 'exposition' et 'agevehicule'
model_interaction <- glm(nbre_dichotomise ~ exposition * agevehicule + bonus + region, 
                         family = binomial, data = data)

# Résumé du modèle avec interaction
summary(model_interaction)

# Comparer les performances du modèle avec et sans interactions

# 6. Vérification des performances du modèle avec la courbe ROC et l'AUC
# Charger les bibliothèques nécessaires
library(pROC)
library(ggplot2)

# Calculer les prédictions du modèle
predictions <- predict(model_proba, type = "response")

# Créer la courbe ROC
roc_curve <- roc(data$nbre_dichotomise, predictions)

# Tracer la courbe ROC avec ggplot2
ggroc(roc_curve, color = "blue", size = 1) +
  labs(title = "Courbe ROC", x = "Taux de faux positifs (FPR)", y = "Taux de vrais positifs (TPR)") +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.3, label = paste("AUC =", round(auc(roc_curve), 3)), color = "red", size = 5)
