# Installation et chargement des bibliothèques nécessaires
# install.packages("tidyverse")
# install.packages("MASS")
# install.packages("pROC")
# install.packages("lmtest")
#install.packages("randomForest")
library(randomForest)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(readr)
library(pROC)
library(lmtest)

# Chargement des données
data <- read.csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/3.standardized_data.csv")

### Section 1 : Models ###
cat("\n### Models ###\n")

#### Modèle 1 : Régression linéaire (Matières premières) ####
cat("\n### Modèle 1 : Matières premières ###\n")
model1 <- lm(sp500.close ~ gold.open + oil.open + silver.open + platinum.open + palladium.open, data = data)
summary(model1)

# Visualisation des valeurs observées vs prédites
data$predicted_model1 <- predict(model1)


ggplot(data, aes(x = sp500.close, y = predicted_model1)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Valeurs observées vs prédites (Matières premières)",
       x = "Valeurs observées", y = "Valeurs prédites")


#### Modèle 2 : Régression linéaire (Indicateurs macroéconomiques) ####
cat("\n### Modèle 2 : Macroéconomiques ###\n")
model2 <- lm(sp500.close ~ GDP + CPI + us_rates_., data = data)
summary(model2)
model2 <- lm(sp500.close ~ GDP + us_rates_., data = data)
summary(model2)

data$predicted_model2 <- predict(model2)
ggplot(data, aes(x = sp500.close, y = predicted_model2)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Valeurs observées vs prédites (Indicateurs macroéconomiques)",
       x = "Valeurs observées", y = "Valeurs prédites")


#### Modèle 3 : Régression logistique pour prédire la volatilité ####
cat("\n### Modèle 3 : Prédiction de la Volatilité ###\n")
# Création de la variable binaire pour la volatilité
data <- data %>%
  mutate(volatility = ifelse(abs((sp500.close - lag(sp500.close)) / lag(sp500.close)) > 0.02, 1, 0)) %>%
  na.omit()


# Ajustement du modèle logistique => general to specific 
model3 <- glm(volatility ~ GDP + CPI + us_rates_. + eur_usd + usd_chf + gold.open + oil.open + silver.open + platinum.open + palladium.open, 
              data = data, family = binomial(link = "logit"))
# oil.open
model3 <- glm(volatility ~ GDP + CPI + us_rates_. + eur_usd + usd_chf + gold.open + silver.open + platinum.open + palladium.open, 
              data = data, family = binomial(link = "logit"))
#silver.open
model3 <- glm(volatility ~ GDP + CPI + us_rates_. + eur_usd + usd_chf + gold.open + platinum.open + palladium.open, 
              data = data, family = binomial(link = "logit"))
#CPI
model3 <- glm(volatility ~ GDP + us_rates_. + eur_usd + usd_chf + gold.open + platinum.open + palladium.open, 
              data = data, family = binomial(link = "logit"))
#usd_chf
model3 <- glm(volatility ~ GDP + us_rates_. + eur_usd + gold.open + platinum.open + palladium.open, 
              data = data, family = binomial(link = "logit"))
summary(model3)

# Calcul des effets marginaux moyens (EMM)
cat("\n### Effets Marginaux Moyens (EMM) pour Modèle 3 ###\n")

# Calcul de la moyenne de lambda(z)
lambda_z_chap <- mean(dlogis(predict(model3, type = "link")))

# Calcul des EMM pour chaque variable explicative
logit_EMM_GDP <- coef(model3)["GDP"] * lambda_z_chap
logit_EMM_us_rates <- coef(model3)["us_rates_."] * lambda_z_chap
logit_EMM_eur_usd <- coef(model3)["eur_usd"] * lambda_z_chap
logit_EMM_gold_open <- coef(model3)["gold.open"] * lambda_z_chap
logit_EMM_platinum_open <- coef(model3)["platinum.open"] * lambda_z_chap
logit_EMM_palladium_open <- coef(model3)["palladium.open"] * lambda_z_chap

# Affichage des résultats
cat("EMM pour GDP :", logit_EMM_GDP, "\n")
cat("EMM pour us_rates_ :", logit_EMM_us_rates, "\n")
cat("EMM pour eur_usd :", logit_EMM_eur_usd, "\n")
cat("EMM pour gold_open :", logit_EMM_gold_open, "\n")
cat("EMM pour platinum_open :", logit_EMM_platinum_open, "\n")
cat("EMM pour palladium_open :", logit_EMM_palladium_open, "\n")







### Section 2 : Assessment ###
cat("\n### Assessment ###\n")

# Tests diagnostiques pour les modèles linéaires
cat("\nDiagnostics pour le Modèle 1 :\n")
shapiro_test1 <- shapiro.test(residuals(model1))  # Test de normalité
bptest1 <- bptest(model1)                         # Test d'hétéroscédasticité
cat("Shapiro-Wilk p-value :", shapiro_test1$p.value, "\n")
cat("Breusch-Pagan p-value :", bptest1$p.value, "\n")

cat("\nDiagnostics pour le Modèle 2 :\n")
shapiro_test2 <- shapiro.test(residuals(model2))  # Test de normalité
bptest2 <- bptest(model2)                         # Test d'hétéroscédasticité
cat("Shapiro-Wilk p-value :", shapiro_test2$p.value, "\n")
cat("Breusch-Pagan p-value :", bptest2$p.value, "\n")


# Comparaison des modèles : AIC, BIC, et MSE
cat("\nComparaison des Modèles :\n")

# Calcul du MSE pour les deux modèles
mse_model1 <- mean((data$sp500.close - data$predicted_model1)^2, na.rm = TRUE)
mse_model2 <- mean((data$sp500.close - predict(model2))^2, na.rm = TRUE)

# Calcul des AIC et BIC
aic_comparison <- AIC(model1, model2)
bic_comparison <- BIC(model1, model2)

# Affichage des résultats
cat("\nModèle 1 (Matières premières) - MSE :", mse_model1, "\n")
cat("Modèle 2 (Macroéconomiques) - MSE :", mse_model2, "\n")
cat("Comparaison des AIC :\n")
print(aic_comparison)
cat("Comparaison des BIC :\n")
print(bic_comparison)

# Courbe ROC et AUC pour le modèle 3
roc_curve3 <- roc(data$volatility, predict(model3, type = "response"))
auc3 <- auc(roc_curve3)
plot(roc_curve3, main = "Courbe ROC - Modèle Logistique", col = "blue", lwd = 2)

# Matrice de confusion
threshold <- 0.5
data$pred_class <- ifelse(predict(model3, type = "response") > threshold, 1, 0)
confusion_matrix3 <- table(Predicted = data$pred_class, Actual = data$volatility)
print(confusion_matrix3)
# Extraction des valeurs de la matrice de confusion
TP <- confusion_matrix3[2, 2]  # Vrai Positifs
TN <- confusion_matrix3[1, 1]  # Vrai Négatifs
FP <- confusion_matrix3[2, 1]  # Faux Positifs
FN <- confusion_matrix3[1, 2]  # Faux Négatifs

# Calcul des métriques
precision <- TP / (TP + FP)  # Précision
sensibility <- TP / (TP + FN)  # Sensibilité (Recall)
specificity <- TN / (TN + FP)  # Spécificité
error_rate <- (FP + FN) / (TP + TN + FP + FN)  # Taux d'erreur

cat("\nMetrics pour le Modèle 3 :\n")
cat("Précision :", precision, "\n")
cat("Sensibilité (Recall) :", sensibility, "\n")
cat("Spécificité :", specificity, "\n")
cat("Taux d'erreur :", error_rate, "\n")
cat("AUC :", auc3, "\n")

