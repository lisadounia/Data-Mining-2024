# Installer et charger les bibliothèques nécessaires
# install.packages("readxl") # À décommenter si nécessaire
# install.packages("GGally") # Pour les visualisations avancées
library(readr)
library(GGally)
library(dplyr)

# Charger les données
data <- read_csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/financial_regression.csv")


data$date <- as.Date(data$date)  
data$month <- format(data$date, "%Y-%m") 

monthly_data <- data %>%
  group_by(month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
monthly_ts <- ts(
  monthly_data$`sp500 close`,
  start = c(as.numeric(substr(monthly_data$month[1], 1, 4)), 
            as.numeric(substr(monthly_data$month[1], 6, 7))),
  frequency = 12
)

# Sélection de toutes les variables numériques
numeric_vars <- data[, sapply(data, is.numeric)]

# Calcul des matrices de corrélation pour toutes les variables numériques
## Corrélations de Pearson
cor_pearson <- cor(numeric_vars, use = "complete.obs", method = "pearson")

## Corrélations de Spearman
cor_spearman <- cor(numeric_vars, use = "complete.obs", method = "spearman")

# Afficher les matrices de corrélation
cat("Corrélations de Pearson :\n")
print(cor_pearson)

cat("\nCorrélations de Spearman :\n")
print(cor_spearman)

# Visualisation : Pair plot des relations principales
# Si trop de variables, sélectionnez un sous-ensemble pertinent
subset_vars <- numeric_vars %>%
  select(
    `sp500 close`, `nasdaq close`, `us_rates_%`, `CPI`, `GDP`,
    `usd_chf`, `eur_usd`, `silver close`, `oil close`, 
    `platinum close`, `palladium close`, `gold close`
  )

ggpairs(
  subset_vars,
  title = "Nuages de points des relations principales",
  aes(alpha = 0.5)
) +
  theme_minimal()
