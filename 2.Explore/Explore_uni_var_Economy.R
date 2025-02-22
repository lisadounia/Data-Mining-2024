# Installer et charger les packages nécessaires
# install.packages("readxl") # Décommentez si le package n'est pas installé
library(readr)
library(zoo)
library(ggplot2)


# Charger les données
data <- read_csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/financial_regression.csv")

summary(data)
# Convertir la colonne "date" en format Date si nécessaire
data$date <- as.Date(data$date, format = "%Y-%m-%d")

shapiro.test(data$GDP)
shapiro.test(data$`us_rates_%`)
shapiro.test(data$CPI)

data_clean <- na.omit(data)  # Supprime toutes les lignes avec au moins un NA
data$GDP <- na.approx(data$GDP, na.rm = FALSE)  # Interpolation linéaire
data$CPI <- na.approx(data$CPI, na.rm = FALSE)
data$`us_rates_%` <- na.approx(data$`us_rates_%`, na.rm = FALSE)

# Analyse de la tendance
plot(data$date, data$GDP, type = "l", main = "Évolution du PIB", xlab = "Date", ylab = "GDP")
plot(data$date, data$`us_rates_%`, type = "l", main = "Évolution du taux d'intérêt américains", xlab = "Date", ylab = "Taux (%)")
plot(data$date, data$CPI, type = "l", main = "Évolution de l'indice des prix à la consommation", xlab = "Date", ylab = "CPI")

# Créer une colonne "année" à partir de la colonne "date"
data$year <- format(data$date, "%Y")

# Boxplot pour le PIB (GDP) par année
p <- ggplot(data, aes(x = year, y = GDP)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.color = "red"
  ) +
  labs(
    title = "Boxplot du PIB par année",
    x = "Année",
    y = "PIB (GDP)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)

# Boxplot pour l'indice des prix à la consommation (CPI) par année
p <- ggplot(data, aes(x = year, y = CPI)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.color = "red"
  ) +
  labs(
    title = "Boxplot du CPI par année",
    x = "Année",
    y = "Indice des prix à la consommation (CPI)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)


# Boxplot pour les taux d'intérêt (us_rates_%) par année

p <- ggplot(data, aes(x = year, y = data$`us_rates_%`)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.color = "red"
  ) +
  labs(
    title = "Boxplot des taux d'intérêt par année",
    x = "Année",
    y = "Taux d'intérêt (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
