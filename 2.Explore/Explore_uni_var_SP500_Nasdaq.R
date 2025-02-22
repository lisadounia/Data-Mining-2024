# Installer et charger les packages nécessaires
# install.packages("readxl")   # Pour lire des fichiers Excel
# install.packages("pracma")   # Pour trouver les pics dans une série temporelle
# install.packages(c("forecast", "zoo", "ggplot2", "dplyr", "tseries"))  # Autres outils nécessaires
library(readr)
library(pracma)
library(forecast)
library(zoo)
library(ggplot2)
library(dplyr)
library(tseries)

# Charger les données
data <- read_csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/financial_regression.csv")

# Aperçu des données
summary(data)

# Convertir la colonne "date" en format Date si nécessaire
data$date <- as.Date(data$date, format = "%Y-%m-%d")

#### SP500 ###
# Visualisation de l'évolution du SP500 Close
plot(
  data$date, data$`sp500 close`,
  type = "l",
  col = "blue",
  xlab = "Date",
  ylab = "SP500 Close",
  main = "Évolution du SP500 Close"
)

# Test de normalité sur la variable "sp500 close"
shapiro_result <- shapiro.test(data$`sp500 close`)
print(shapiro_result)

# Histogramme du SP500 Close
ggplot(data, aes(x = `sp500 close`)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  labs(title = "Histogramme de SP500 Close", x = "SP500 Close", y = "Fréquence") +
  theme_minimal()

## Décomposition 
# Identifier et traiter les valeurs manquantes
na_rows <- which(is.na(data$`sp500 close`))
cat("Nombre de valeurs manquantes :", sum(is.na(data$`sp500 close`)), "\n")
data$`sp500 close` <- na.approx(data$`sp500 close`, na.rm = FALSE)
data$month <- format(data$date, "%Y-%m")
monthly_data <- data %>%
  group_by(month) %>%
  summarise(mean_close = mean(`sp500 close`, na.rm = TRUE))
monthly_ts <- ts(monthly_data$mean_close, start = c(2010, 1), frequency = 12)


# Décomposer la série temporelle 
decomp <- decompose(monthly_ts)

plot(decomp) 
stl_decomp <- stl(monthly_ts, s.window = "periodic")

# Calculer la contribution de la saisonnalité
var_seasonal <- var(stl_decomp$time.series[, "seasonal"])
var_total <- var(monthly_ts)
seasonal_contribution <- var_seasonal / var_total * 100
cat("Contribution de la saisonnalité :", seasonal_contribution, "%\n")



# Analyse des résidus (composante aléatoire)


shapiro.test(stl_decomp$time.series[, "remainder"])
acf(stl_decomp$time.series[, "remainder"], main = "Autocorrélation des résidus")


# Ajuster le modèle SARIMA
sarima_model <- auto.arima(monthly_ts, seasonal = TRUE)
summary(sarima_model)
sarima_residuals <- residuals(sarima_model)
residuals_df <- data.frame(
  Date = time(monthly_ts),                     # Dates des observations
  Residuals = sarima_residuals                # Résidus du modèle
)

residuals_df$Year <- as.numeric(format(as.Date(residuals_df$Date), "%Y"))

# Créer un box plot des résidus par année
ggplot(residuals_df, aes(x = factor(Year), y = Residuals)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Box plot des résidus du S&P 500 par année",
    x = "Année",
    y = "Résidus"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        









#### Analyse pour le Nasdaq ####


# Visualisation de l'évolution du NASDAQ Close
plot(
  data$date, data$`nasdaq close`,
  type = "l",
  col = "blue",
  xlab = "Date",
  ylab = "NASDAQ Close",
  main = "Évolution du NASDAQ Close"
)

# Test de normalité sur la variable "nasdaq close"
shapiro_result <- shapiro.test(data$`nasdaq close`)
print(shapiro_result)

# Histogramme du NASDAQ Close
ggplot(data, aes(x = `nasdaq close`)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogramme de NASDAQ Close", x = "NASDAQ Close", y = "Fréquence") +
  theme_minimal()

# Identifier et traiter les valeurs manquantes
na_rows <- which(is.na(data$`nasdaq close`))
cat("Nombre de valeurs manquantes :", sum(is.na(data$`nasdaq close`)), "\n")
data$`nasdaq close` <- na.approx(data$`nasdaq close`, na.rm = FALSE)

# Agrégation par mois
data$month <- format(data$date, "%Y-%m")
monthly_data <- data %>%
  group_by(month) %>%
  summarise(mean_close = mean(`nasdaq close`, na.rm = TRUE))

# Conversion en série temporelle
monthly_ts <- ts(monthly_data$mean_close, start = c(2010, 1), frequency = 12)

# Décomposer la série temporelle 
decomp <- decompose(monthly_ts)
plot(decomp)
# Décomposition STL
stl_decomp <- stl(monthly_ts, s.window = "periodic")

# Contribution de la saisonnalité
var_seasonal <- var(stl_decomp$time.series[, "seasonal"])
var_total <- var(monthly_ts)
seasonal_contribution <- var_seasonal / var_total * 100
cat("Contribution de la saisonnalité :", seasonal_contribution, "%\n")

# Analyse des résidus (composante aléatoire)
shapiro.test(stl_decomp$time.series[, "remainder"])
acf(stl_decomp$time.series[, "remainder"], main = "Autocorrélation des résidus")

# Ajuster le modèle SARIMA
sarima_model <- auto.arima(monthly_ts, seasonal = TRUE)
summary(sarima_model)

# Extraction des résidus du modèle SARIMA
sarima_residuals <- residuals(sarima_model)
residuals_df <- data.frame(
  Date = time(monthly_ts),                     
  Residuals = sarima_residuals               
)

# Extraire l'année des dates
residuals_df$Year <- as.numeric(format(as.Date(residuals_df$Date), "%Y"))

# Créer un box plot des résidus par année
ggplot(residuals_df, aes(x = factor(Year), y = Residuals)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Box plot des résidus du NASDAQ par année",
    x = "Année",
    y = "Résidus"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



