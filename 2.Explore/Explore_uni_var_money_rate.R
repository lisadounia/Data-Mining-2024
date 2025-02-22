
# Installer et charger les bibliothèques nécessaires
# install.packages("readxl") # À décommenter si nécessaire
library(readr)
library(ggplot2)
library(strucchange)

# Charger les données
data <- read_csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/financial_regression.csv")
# Conversion de la colonne date en format Date
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Extraire l'année à partir de la colonne date
data$year <- format(data$date, "%Y")

#### Analyse pour USD/CHF####
#boxplot
p <- ggplot(data, aes(x = year, y = usd_chf)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.color = "red"
  ) +
  labs(
    title = "Boxplot Annuel pour USD/CHF",
    x = "Année",
    y = "USD/CHF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)

## Test de normalité
shapiro.test(data$usd_chf)
data$usd_chf_sqrt <- sqrt(data$usd_chf)
shapiro.test(data$usd_chf_sqrt)

## Trouver les valeurs extrêmes
min_value_usd_chf <- min(data$usd_chf, na.rm = TRUE)
max_value_usd_chf <- max(data$usd_chf, na.rm = TRUE)
min_date_usd_chf <- data$date[which.min(data$usd_chf)]
max_date_usd_chf <- data$date[which.max(data$usd_chf)]

## Graphique avec annotations
ggplot(data, aes(x = date, y = usd_chf)) +
  geom_line(color = "blue", size = 1) +
  geom_point(aes(x = min_date_usd_chf, y = min_value_usd_chf), color = "red", size = 3) +
  geom_point(aes(x = max_date_usd_chf, y = max_value_usd_chf), color = "green", size = 3) +
  annotate("text", x = min_date_usd_chf, y = min_value_usd_chf, label = paste("Min:", round(min_value_usd_chf, 4)), hjust = -0.1, color = "red") +
  annotate("text", x = max_date_usd_chf, y = max_value_usd_chf, label = paste("Max:", round(max_value_usd_chf, 4)), hjust = -0.1, color = "green") +
  labs(title = "USD/CHF : Points extrêmes annotés", x = "Date", y = "USD/CHF") +
  theme_minimal()

## Analyse des ruptures de série temporelle
breakpoints_model_usd_chf <- breakpoints(usd_chf ~ 1, data = data)
plot(breakpoints_model_usd_chf)
lines(breakpoints_model_usd_chf)
summary(breakpoints_model_usd_chf)
break_indices <- breakpoints_model_usd_chf$breakpoints
optimal_dates <- data$date[break_indices]
print(optimal_dates)


#### Analyse pour EUR/USD ####
#boxplot
p <- ggplot(data, aes(x = year, y = eur_usd)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.color = "red"
  ) +
  labs(
    title = "Boxplot Annuel pour EUR/USD",
    x = "Année",
    y = "EUR/USD"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)

## Test de normalité
shapiro.test(data$eur_usd)
data$eur_usd_sqrt <- sqrt(data$eur_usd)
shapiro.test(data$eur_usd)

## Trouver les valeurs extrêmes
min_value_eur_usd <- min(data$eur_usd, na.rm = TRUE)
max_value_eur_usd <- max(data$eur_usd, na.rm = TRUE)
min_date_eur_usd <- data$date[which.min(data$eur_usd)]
max_date_eur_usd <- data$date[which.max(data$eur_usd)]

## Graphique avec annotations
ggplot(data, aes(x = date, y = eur_usd)) +
  geom_line(color = "blue", size = 1) +
  geom_point(aes(x = min_date_eur_usd, y = min_value_eur_usd), color = "red", size = 3) +
  geom_point(aes(x = max_date_eur_usd, y = max_value_eur_usd), color = "green", size = 3) +
  annotate("text", x = min_date_eur_usd, y = min_value_eur_usd, label = paste("Min:", round(min_value_eur_usd, 4)), hjust = -0.1, color = "red") +
  annotate("text", x = max_date_eur_usd, y = max_value_eur_usd, label = paste("Max:", round(max_value_eur_usd, 4)), hjust = -0.1, color = "green") +
  labs(title = "EUR/USD : Points extrêmes annotés", x = "Date", y = "EUR/USD") +
  theme_minimal()


## Analyse des ruptures de série temporelle
breakpoints_model_eur_usd <- breakpoints(eur_usd ~ 1, data = data)
plot(breakpoints_model_eur_usd)
lines(breakpoints_model_eur_usd)
summary(breakpoints_model_eur_usd)
break_indices <- breakpoints_model_eur_usd$breakpoints
optimal_dates <- data$date[break_indices]
print(optimal_dates)
