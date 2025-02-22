# Installer et charger les bibliothèques nécessaires
# install.packages("readxl") # À décommenter si nécessaire
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)

# Charger les données
data <- read_csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/financial_regression.csv")

# Aperçu des données
summary(data)

# Conversion de la date en format Date
data$date <- as.Date(data$date, format = "%Y-%m-%d")


#### Analyse annuel des variables de volatilité ####

# Ajouter une colonne pour l'année
data$year <- format(as.Date(data$date), "%Y")

# Variables à analyser
volatility_vars <- c("oil high-low", "palladium high-low", "platinum high-low", "silver high-low")

# Boucle pour créer un boxplot annuel pour chaque variable
for (var in volatility_vars) {
  # Créer le boxplot
  p <- ggplot(data, aes(x = year, y = .data[[var]])) +
    geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
    labs(
      title = paste("Boxplot Annuel pour", var),
      x = "Année",
      y = var
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Afficher le graphique
  print(p) }

#### evolution prix cloture 
data_long_close <- melt(data, 
                        id.vars = "date", 
                        measure.vars = c("silver close", 
                                         "oil close", 
                                         "palladium close", 
                                         "platinum close", 
                                         "gold close"),
                        variable.name = "Produit",
                        value.name = "Prix")

# Renommer les produits pour qu'ils soient plus lisibles
data_long_close$Produit <- gsub("_close", "", data_long_close$Produit)

# Créer le graphique pour les prix de clôture uniquement
library(ggplot2)
ggplot(data_long_close, aes(x = date, y = Prix, color = Produit)) +
  geom_line(size = 1) +
  labs(title = "Évolution des prix de clôture des ressources naturelles",
       x = "Date",
       y = "Prix",
       color = "Produit") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


#### evolution des volumes
data_long_volume <- melt(data, 
                         id.vars = "date", 
                         measure.vars = c("silver volume", 
                                          "oil volume", 
                                          "palladium volume", 
                                          "platinum volume", 
                                          "gold volume"),
                         variable.name = "Produit",
                         value.name = "Volume")

# Renommer les produits pour qu'ils soient plus lisibles
data_long_volume$Produit <- gsub("_volume", "", data_long_volume$Produit)


library(ggplot2)
ggplot(data_long_volume, aes(x = date, y = Volume, color = Produit)) +
  geom_line(size = 1) +
  labs(title = "Évolution des volumes de transactions des ressources naturelles",
       x = "Date",
       y = "Volume",
       color = "Produit") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
