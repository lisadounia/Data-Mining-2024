library(readxl)
library(dplyr)
library(writexl)

# Charger les données
data <- read.csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/3.standardized_data.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Étape 1 : Créer et standardiser la variable high-low
data <- data %>%
  mutate(high_low = `sp500.high` - `sp500.low`)
data <- data %>%
  mutate(high_low_standardized = (high_low - mean(high_low, na.rm = TRUE)) / sd(high_low, na.rm = TRUE))



# Fonction pour discrétiser en quantiles (général)
discretize_quantiles <- function(variable) {
  quantiles <- quantile(variable, probs = c(0.25, 0.75), na.rm = TRUE) # 1er et 3e quartiles
  cut(variable, breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
      labels = c("Faible", "Moyen", "Élevé"), include.lowest = TRUE)
}



# Fonction pour discrétiser les volumes
discretize_volume <- function(variable) {
  mean_value <- mean(variable, na.rm = TRUE)
  cut(variable, breaks = c(-Inf, 0.8 * mean_value, 1.2 * mean_value, Inf),
      labels = c("Faible", "Moyen", "Élevé"), include.lowest = TRUE)
}

# Fonction pour discrétiser les taux de change
discretize_exchange_rate <- function(variable, thresholds) {
  cut(variable, breaks = c(-Inf, thresholds[1], thresholds[2], Inf),
      labels = c("Faible", "Moyen", "Élevé"), include.lowest = TRUE)
}
discretize_sp500_rsi <- function(variable, thresholds) {
  cut(variable, breaks = c(-Inf, thresholds[1], thresholds[2], Inf),
      labels = c("Survente", "Neutre", "Surachat"), include.lowest = TRUE)
}


# Application de la discrétisation
discretized_data <- data %>%
  mutate(
    # Indices boursiers
    sp500_open_cat = discretize_quantiles(`sp500.open`),
    sp500_close_cat = discretize_quantiles(`sp500.close`),
    sp500_high_low_cat = discretize_quantiles(`sp500.high.low`),
    sp500_volume_cat = discretize_volume(`sp500.volume`),
    nasdaq_open_cat = discretize_quantiles(`nasdaq.open`),
    nasdaq_close_cat = discretize_quantiles(`nasdaq.close`),
    nasdaq_high_low_cat = discretize_quantiles(`nasdaq.high.low`),
    nasdaq_volume_cat = discretize_volume(`nasdaq.volume`),
    
    
    # Macroéconomiques
    us_rates_cat = discretize_quantiles(`us_rates_.`),
    CPI_cat = discretize_quantiles(`CPI`),
    GDP_cat = discretize_quantiles(`GDP`),
    
    # Taux de change
    usd_chf_cat = discretize_exchange_rate(`usd_chf`, c(0.90, 1.10)),
    eur_usd_cat = discretize_exchange_rate(`eur_usd`, c(1.10, 1.30)),
    
    sp500_rsi_cat=discretize_sp500_rsi(sp500_rsi, c(0.30, 0,70)),
    
    # Matières premières (prix et volumes)
    silver_high_low_cat = discretize_quantiles(`silver.high.low`),
    silver_volume_cat = discretize_volume(`silver.volume`),
    oil_high_low_cat = discretize_quantiles(`oil.high.low`),
    oil_volume_cat = discretize_volume(`oil.volume`),
    platinum_high_low_cat = discretize_quantiles(`platinum.high.low`),
    platinum_volume_cat = discretize_volume(`platinum.volume`),
    palladium_high_low_cat = discretize_quantiles(`palladium.high.low`),
    palladium_volume_cat = discretize_volume(`palladium.volume`),
    #création de gold high low 
    gold.high.low = gold.high - gold.low,  
    gold.high.low_cat = discretize_quantiles(gold.high.low),
    gold_volume_cat = discretize_volume(`gold.volume`),
    

    
    # Retours
    sp500_return_cat = discretize_quantiles(`sp500_return`),
    nasdaq_return_cat = discretize_quantiles(`nasdaq_return`),
    silver_return_cat = discretize_quantiles(`silver_return`),
    oil_return_cat = discretize_quantiles(`oil_return`),
    platinum_return_cat = discretize_quantiles(`platinum_return`),
    palladium_return_cat = discretize_quantiles(`palladium_return`),
    gold_return_cat = discretize_quantiles(`gold_return`),
    
    # Moyennes mobiles et RSI
    sp500_rsi_cat = discretize_quantiles(`sp500_rsi`),
    sp500_macd_cat = discretize_quantiles(`sp500_macd`),
    sp500_macd_signal_cat = discretize_quantiles(`sp500_macd_signal`),
    sp500_MA7_cat = discretize_quantiles(sp500_MA7),
    sp500_MA7_diff_cat = discretize_quantiles(sp500_MA7_diff),
    sp500_MA30_cat = discretize_quantiles(sp500_MA30),
    sp500_MA30_diff_cat = discretize_quantiles(sp500_MA30_diff),
    sp500_MA50_cat = discretize_quantiles(sp500_MA50),
    sp500_MA50_diff_cat = discretize_quantiles(sp500_MA50_diff),
    sp500_MA100_cat = discretize_quantiles(sp500_MA100),
    sp500_MA100_diff_cat = discretize_quantiles(sp500_MA100_diff),
    nasdaq_MA7_cat = discretize_quantiles(nasdaq_MA7),
    nasdaq_MA7_diff_cat = discretize_quantiles(nasdaq_MA7_diff),
    nasdaq_MA30_cat = discretize_quantiles(nasdaq_MA30),
    nasdaq_MA30_diff_cat = discretize_quantiles(nasdaq_MA30_diff),
    nasdaq_MA50_cat = discretize_quantiles(nasdaq_MA50),
    nasdaq_MA50_diff_cat = discretize_quantiles(nasdaq_MA50_diff),
    nasdaq_MA100_cat = discretize_quantiles(nasdaq_MA100),
    nasdaq_MA100_diff_cat = discretize_quantiles(nasdaq_MA100_diff),
    
    # Autres ratios et métriques
    sp500_volatility_cat = discretize_quantiles(sp500_volatility),
    normalized_price_range_cat = discretize_quantiles(normalized_price_range),
    
    
    # Ratios normalisés
    sp500_nasdaq_ratio_cat = discretize_quantiles(`sp500_nasdaq_ratio`),
    sp500_silver_ratio_cat = discretize_quantiles(`sp500_silver_ratio`),
    sp500_oil_ratio_cat = discretize_quantiles(`sp500_oil_ratio`),
    sp500_platinum_ratio_cat = discretize_quantiles(`sp500_platinum_ratio`),
    sp500_palladium_ratio_cat = discretize_quantiles(`sp500_palladium_ratio`),
    sp500_gold_ratio_cat = discretize_quantiles(`sp500_gold_ratio`),
    sp500_us_rates_ratio_cat = discretize_quantiles(`sp500_us_rates_._ratio`),
    sp500_CPI_ratio_cat = discretize_quantiles(`sp500_CPI_ratio`),
    sp500_GDP_ratio_cat = discretize_quantiles(`sp500_GDP_ratio`),
    sp500_usd_chf_ratio_cat = discretize_quantiles(`sp500_usd_chf_ratio`),
    sp500_eur_usd_ratio_cat = discretize_quantiles(`sp500_eur_usd_ratio`)
  )

# Filtrer uniquement les colonnes catégorisées
discretized_columns <- names(discretized_data)[grepl("_cat$", names(discretized_data))]

# Ajouter la colonne date pour la conserver
final_data <- discretized_data %>%
  select(date, all_of(discretized_columns))

# Exporter les données discrétisées
write.csv(final_data, "/Users/lisadounia/projets_masterQ1/Data-Mining/data/discretized_full_data.csv")

# Vérification des nouvelles colonnes
summary(final_data)
