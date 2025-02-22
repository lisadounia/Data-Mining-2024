library(readxl)
library(readr)
library(dplyr)
library(zoo)
#install.packages("lubridate")
library(lubridate)
library(zoo)
#install.packages("writexl")
library(writexl)




# Charger les données
data <- read_csv("/Users/lisadounia/projets_masterQ1/Data-Mining/data/financial_regression.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")

#si rien à part la date => jour férié
cleaned_data <-data %>% 
  filter(rowSums(!is.na(select(., -date))) > 0)
summary(cleaned_data)


#suppression Good Friday et Ourangan Sandy
# Créer un vecteur contenant les dates à supprimer
dates_to_remove <- as.Date(c(
  "2010-04-02", "2011-04-22", "2012-04-06", "2012-10-29", "2012-10-30",
  "2013-03-29", "2014-04-18", "2015-04-03", "2016-03-25", "2017-04-14",
  "2018-03-30", "2019-04-19", "2020-04-10", "2021-04-02", "2022-04-15",
  "2023-04-07", "2024-03-29"
))

# Filtrer la base de données pour exclure les lignes correspondant aux dates
cleaned_data <- cleaned_data %>%
  filter(!date %in% dates_to_remove)


summary(cleaned_data)




# Remplir les valeurs manquantes des variables macroéconomiques 


# Identifier le premier jour de chaque mois et propager la valeur du mois suivant
monthly_values <- cleaned_data %>%
  mutate(first_day_of_month = floor_date(date, "month")) %>%  # Identifier le 1er jour du mois
  group_by(first_day_of_month) %>%
  summarise(
    CPI_next_month = first(na.omit(CPI)),  # Valeur CPI du 1er jour du mois
    us_rates_next_month = first(na.omit(`us_rates_%`))
  ) %>%
  mutate(first_day_of_month = first_day_of_month - months(1))  # Décaler vers le mois précédent

# Joindre ces valeurs au dataset original
cleaned_data <- cleaned_data %>%
  mutate(first_day_of_month = floor_date(date, "month")) %>%
  left_join(monthly_values, by = c("first_day_of_month")) %>%
  mutate(
    CPI = ifelse(is.na(CPI), CPI_next_month, CPI),
    `us_rates_%` = ifelse(is.na(`us_rates_%`), us_rates_next_month, `us_rates_%`)
  ) %>%
  select(-CPI_next_month, -us_rates_next_month, -first_day_of_month)  # Nettoyer les colonnes temporaires

# Vérifier le résultat
summary(cleaned_data)






# Identifier le premier jour de chaque trimestre et propager la valeur du trimestre suivant
quarterly_values <- cleaned_data %>%
  mutate(first_day_of_quarter = floor_date(date, "quarter")) %>%  # Identifier le 1er jour du trimestre
  group_by(first_day_of_quarter) %>%
  summarise(
    GDP_next_quarter = first(na.omit(GDP))  # Valeur GDP du 1er jour du trimestre
  ) %>%
  mutate(first_day_of_quarter = first_day_of_quarter - months(3))  # Décaler vers le trimestre précédent

# Joindre ces valeurs au dataset original
cleaned_data <- cleaned_data %>%
  mutate(first_day_of_quarter = floor_date(date, "quarter")) %>%
  left_join(quarterly_values, by = c("first_day_of_quarter")) %>%
  mutate(
    GDP = ifelse(is.na(GDP), GDP_next_quarter, GDP)
  ) %>%
  select(-GDP_next_quarter, -first_day_of_quarter)  # Nettoyer les colonnes temporaires

# Vérifier le résultat
summary(cleaned_data)

#on enleve

cleaned_data <- cleaned_data %>%
  filter(!(rowSums(!is.na(select(., -date, -CPI, -`us_rates_%`, -GDP))) == 0))  # Garder les lignes où d'autres colonnes ont des valeurs



# Vérifier le résultat
summary(cleaned_data)

#taux de change manquant

# Appliquer la méthode LOCF pour les colonnes des taux de change
cleaned_data <- cleaned_data %>%
  mutate(
    usd_chf = na.locf(usd_chf, na.rm = FALSE),  # Remplir les valeurs manquantes avec LOCF
    eur_usd = na.locf(eur_usd, na.rm = FALSE)   # Même méthode pour EUR/USD
  )

# Vérifier les résultats
summary(cleaned_data)

###gestion des données manquante en fin 2024 
# 1. Supprimer les lignes correspondant au mois d'octobre 2024
cleaned_data <- cleaned_data %>%
  filter(!(year(date) == 2024 & month(date) == 10))

# 2. Remplir les valeurs pour GDP
# Deuxième trimestre (Avril à Juin 2024) : 29 020
# Troisième trimestre (Juillet à Septembre 2024) : 29 350
cleaned_data <- cleaned_data %>%
  mutate(
    GDP = case_when(
      date >= as.Date("2024-04-01") & date <= as.Date("2024-06-30") ~ 29020,  # T2 2024
      date >= as.Date("2024-07-01") & date <= as.Date("2024-09-30") ~ 29350,  # T3 2024
      TRUE ~ GDP  # Conserver les autres valeurs existantes
    )
  )

# 3. Remplir les valeurs pour us_rates_% et CPI en septembre 2024
cleaned_data <- cleaned_data %>%
  mutate(
    `us_rates_%` = case_when(
      date >= as.Date("2024-09-01") & date <= as.Date("2024-09-30") ~ 5,  # Septembre 2024 : 5%
      TRUE ~ `us_rates_%`
    ),
    CPI = case_when(
      date >= as.Date("2024-09-01") & date <= as.Date("2024-09-30") ~ 315.664,  # CPI pour septembre 2024
      TRUE ~ CPI
    )
  )



# Vérifier les résultats
summary(cleaned_data)
write.csv(cleaned_data, "/Users/lisadounia/projets_masterQ1/Data-Mining/3.Modify/cleaned_data_lisa.csv")
