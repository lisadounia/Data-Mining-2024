import pandas as pd
from sklearn.model_selection import train_test_split

# Charger le fichier CSV
file_path = "/Users/lisadounia/projets_masterQ1/Data-Mining/3.Modify/discretized_data.xlsx"
data = pd.read_excel(file_path)


# Aperçu des données pour vérifier le chargement
data_info = {
    "head": data.head(),
    "info": data.describe(),  # Replacing with data.describe() to get a summary
    "shape": data.shape
}

data.info()  # Call data.info() separately to print the information


# Diviser les données en ensembles d'apprentissage et de validation (80%-20%)
train_data, validation_data = train_test_split(data, test_size=0.2, random_state=42)

# Sauvegarder les ensembles dans des fichiers 
train_file_path = '/Users/lisadounia/projets_masterQ1/Data-Mining/3.Modify/train_data.xlsx'
validation_file_path = '/Users/lisadounia/projets_masterQ1/Data-Mining/3.Modify/validation_data.xlsx'

train_data.to_excel(train_file_path, index=False) # ou to_parquet pour un fichier Parquet
validation_data.to_excel(validation_file_path, index=False)

