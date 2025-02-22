# Data-Mining : Analyse des Données Économiques et Prédictions des Dynamiques Boursières aux États-Unis

## Contexte et Objectif

Ce projet a été réalisé dans le cadre du cours de **Data Mining** à l'Université catholique de Louvain (UCLouvain). Son objectif principal est d'explorer et de prédire la fluctuations de l'indice boursier américain, le **S&P 500**, en tenant compte de plusieurs facteurs économiques et financiers.

Ce projet a pour but de répondre aux questions suivantes :
1. Comment les variations des prix des matières premières (or, pétrole, argent, platine, palladium) influencent-elles le **S&P 500** ?
2. Quel est l'impact des taux de change (USD/CHF, EUR/USD) sur les performances du  **S&P 500**  ?
3. Quels sont les indicateurs macroéconomiques (PIB, taux d'intérêt, inflation) qui affectent la performance du  **S&P 500** ?

## Méthodologie

Pour mener à bien cette analyse, la méthodologie **SEMMA** (Sample, Explore, Modify, Model, Assess) a été adoptée. Cette approche permet une analyse approfondie des données, tout en intégrant des modèles prédictifs afin de comprendre et anticiper les comportements des indices étudiés.

Les données utilisées proviennent d'une base publique disponible sur Kaggle, couvrant la période de 2010 à 2024, incluant des informations sur les indices boursiers, les matières premières, les taux de change et les indicateurs macroéconomiques américains. https://www.kaggle.com/datasets/franciscogcc/financial-data/data

## Résultats

### Impact des Matières Premières
Les matières premières ont des effets variés sur les indices boursiers :
- **Or, Pétrole et Palladium** : Ces matières premières influencent positivement le S&P 500, agissant comme valeurs refuges et étant liées à l’activité économique globale.
- **Platine** : A l'inverse, le platine a un effet négatif sur le  **S&P 500** , surtout en période d'incertitude économique, car il est davantage lié à des dynamiques industrielles.
- En termes de volatilité, l'augmentation du prix de l’or et du platine agit comme stabilisateurs, tandis que l'augmentation du prix du palladium peut amplifier l’instabilité du marché.

### Effet des Taux de Change
Bien que l'effet direct des taux de change sur les prix de clôture soit limité, ils jouent un rôle crucial dans les dynamiques de volatilité :
- **EUR/USD** : Ce taux est influencé par les politiques internationales et peut entraîner des incertitudes économiques.
- **USD/CHF** : Bien qu’il soit un actif refuge, il n’a pas eu un impact aussi significatif dans cette analyse.

### Influence des Indicateurs Macroéconomiques
Les principaux indicateurs macroéconomiques influencent fortement la performance des indices boursiers :
- **PIB** : Une croissance économique soutenue, mesurée par le PIB, favorise un marché dynamique et renforce la confiance des investisseurs.
- **Taux d'intérêt** : L’augmentation des taux d’intérêt tend à freiner les investissements, ralentissant ainsi la dynamique du marché.
- **Inflation** : Bien que l'inflation ait un impact indirect sur le marché via les politiques monétaires, son effet direct semble limité dans ce contexte.

### Modèles Prédictifs
Deux approches prédictives ont été particulièrement efficaces :
- **Régression linéaire** : Ce modèle, intégrant les variables macroéconomiques, a montré une grande précision pour prédire le prix de clôture actuel.
- **Boosting et Forêts Aléatoires** : Ces modèles ont excellé dans la prédiction du prix de clôture du lendemain.
- **Régression Logistique** : Bien qu’elle ait montré des résultats corrects, ce modèle a été plus performant pour prédire la volatilité.

### Clustering et Régimes de Marché
L’utilisation du **clustering** a permis de distinguer deux régimes de marché :
- **Avant la COVID-19** : Une période marquée par une instabilité accrue et des volumes de transactions plus élevés.
- **Après la COVID-19** : Un marché plus stable, avec des volumes réduits et des performances régulières, illustrant l'impact des grands événements économiques sur les dynamiques financières.

## Conclusion

Cette étude démontre que comprendre les variations du S&P 500 nécessite une approche globale prenant en compte les interactions complexes entre les matières premières, les indicateurs économiques et les taux de change. Le projet fournit des insights précieux sur la manière dont ces facteurs externes influencent les marchés financiers et met en lumière les modèles les plus efficaces pour prédire leurs comportements futurs.

