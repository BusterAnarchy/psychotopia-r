# Psychotopia-r

**psychotopia-r** est un pipeline d’analyse de données écrit en **R**, conçu pour explorer, filtrer et analyser des jeux de données liés à des échantillons de molécules.  

Il permet d’obtenir des statistiques globales, temporelles et géographiques, ainsi que des analyses de pureté et d’approvisionnement.


## 📁 Structure du projet

Le projet est organisé en plusieurs répertoires modulaires :

- **`analysis/`**  
  Contient les scripts d’analyse.  
  Chaque sous-répertoire correspond à un type d’analyse :  
  - `count/` pour les analyses de comptage  
  - `purity/` pour les analyses de pureté  
  - `supply/` pour les analyses liées à l’approvisionnement  
  - et un script `describe.R` pour décrire les colonnes du jeu de données  

- **`filters/`**  
  Contient les filtres appliqués aux données avant analyse (par date, molécule, famille, pureté, etc.).

- **`csv/`**  
  Fichiers de référence utilisés pour enrichir les analyses (par exemple, correspondance entre départements et régions).

- **`results/`**  
  Dossier de sortie où sont enregistrés les résultats des analyses, sous différents formats (`csv`, `json`, `rds`, etc.).

- **`cli.R`**  
  Script principal servant d’**interface en ligne de commande (CLI)**.  
  C’est le point d’entrée du pipeline : il charge les filtres, exécute les analyses choisies et exporte les résultats.


## 🚀 Installation

### Prérequis
- **R ≥ 4.1**
- Packages nécessaires :
  ```r
  install.packages(c(
    "argparse",
    "crayon",
    "DBI",
    "dplyr",
    "jsonlite",
    "lfe",
    "lubridate",
    "RMariaDB"
  ))
  ```

### Installation
Cloner le dépôt et se placer dans le dossier du projet :
```bash
git clone https://github.com/toncompte/psychotopia-r.git
cd psychotopia-r
```

Rendre le script exécutable (optionnel sous Linux/macOS) :
```bash
chmod +x cli.R
```

## 🧪 Exemples d’utilisation

### 1. Compter le nombre total d’échantillons
```bash
./cli.R count
```

### 2. Évolution de la pureté dans le temps entre deux dates
```bash
./cli.R --start 01/01/2024 --end 01/01/2025 temporal_purity --delta 15 --mode avg
```

### 3. Répartition du nombre d’échantillons par molécule pour deux famille donnée
```bash
./cli.R -mf "MDMA,Cocaïne" -f json histo_count
```

## 🤝 Contribuer

Vous souhaitez participer au projet **psychotopia-r** ?  
Consultez le guide de collaboration complet ici 👉 [Collaboration.md](./Collaboration.md)

Ce document explique comment :
- Ajouter un nouveau **filtre** (`filters/`)
- Ajouter une nouvelle **analyse** (`analysis/`)
- Les bonnes pratiques pour les **Pull Requests**
