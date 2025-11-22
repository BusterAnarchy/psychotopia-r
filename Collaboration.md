## 🤝 Collaboration

Le projet **psychotopia-r** est conçu pour être **ouvert et extensible**.

Nous encourageons les contributions sous forme de **Pull Requests** afin d’ajouter de nouvelles analyses ou de nouveaux filtres, sans modifier le reste du code. Chaque ajout correspond simplement à **un nouveau fichier placé dans le bon répertoire**.

Les modérateurs du dépôt effectueront une **revue du code** avant toute fusion de Pull Request pour garantir la qualité et la cohérence du projet.

---

### 🧩 Ajouter un nouveau filtre

Les filtres sont situés dans le dossier [`filters/`](filters/).  
Chaque filtre est un fichier `.R` qui contient deux éléments :

1. Une **description** du filtre sous forme de liste (`filter_description`)
2. Une **fonction** qui applique le filtre (`filter_function`)

#### Exemple minimal

```r
filter_description <- list(
  name = "Filtre temporel",
  args = list(
    start = list(required = TRUE, help = "Date de début (JJ/MM/AAAA)"),
    end   = list(required = TRUE, help = "Date de fin (JJ/MM/AAAA)")
  ),
  help = "Filtre les lignes d’un data.frame entre deux dates"
)

filter_function <- function(data, args) {
  start <- as.Date(args$start, format="%d/%m/%Y")
  end   <- as.Date(args$end, format="%d/%m/%Y")
  data %>% filter(date >= start, date <= end)
}
```

#### Étapes pour contribuer :
1. Créer un nouveau fichier dans `filters/` (ex: `filter_region.R`)
2. Définir :
   - `filter_description` (nom, aide, arguments éventuels)
   - `filter_function(data, args)` (code de calcul)
3. Tester le filtre localement avec une commande du type :
   ```bash
   ./cli.R --start 01/01/2022 --end 31/12/2022 count
   ```
4. Ouvrir une **Pull Request** sur le dépôt GitHub

Le script `cli.R` détecte automatiquement tous les fichiers `filter_*.R` présents dans `filters/`, **aucune modification supplémentaire n’est nécessaire.**

---

### 📊 Ajouter une nouvelle analyse

Les analyses sont stockées dans le dossier [`analysis/`](analysis/).

Chaque analyse est un fichier `.R` contenant deux objets :

1. Une **description** de l’analyse (`analysis_description`)
2. Une **fonction** qui exécute le calcul (`analysis_function`)

#### Exemple minimal

```r
analysis_description <- list(
  name = "count",
  help = "Renvoie le nombre d'échantillions",
  args = list()
)

analysis_function <- function(data, args) {
  nrow(data)
}
```

#### Étapes pour contribuer :
1. Créer un nouveau fichier dans le dossier approprié (ex : `analysis/purity/new_purity_metric.R`)
2. Définir :
   - `analysis_description` (nom, aide, arguments éventuels)
   - `analysis_function(data, args)` (code de calcul)
3. Tester la commande :
   ```bash
   ./cli.R new_purity_metric
   ```
4. Ouvrir une **Pull Request**

Le script `cli.R` toutes les analyses disponibles dans le dossier et les sous-dossiers de `analysis/`, **aucune modification supplémentaire n’est nécessaire.**

---

### ✅ Bonnes pratiques

- Utiliser des noms clairs et explicites pour les fichiers (`filter_`, `analysis_`).
- Ajouter des commentaires concis dans le code.
- Respecter la structure standard (`*_description`, `*_function`).
- Tester les nouvelles fonctionnalités avant de proposer une Pull Request.
- Ne pas modifier les fichiers existants sans justification.

---

En suivant cette approche, tout.e contributeur·rice peut enrichir **psychotopia-r** simplement en ajoutant un fichier bien structuré.  

Cela garantit la modularité, la maintenabilité et la robustesse du pipeline d’analyse.
