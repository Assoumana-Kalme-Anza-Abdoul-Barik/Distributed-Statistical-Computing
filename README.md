# High-Performance & Distributed Computing with R 🚀

Ce dépôt regroupe une série de travaux avancés sur le traitement de données massives (**Big Data**) et l'optimisation de calculs statistiques via des architectures distribuées.

---

## 📊 Projet 1 : Optimisation de Clustering (K-Means) en Environnement Parallèle
*Focus : Architecture technique et granularité des tâches*

Mise en œuvre d'architectures de calcul parallèle pour optimiser l'algorithme des K-Means sur des données hospitalières.

### 🛠️ Stack Technique
* **Environnement :** Serveur Linux, SSH, Git, Tmux (persistance des sessions).
* **Parallélisation :** Packages `parallel` (héritages `multicore` & `snow`), `PSOCK` clusters.
* **Profiling :** `Rprof` et `microbenchmark`.

### 💡 Points Clés
* **Prétraitement Statistique :** Stabilisation de la variance via transformation logarithmique, faisant passer le rapport de corrélation $\eta^{2}$ de **0.38** à **0.67**.
* **Optimisation de la Granularité :** Démonstration de l'impact du ratio calcul/communication. La stratégie $8 \times 512$ s'est révélée **33 fois plus rapide** que l'approche unitaire.
* **Performance :** Accélération d'un facteur **x8** par rapport au mode séquentiel.

---

## 🏗️ Projet 2 : Pipeline Big Data & Statistiques Distribuées (17M+ lignes)
*Focus : Lecture morcelée et calcul incrémental (Base OpenDamir/SNIRAM)*

Conception d'une chaîne complète de traitement statistique sans chargement intégral en mémoire vive pour plus de **17 081 967 enregistrements**.

### 🛠️ Stack Technique
* **Traitement :** Lecture morcelée (Chunk-based processing), gestion de fichiers compressés.
* **Architecture :** Cluster réparti sur 4 machines physiques (16 instances R).
* **Algorithmique :** Formules incrémentales pour moyennes, variances, covariances et quantiles par échantillonnage (1%).

### 💡 Points Clés
* **Stratégie de Lecture :** Analyse de la complexité temporelle du format `gzip`. Le passage au format splité a généré un gain de performance de **x10**.
* **Loi d'Amdahl :** Modélisation de l'accélération théorique vs observée.
* **Résultats Cluster :**
    | Configuration | Temps Total | Accélération |
    | :--- | :--- | :--- |
    | Séquentielle | 155.3 s | Réf. |
    | Cluster Local (4 cœurs) | 43.9 s | **x3.5** |
    | Cluster Réparti (16 cœurs) | 20.3 s | **x7.6** |

---

## 🧠 Compétences Validées
* ✅ Gestion de ressources sur **Cluster de calcul**.
* ✅ Implémentation d'algorithmes **"Embarrassingly Parallel"**.
* ✅ Analyse de la complexité algorithmique et des goulots d'étranglement (I/O, Latence réseau).
* ✅ Maîtrise de l'environnement de production (SSH, Tmux, Git).
