# ==============================================================================
# CHARGEMENT DES DONNÉES
# ==============================================================================

read.csv("hop.csv", stringsAsFactors = TRUE) -> hop

# ==============================================================================
# EXPLORATION DES DONNÉES
# ==============================================================================

summary(hop)
dim(hop)

# ==============================================================================
# CALCUL DE LA SOMME PAR LIGNE
# ==============================================================================
# Calcule la somme horizontale (par ligne) et l'assigne à 'NbAct'
hop$NbAct <- rowSums(hop[sapply(hop, is.numeric)], na.rm = TRUE)

# ==============================================================================
# VERIFICATION
# ==============================================================================

head(hop)

# ==============================================================================
# CALCUL DU RAPPORT DE CORRÉLATION (ETA-SQUARE)
# ==============================================================================
# Calcul de l'ANOVA pour obtenir les sommes des carrés
fit_anova <- aov(NbAct ~ TYPE, data = hop)
sum_sq <- summary(fit_anova)[[1]]["Sum Sq"]

# Calcul de l'Eta-squared (SS_between / SS_total)
eta2 <- sum_sq[1, 1] / sum(sum_sq)

# ==============================================================================
# EXPORTATION DU GRAPHIQUE AU FORMAT PDF
# ==============================================================================
# Ouverture du périphérique graphique PDF
pdf("gr01_bp.pdf")

# Génération du graphique avec le rapport de corrélation
boxplot(NbAct ~ TYPE, 
        data = hop, 
        main = paste("Boxplots de NbAct par TYPE (Eta² =", round(eta2, 4), ")"),
        xlab = "TYPE", 
        ylab = "NbAct", 
        col = "steelblue")

# Fermeture du périphérique (sauvegarde du fichier)
dev.off()

# ==============================================================================
# TRANSFERT VERS LE RÉPERTOIRE SITES
# ==============================================================================
# Copie du fichier vers le dossier personnel public
file.copy("gr01_bp.pdf", "~/Sites/bp1.pdf", overwrite = TRUE)

# ==============================================================================
# ACCÈS VIA NAVIGATEUR
# ==============================================================================
# L'URL d'accès sera : https://pages.isfa.fr/~maiXYZ/bp1.pdf

# ==============================================================================
# CALCUL DU LOG10 ET DU RAPPORT DE CORRÉLATION
# ==============================================================================
# Création de la variable log10(NbAct)
# On ajoute 1 pour éviter log10(0) si nécessaire
log_nbact <- log10(hop$NbAct + 1)

# Calcul de l'ANOVA pour l'Eta-squared sur le log
fit_anova_log <- aov(log_nbact ~ hop$TYPE)
sum_sq_log <- summary(fit_anova_log)[[1]]["Sum Sq"]
eta2_log <- sum_sq_log[1, 1] / sum(sum_sq_log)

# ==============================================================================
# GÉNÉRATION ET EXPORTATION DU GRAPHIQUE PDF (gr02_bp.pdf)
# ==============================================================================
pdf("gr02_bp.pdf")

boxplot(log_nbact ~ hop$TYPE, 
        main = paste("Boxplots de log10(NbAct) par TYPE (Eta² =", round(eta2_log, 4), ")"),
        xlab = "TYPE", 
        ylab = "log10(NbAct)", 
        col = "lightgreen")

dev.off()

# ==============================================================================
# TRANSFERT VERS LE RÉPERTOIRE SITES
# ==============================================================================
# Copie vers ~/Sites avec le nom correspondant à l'URL cible
file.copy("gr02_bp.pdf", "~/Sites/bp2.pdf", overwrite = TRUE)

# ==============================================================================
# ACCÈS VIA NAVIGATEUR
# ==============================================================================
# L'URL d'accès sera : https://pages.isfa.fr/~maiXYZ/bp2.pdf

# ==============================================================================
# PARALLÉLISATION : K-MEANS SÉQUENTIEL (BASELINE)
# ==============================================================================
# Sélection des données numériques uniquement
# On exclut TYPE (facteur) et NbAct (calculé précédemment)
dat_km <- hop[sapply(hop, is.numeric)]
dat_km$NbAct <- NULL 

# Définition des paramètres
nb_essais <- 4096
nb_clusters <- 3

# Mesure du temps d'exécution
cat("Lancement de kmeans (4096 essais) en cours...\n")
temps_seq <- system.time({
  res_km <- kmeans(dat_km, centers = nb_clusters, nstart = nb_essais)
})

# Affichage du résultat
print(temps_seq)

# ==============================================================================
# INSTALLATION ET CHARGEMENT DES BIBLIOTHÈQUES
# ==============================================================================
if(!require(microbenchmark)) install.packages("microbenchmark", repos="https://cloud.r-project.org")
library(microbenchmark)

# ==============================================================================
# MESURE DE PERFORMANCE (MICROBENCHMARK : 10 ESSAIS)
# ==============================================================================
cat("Lancement du microbenchmark (10 essais par méthode)...\n")

res_benchmark <- microbenchmark(
  # Méthode 1 : Un seul appel massif
  Direct = {
    kmeans(dat_km, centers = 3, nstart = 4096)
  },
  # Méthode 2 : Appels répétés dans une boucle (préparation parallélisation)
  Boucle = {
    meilleur_km <- NULL
    for (i in 1:16) {
      res_actuel <- kmeans(dat_km, centers = 3, nstart = 256)
      if (is.null(meilleur_km) || res_actuel$tot.withinss < meilleur_km$tot.withinss) {
        meilleur_km <- res_actuel
      }
    }
  },
  times = 10
)

# ==============================================================================
# AFFICHAGE DES RÉSULTATS
# ==============================================================================
# Affiche le tableau statistique complet (min, median, max, etc.)
print(res_benchmark)

# Boxplot pour visualiser la distribution des temps d'exécution
boxplot(res_benchmark, main = "Comparaison des temps d'exécution (10 essais)")



# ============================================================================== 
# ANALYSE DE PERFORMANCE AVEC RPROF 
# ============================================================================== 
# Activation du profilage
Rprof("kmeans_profile.out")
# Initialisation (bien séparer les lignes)
meilleur_km <- NULL
# Exécution de la boucle pour identifier les goulots d'étranglement
for (i in 1:16) {
  res_actuel <- kmeans(dat_km, centers = 3, nstart = 256)
  if (is.null(meilleur_km) || res_actuel$tot.withinss < meilleur_km$tot.withinss) {
    meilleur_km <- res_actuel
  }
}
# Arrêt du profilage et affichage du rapport
Rprof(NULL) 
print(summaryRprof("kmeans_profile.out")$by.self)
# ============================================================================== 
# GÉNÉRATION DU GRAPHIQUE COMPARATIF (gr03_perf.pdf) 
# ==============================================================================
pdf("gr03_perf.pdf")
# Comparaison visuelle des mesures stockées dans res_benchmark Utilisation de boxplot sur l'objet microbenchmark directement
boxplot(res_benchmark,
        main = "Comparaison des temps : Direct (4096) vs Boucle (16x256)",
        ylab = "Temps d'exécution (s)",
        col = c("orange", "lightblue")) 
dev.off()
# ============================================================================== 
# EXPORT ET ACCÈS DISTANT 
# ============================================================================== 
# Copie vers le répertoire Sites
file.copy("gr03_perf.pdf", "~/Sites/perf1.pdf", overwrite = TRUE)
# URL d'accès : https://pages.isfa.fr/~mai2614301/perf1.pdf


# ==============================================================================
# PARALLÉLISATION : ANALYSE MULTICORE (MCLAPPLY)
# ==============================================================================
library(parallel)

# Configuration des essais
n_total <- 4096
n_appels <- 16        # On augmente le nombre de blocs pour mieux répartir
n_bloc <- n_total / n_appels
cores_to_test <- c(1, 2, 4, detectCores())

# Initialisation du vecteur de résultats
resultats_mcore <- numeric(length(cores_to_test))
names(resultats_mcore) <- paste(cores_to_test, "Coeurs")

# Boucle de test de performance
for (i in seq_along(cores_to_test)) {
  nb_c <- cores_to_test[i]
  cat("Test avec", nb_c, "coeur(s)...\n")
  
  t_exec <- system.time({
    # Transformation du sapply en mclapply
    liste_res <- mclapply(1:n_appels, function(j) {
      kmeans(dat_km, centers = 3, nstart = n_bloc)
    }, mc.cores = nb_c)
    
    # Agrégation (recherche du meilleur résultat)
    inertes <- sapply(liste_res, function(x) x$tot.withinss)
    meilleur_res <- liste_res[[which.min(inertes)]]
  })
  
  resultats_mcore[i] <- t_exec["elapsed"]
}

# ==============================================================================
# GRAPHIQUE DE PERFORMANCE (gr05_multicore.pdf)
# ==============================================================================
pdf("gr05_multicore.pdf")

# Graphique en barres pour comparer les temps
bp <- barplot(resultats_mcore, 
              main = "Performance K-means selon le nombre de coeurs",
              ylab = "Temps écoulé (s)", 
              col = "steelblue",
              ylim = c(0, max(resultats_mcore) * 1.2))

# Ajout des valeurs au-dessus des barres
text(x = bp, y = resultats_mcore, labels = round(resultats_mcore, 2), pos = 3)

dev.off()

# Export pour visualisation
file.copy("gr05_multicore.pdf", "~/Sites/multicore.pdf", overwrite = TRUE)

# Affichage du paramétrage le plus intéressant
cat("\n--- Bilan des performances ---\n")
print(resultats_mcore)
cat("\nLe paramétrage le plus efficace est avec", 
    cores_to_test[which.min(resultats_mcore)], "coeurs.\n")

# ==============================================================================
# CALCUL DISTRIBUÉ : EXPLORATION SNOW (clusterApply)
# ==============================================================================
library(parallel)

# Configuration du cluster local
ClLocal <- makeCluster(detectCores(), type = "PSOCK")

# Exportation des données nécessaires sur chaque noeud (indispensable en PSOCK)
clusterExport(ClLocal, "hop")

# 1. TEST INITIAL : 4096 appels de 1 nstart
# ------------------------------------------------------------------------------
t_unitaire <- system.time({
  liste_res <- clusterApply(ClLocal, rep(1, 4096), function(s) {
    kmeans(log10(hop$NbAct), 9, nstart = s)
  })
  # Extraction du meilleur (KMbest)
  inertes <- sapply(liste_res, function(x) x$tot.withinss)
  KMbest <- liste_res[[which.min(inertes)]]
})

# 2. OPTIMISATION : TEST DE DIFFÉRENTES RÉPARTITIONS (Chunks)
# ------------------------------------------------------------------------------
# Fonction pour générer des vecteurs dont la somme est 4096
# nb_blocs : nombre de paquets à envoyer au cluster
generer_repartition <- function(nb_blocs) {
  rep(4096 / nb_blocs, nb_blocs)
}

# Scénarios à tester (moins de messages = souvent plus rapide)
scenarios <- list(
  "4096 x 1" = rep(1, 4096),
  "256 x 16" = generer_repartition(256),
  "16 x 256" = generer_repartition(16),
  "8 x 512"  = generer_repartition(8)
)

resultats_snow <- numeric(length(scenarios))
names(resultats_snow) <- names(scenarios)

for (nom in names(scenarios)) {
  cat("Calcul pour le scénario :", nom, "...\n")
  vecteur_travail <- scenarios[[nom]]
  
  resultats_snow[nom] <- system.time({
    clusterApply(ClLocal, vecteur_travail, function(s) {
      kmeans(log10(hop$NbAct), 9, nstart = s)
    })
  })["elapsed"]
}

# ==============================================================================
# GRAPHIQUE DE PERFORMANCE (gr06_clusterApply.pdf)
# ==============================================================================
pdf("gr06_clusterApply.pdf")

bp <- barplot(resultats_snow, 
              main = "Impact de la granularité (clusterApply)",
              ylab = "Temps écoulé (s)", 
              col = "darkorange",
              ylim = c(0, max(resultats_snow) * 1.3))

text(x = bp, y = resultats_snow, labels = round(resultats_snow, 2), pos = 3)

dev.off()

# Nettoyage et export
stopCluster(ClLocal)
file.copy("gr06_clusterApply.pdf", "~/Sites/clusterApply.pdf", overwrite = TRUE)

cat("\nLe meilleur remplacement pour rep(1,4096) est :", 
    names(resultats_snow)[which.min(resultats_snow)], "\n")


# ==============================================================================
# COMPARAISON AVEC LOAD BALANCING ET PARLAPPLY (gr07)
# ==============================================================================
# 1. Relance du cluster pour cette nouvelle série de tests
ClLocal <- makeCluster(detectCores(), type = "PSOCK")
clusterExport(ClLocal, "hop")

# On utilise le meilleur vecteur trouvé précédemment (ex: 8 x 512)
meilleur_vecteur <- rep(512, 8)

cat("\nDébut des tests pour gr07 (Load Balancing et parLapply)...\n")

# 2. Mesures de base (sans préciser chunk.size)
# ------------------------------------------------------------------------------
t_cALB <- system.time({
  clusterApplyLB(ClLocal, meilleur_vecteur, function(s) kmeans(log10(hop$NbAct), 9, nstart = s))
})["elapsed"]

t_pLA <- system.time({
  parLapply(ClLocal, meilleur_vecteur, function(s) kmeans(log10(hop$NbAct), 9, nstart = s))
})["elapsed"]

t_pLALB <- system.time({
  parLapplyLB(ClLocal, meilleur_vecteur, function(s) kmeans(log10(hop$NbAct), 9, nstart = s))
})["elapsed"]

# 3. Mesures avec le paramètre chunk.size
# ------------------------------------------------------------------------------
chunks_a_tester <- c(1, 2, 4)
res_chunk <- list()

for (cs in chunks_a_tester) {
  # parLapply avec chunk
  nom_pLA <- paste0("pLA_chunk", cs)
  res_chunk[[nom_pLA]] <- system.time({
    parLapply(ClLocal, meilleur_vecteur, function(s) kmeans(log10(hop$NbAct), 9, nstart = s), chunk.size = cs)
  })["elapsed"]
  
  # parLapplyLB avec chunk
  nom_pLALB <- paste0("pLALB_chunk", cs)
  res_chunk[[nom_pLALB]] <- system.time({
    parLapplyLB(ClLocal, meilleur_vecteur, function(s) kmeans(log10(hop$NbAct), 9, nstart = s), chunk.size = cs)
  })["elapsed"]
}

# ==============================================================================
# SYNTHÈSE GRAPHIQUE (gr07_parLApply.pdf)
# ==============================================================================
# Regroupement de toutes les valeurs
toutes_perfs <- c("ApplyLB" = t_cALB, "parLapply" = t_pLA, "parLapplyLB" = t_pLALB, unlist(res_chunk))

pdf("gr07_parLApply.pdf")

bp <- barplot(toutes_perfs, 
              main = "Comparaison Load Balancing & Chunk Size",
              ylab = "Temps écoulé (s)", 
              col = "lightgreen",
              las = 2, # Tourne les étiquettes de l'axe X pour qu'elles soient lisibles
              ylim = c(0, max(toutes_perfs) * 1.3))

text(x = bp, y = toutes_perfs, labels = round(toutes_perfs, 2), pos = 3, cex = 0.8)

dev.off()

# Nettoyage et copie
stopCluster(ClLocal)
file.copy("gr07_parLApply.pdf", "~/Sites/parLApply.pdf", overwrite = TRUE)

# Détermination de la configuration la plus rapide
configuration_opti <- names(toutes_perfs)[which.min(toutes_perfs)]
cat("\nLa configuration conduisant aux résultats le plus rapidement est :", configuration_opti, "\n")
