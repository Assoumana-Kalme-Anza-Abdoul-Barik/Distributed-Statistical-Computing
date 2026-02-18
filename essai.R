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
