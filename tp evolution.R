#modele: 4 génotypes (YG, Yg, yG, yg) dans 3 patches (Y, G, C)
# Selection (fitness depends on patch and genotype) -> normalize -> migration with m1, m2

#patch Y: traité fortement avec le fongicide (région)
#patch G: traité moyen
#patch C: non traité

# PARAMÈTRES
gens <- 200            # nb de generation
m1 <- 0.05             # migration rate from patch G into Y (choisir aléatoire)
m2 <- 0.03             # migration rate from patch C into Y (choisir aléatoire)
stopifnot(m1 + m2 <= 1)

# génotypes
gtypes <- c("YG","Yg","yG","yg")

# Fitness (valeur sélective w): matrix (patch x génotype)
#valeur choisit aléatoire et fixé et on peut modifier
W <- matrix(c(
  # YG  Yg  yG  yg   <- genotype columns
  1.3,1.0,0.9,0.8,  # patch Y
  1.1,1.0,0.95,0.9, # patch G
  0.9,1.0,1.0,0.95  # patch C
), nrow=3, byrow=TRUE, dimnames=list(c("Y","G","C"), gtypes))

# Frequencies initiales: sum = 1
pY <- c(YG=0.4, Yg=0.2, yG=0.3, yg=0.1)
pG <- c(YG=0.2, Yg=0.3, yG=0.4, yg=0.1)
pC <- c(YG=0.1, Yg=0.2, yG=0.4, yg=0.3)
stopifnot(abs(sum(pY)-1) < 1e-8, abs(sum(pG)-1) < 1e-8, abs(sum(pC)-1) < 1e-8)

# Data frame
res <- data.frame(gen = 0:gens,
                  Y_YG=NA, Y_Yg=NA, Y_yG=NA, Y_yg=NA,
                  G_YG=NA, G_Yg=NA, G_yG=NA, G_yg=NA,
                  C_YG=NA, C_Yg=NA, C_yG=NA, C_yg=NA)

res[1, 2:13] <- c(pY, pG, pC)

#SIMULATION
for (t in 1:gens) {
  # Selection step: multiply by fitness, then normalize in each patch
  # patch Y
  wbar_Y <- sum(pY * W["Y", ]) #formule déjà fourni en cours
  pY_sel <- (pY * W["Y", ]) / wbar_Y #formule déjà fourni en cours
  
  # patch G
  wbar_G <- sum(pG * W["G", ])
  pG_sel <- (pG * W["G", ]) / wbar_G
  
  # patch C
  wbar_C <- sum(pC * W["C", ])
  pC_sel <- (pC * W["C", ]) / wbar_C
  
  # Migration step
  # new freq in Y receives (1 - m1 - m2) of its selected individuals, plus m1 from G and m2 from C
  pY_new <- (1 - m1 - m2) * pY_sel + m1 * pG_sel + m2 * pC_sel
  pG_new <- (1 - m1 - m2) * pG_sel + m1 * pY_sel + m2 * pC_sel
  pC_new <- (1 - m1 - m2) * pC_sel + m1 * pY_sel + m2 * pG_sel
  
  # numerical safety: renormalize if tiny numeric error
  pY <- pY_new / sum(pY_new)
  pG <- pG_new / sum(pG_new)
  pC <- pC_new / sum(pC_new)
  
  res[t+1, 2:13] <- c(pY, pG, pC)
}

# PLOTTING: genotype frequencies per patch
par(mfrow=c(2,1), mar=c(4,4,1,1))
# Patch Y
matplot(res$gen, as.matrix(res[, c("Y_YG","Y_Yg","Y_yG","Y_yg")]), type="l", lty=1, lwd=2,
        xlab="Generation", ylab="Freq (patch Y)", ylim=c(0,1), col=1:4)
#utiliser matplotlib pour faire le plot en R
legend("topright", legend=gtypes, col=1:4, lwd=2, bty="n")

# Patch G
matplot(res$gen, as.matrix(res[, c("G_YG","G_Yg","G_yG","G_yg")]), type="l", lty=1, lwd=2,
        xlab="Generation", ylab="Freq (patch G)", ylim=c(0,1), col=1:4)
legend("topright", legend=gtypes, col=1:4, lwd=2, bty="n")

# Patch C (separate plot)
plot(res$gen, res$C_YG, type="l", col=1, lwd=2, ylim=c(0,1),
     xlab="Generation", ylab="Freq (patch C)")
lines(res$gen, res$C_Yg, col=2, lwd=2)
lines(res$gen, res$C_yG, col=3, lwd=2)
lines(res$gen, res$C_yg, col=4, lwd=2)
legend("topright", legend=gtypes, col=1:4, lwd=2, bty="n")

#calculer alleles frequences
alleles <- data.frame(gen=res$gen,
                      Y_Y = res$Y_YG + res$Y_Yg,  # allele Y frequency in patch Y
                      Y_G = res$Y_yG + res$Y_yg,  # allele G frequency in patch Y (note naming)
                      G_Y = res$G_YG + res$G_Yg,
                      G_G = res$G_yG + res$G_yg,
                      C_Y = res$C_YG + res$C_Yg,
                      C_G = res$C_yG + res$C_yg)

# plot allele Y frequency across patches = ce code pour fusionner toutes les lines/plot dans 1 graph
plot(alleles$gen, alleles$Y_Y, type="l", col="blue", lwd=2, ylim=c(0,1),
     xlab="Generation", ylab="Allele freq (Y)", main="Allele Y frequency")
lines(alleles$gen, alleles$G_Y, col="red", lwd=2)
lines(alleles$gen, alleles$C_Y, col="darkgreen", lwd=2)
legend("topright", legend=c("patch Y","patch G","patch C"), col=c("blue","red","darkgreen"), lwd=2)
