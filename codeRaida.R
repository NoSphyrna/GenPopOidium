library(ggplot2)
library(dplyr)
library(tidyr)

simulate_force_evolutive <- function(
    n_pop = 4,
    Ne = rep(1000, 4),
    generations = 200,
    reps = 50,
    pDMI_init = c(0.05, 0.10, 0.02, 0.03),
    pQOI_init = c(0.03, 0.15, 0.01, 0.05),
    s_DMI = 0.10,
    s_QOI = 0.15,
    distance = NULL,
    m = 0.05,
    use_drift = TRUE,
    seed = 42
){
  set.seed(seed)
  
  phenotypes <- c("DMI_S", "DMI_R", "QOI_S", "QOI_R")
  fitness <- c(DMI_S = 1, DMI_R = 1 - s_DMI, QOI_S = 1, QOI_R = 1 - s_QOI)
  
  # --- distance par défaut si NULL (exemple simple) ---
  if(is.null(distance)){
    distance <- matrix(c(
      0,10,30,50,
      10,0,20,40,
      30,20,0,25,
      50,40,25,0
    ), nrow = n_pop, byrow = TRUE)
  }
  if(!all(dim(distance) == c(n_pop, n_pop))) stop("distance doit être une matrice n_pop x n_pop")
  
  # stockage
  results_DMI <- array(NA, dim = c(generations+1, n_pop, reps))
  results_QOI <- array(NA, dim = c(generations+1, n_pop, reps))
  
  for(rep in 1:reps){
    populations <- matrix(0, n_pop, 4)
    colnames(populations) <- phenotypes
    populations[, "DMI_R"] <- pDMI_init
    populations[, "DMI_S"] <- 1 - pDMI_init
    populations[, "QOI_R"] <- pQOI_init
    populations[, "QOI_S"] <- 1 - pQOI_init
    
    results_DMI[1, , rep] <- pDMI_init
    results_QOI[1, , rep] <- pQOI_init
    
    for(gen in 2:(generations+1)){
      # SELECTION
      for(i in 1:n_pop){
        x <- populations[i, ]
        wbar <- sum(x * fitness)
        if(wbar <= 0) {
          # garde contre div/0 : si wbar 0 → on laisse x inchangé
          x_sel <- x
        } else {
          x_sel <- (x * fitness) / wbar
        }
        populations[i, ] <- x_sel
      }
      
      # MIGRATION robuste (évite Inf/NA)
      pop_after_mig <- matrix(0, n_pop, 4)
      colnames(pop_after_mig) <- phenotypes
      
      for(i in 1:n_pop){
        # calcule poids = inverse de la distance en ignorant la diagonale
        tmp <- distance[i, ]
        # mettre la diagonale en NA pour éviter 1/0
        tmp[i] <- NA
        weights <- 1 / tmp            # produit NA ou Inf si tmp 0/NA
        # convertir Inf/NA en 0
        weights[!is.finite(weights)] <- 0
        # s'il n'y a aucune source, distribue uniformément sur les autres
        if(sum(weights) == 0){
          weights <- rep(1, n_pop)
          weights[i] <- 0
        }
        # normalise
        weights <- weights / sum(weights)
        
        # calcul des migrants : somme des populations[j,] * poids[j]
        migrants <- colSums(sapply(1:n_pop, function(j) populations[j, ] * weights[j]))
        pop_after_mig[i, ] <- (1 - m) * populations[i, ] + m * migrants
      }
      
      populations <- pop_after_mig
      
      # DERIVE multinomiale sur les 4 phénotypes
      if(use_drift){
        for(i in 1:n_pop){
          N <- Ne[i]
          if(N <= 0) stop("Ne doit être > 0")
          new_counts <- rmultinom(1, size = N, prob = populations[i, ])
          # rmultinom renvoie une matrix 4 x 1 ; on met en vecteur / N
          populations[i, ] <- as.vector(new_counts) / N
        }
      }
      
      results_DMI[gen, , rep] <- populations[, "DMI_R"]
      results_QOI[gen, , rep] <- populations[, "QOI_R"]
    } # fin génération
  } # fin reps
  
  # Moyennes et se
  mean_DMI <- apply(results_DMI, c(1,2), mean)
  se_DMI <- apply(results_DMI, c(1,2), sd) / sqrt(reps)
  mean_QOI <- apply(results_QOI, c(1,2), mean)
  se_QOI <- apply(results_QOI, c(1,2), sd) / sqrt(reps)
  
  # DF long DMI
  df_mean_DMI <- as.data.frame(mean_DMI)
  colnames(df_mean_DMI) <- paste0("Pop", 1:n_pop)
  df_mean_DMI$Generation <- 0:generations
  df_mean_DMI_long <- pivot_longer(df_mean_DMI, cols = -Generation, names_to = "Pop", values_to = "mean")
  df_se_DMI <- as.data.frame(se_DMI); colnames(df_se_DMI) <- paste0("Pop", 1:n_pop); df_se_DMI$Generation <- 0:generations
  df_se_DMI_long <- pivot_longer(df_se_DMI, cols = -Generation, names_to = "Pop", values_to = "se")
  df_DMI_long <- left_join(df_mean_DMI_long, df_se_DMI_long, by = c("Generation","Pop")) %>%
    mutate(lower = pmax(mean - 1.96*se, 0), upper = pmin(mean + 1.96*se, 1))
  
  # DF long QOI
  df_mean_QOI <- as.data.frame(mean_QOI)
  colnames(df_mean_QOI) <- paste0("Pop", 1:n_pop)
  df_mean_QOI$Generation <- 0:generations
  df_mean_QOI_long <- pivot_longer(df_mean_QOI, cols = -Generation, names_to = "Pop", values_to = "mean")
  df_se_QOI <- as.data.frame(se_QOI); colnames(df_se_QOI) <- paste0("Pop", 1:n_pop); df_se_QOI$Generation <- 0:generations
  df_se_QOI_long <- pivot_longer(df_se_QOI, cols = -Generation, names_to = "Pop", values_to = "se")
  df_QOI_long <- left_join(df_mean_QOI_long, df_se_QOI_long, by = c("Generation","Pop")) %>%
    mutate(lower = pmax(mean - 1.96*se, 0), upper = pmin(mean + 1.96*se, 1))
  
  # Graphs
  p1 <- ggplot(df_DMI_long, aes(x = Generation, y = mean, color = Pop, fill = Pop)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) + theme_classic() + labs(title = "Évolution moyenne de DMI_R", y = "Fréquence")
  
  p2 <- ggplot(df_QOI_long, aes(x = Generation, y = mean, color = Pop, fill = Pop)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) + theme_classic() + labs(title = "Évolution moyenne de QOI_R", y = "Fréquence")
  
  return(list(
    results_DMI = results_DMI,
    results_QOI = results_QOI,
    df_DMI_long = df_DMI_long,
    df_QOI_long = df_QOI_long,
    plot_DMI = p1,
    plot_QOI = p2
  ))
}


distance <- matrix(c(
  0,10,30,50,
  10,0,20,40,
  30,20,0,25,
  50,40,25,0
), 4,4, byrow=TRUE)

res <- simulate_force_evolutive(distance = distance, reps = 20, generations = 200)
p1=res$plot_DMI
p2=res$plot_QOI

library(patchwork)

# Affichage en grille 2x2 avec titre global et axe x commun
(p1 | p2) +
  plot_annotation(
    title = "Évolution des résistances DMI, QoI",
    caption = "Source: Simulation",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  ) 

