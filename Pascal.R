# -------------------- Projet Evolution de l'Oïdium -------------------- #

# Packages

library(tidyverse, help, pos = 2, lib.loc = NULL)
library(data.table)
library(gridExtra)

# Assignation des string

dmi <- "DMI"

qoi <- "QoI"

dmi_qoi <- "DMI & QoI"

#  ------------------ Paramètres initiaux à modifier ------------------ #

# Nombre de générations :

nb_generations <- 100

# nombre de populations :

nb_populations <- 8

# Nombre de jours d'activitéde l'oïdium

nb_jours_an <- 150 # mi avril à mi-septembre

nb_jours_nvl_gen <- 10

nb_gen_an <- 15

scenarios <- c("Scénario A (Pas de traitement)",
              "Scénario B (Uniquements DMI)",
              "Scénario C (Uniquement QoI)",
              "Scénario D (Uniquement DMI & QoI)",
              "Scénario E (Alternance DMI et QoI)",
              "Scénario F (Populations refuges + traitements DMI)",
              "Scénario G (Populations refuges + traitements QoI)",
              "Scénario H (Populations refuges + traitements DMI & QoI)",
              "Scénario I (Traitements aléatoires)")

choix <- select.list(
  scenarios,
  title = "Choisissez un scénario",
  multiple = FALSE
)


writeLines(paste("\n", choix, "\n"))

# Planification des traitements sur un an (15 générations) (pour chaque sous populations) :

planifications_traitements_A <- list(
    data.frame( # pop 1
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 2
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 3
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 4
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 5
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 6
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 7
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 8
      generation = c(),
      traitement = c()
    )
)
planifications_traitements_B <- list(
    data.frame( # pop 1
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 4
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 6
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    )
)

planifications_traitements_C <- list(
    data.frame( # pop 1
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 4
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 6
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    )
)

planifications_traitements_D <- list(
    data.frame( # pop 1
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 4
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 6
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    )
)

planifications_traitements_E <- list(
    data.frame( # pop 1
      generation = c(3, 6, 9),
      traitement = c(dmi, qoi, dmi)
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(qoi, dmi, qoi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(dmi, qoi, dmi)
    ),
    data.frame( # pop 4
      generation = c(3, 6, 9),
      traitement = c(qoi, dmi, qoi)
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(dmi, qoi, dmi)
    ),
    data.frame( # pop 6
      generation = c(3, 6, 9),
      traitement = c(qoi, dmi, qoi)
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(dmi, qoi, dmi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(qoi, dmi, qoi)
    )
)

planifications_traitements_F <- list(
    data.frame( # pop 1
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 4
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 6
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    )
)

planifications_traitements_G <- list(
    data.frame( # pop 1
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 4
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 6
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    )
)

planifications_traitements_H <- list(
    data.frame( # pop 1
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 4
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 6
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    )
)

planifications_traitements_I <- list(
    data.frame( # pop 1
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 2
      generation = c(3, 6, 9),
      traitement = c(dmi, qoi, dmi_qoi)
    ),
    data.frame( # pop 3
      generation = c(3, 6, 9),
      traitement = c(qoi, qoi, qoi)
    ),
    data.frame( # pop 4
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 5
      generation = c(3, 6, 9),
      traitement = c(dmi, dmi, dmi)
    ),
    data.frame( # pop 6
      generation = c(),
      traitement = c()
    ),
    data.frame( # pop 7
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi_qoi, dmi_qoi)
    ),
    data.frame( # pop 8
      generation = c(3, 6, 9),
      traitement = c(dmi_qoi, dmi, qoi)
    )
)



planifications_traitements <- switch(choix,
    "Scénario A (Pas de traitement)" = planifications_traitements_A,
    "Scénario B (Uniquements DMI)" = planifications_traitements_B,
    "Scénario C (Uniquement QoI)" = planifications_traitements_C,
    "Scénario D (Uniquement DMI & QoI)" = planifications_traitements_D,
    "Scénario E (Alternance DMI et QoI)" = planifications_traitements_E,
    "Scénario F (Populations refuges + traitements DMI)" = planifications_traitements_F,
    "Scénario G (Populations refuges + traitements QoI)" = planifications_traitements_G,
    "Scénario H (Populations refuges + traitements DMI & QoI)" = planifications_traitements_H,
    "Scénario I (Traitements aléatoires)" = planifications_traitements_I,
    planifications_traitements_A # par défaut
)


# valeurs selectives avec et sans traitements :

w_traitements <- list(
  sans_traitements       = c(YG = 0.7, Yg = 0.8, yG = 0.9, yg = 1.0),
  traitements_DMI        = c(YG = 0.9, Yg = 1.0, yG = 0.1, yg = 0.2),
  traitements_QoI        = c(YG = 0.8, Yg = 0.1, yG = 1.0, yg = 0.2),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.1, yG = 0.2, yg = 0.05)
)

# Matrice des taux de migrations (m_ij = taux de migration de j vers i):

m <- matrix(
  c(#de 1,   2,    3,    4,    5,    6,    7,    8
    0.89, 0.06, 0.00, 0.00, 0.00, 0.00, 0.00, 0.05, # vers 1
    0.06, 0.89, 0.05, 0.00, 0.00, 0.00, 0.00, 0.00, # vers 2
    0.00, 0.07, 0.92, 0.01, 0.00, 0.00, 0.00, 0.00, # vers 3
    0.00, 0.00, 0.03, 0.93, 0.04, 0.00, 0.00, 0.00, # vers 4
    0.00, 0.00, 0.00, 0.02, 0.76, 0.10, 0.12, 0.00, # vers 5
    0.00, 0.00, 0.00, 0.00, 0.15, 0.65, 0.20, 0.00, # vers 6
    0.00, 0.00, 0.00, 0.00, 0.10, 0.08, 0.62, 0.20, # vers 7
    0.06, 0.00, 0.00, 0.00, 0.00, 0.00, 0.08, 0.86  # vers 8
  ),
  nrow = 8, byrow = TRUE
)


# fréquences initiales :
freq_init <- data.table(
  population = 1:8,
  pY0 = c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2),
  pG0 = c(0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
)


## ------------- Initialisation -------------------
dt <- data.table(
  generation = rep(0:nb_generations, each = nb_populations),
  population = rep(1:nb_populations, times = nb_generations + 1)
)

# Colonnes préallouées
cols <- c("pY","pG",
          "f_YG","f_Yg","f_yG","f_yg",
          "w_YG","w_Yg","w_yG","w_yg",
          "pY_s","pG_s",
          "f_YG_s","f_Yg_s","f_yG_s","f_yg_s",
          "f_YG_reprod","f_Yg_reprod","f_yG_reprod","f_yg_reprod",
          "pY_m","pG_m",
          "f_YG_m","f_Yg_m","f_yG_m","f_yg_m"
)
dt[, (cols) := NA_real_]

# initilaisation du tableau :
writeLines(" --------- Initialisation --------")
dt[generation == 0, `:=`(
  pY = freq_init$pY0,
  pY_s = freq_init$pY0,
  pY_m = freq_init$pY0,
  pG = freq_init$pG0,
  pG_s = freq_init$pG0,
  pG_m = freq_init$pG0,
  f_YG = freq_init$pY0 * freq_init$pG0,
  f_Yg = freq_init$pY0 * (1 - freq_init$pG0),
  f_yG = (1 - freq_init$pY0) * freq_init$pG0,
  f_yg = (1 - freq_init$pY0) * (1 - freq_init$pG0),
  f_YG_s = freq_init$pY0 * freq_init$pG0,
  f_Yg_s = freq_init$pY0 * (1 - freq_init$pG0),
  f_yG_s = (1 - freq_init$pY0) * freq_init$pG0,
  f_yg_s = (1 - freq_init$pY0) * (1 - freq_init$pG0),
  f_YG_reprod = freq_init$pY0 * freq_init$pG0,
  f_Yg_reprod = freq_init$pY0 * (1 - freq_init$pG0),
  f_yG_reprod = (1 - freq_init$pY0) * freq_init$pG0,
  f_yg_reprod = (1 - freq_init$pY0) * (1 - freq_init$pG0),
  f_YG_m = freq_init$pY0 * freq_init$pG0,
  f_Yg_m = freq_init$pY0 * (1 - freq_init$pG0),
  f_yG_m = (1 - freq_init$pY0) * freq_init$pG0,
  f_yg_m = (1 - freq_init$pY0) * (1 - freq_init$pG0),
  w_YG = 1.0,
  w_Yg = 1.0,
  w_yG = 1.0,
  w_yg = 1.0
)]

writeLines(" ---------  fin initialisation --------")
get_fitness <- function(population, gen, planification, val_selectives) {
  if (!is.numeric(population)) stop("population doit être numeric.")
  if (!is.numeric(gen)) stop("gen doit être numeric.")
  if (!is.list(planification)) stop("planification doit être une liste de dataframes")
  if (!is.list(val_selectives)) stop("val_selectives doit être une liste de vecteurs")
  df <- planification[[population]]
  if ((gen%%10) %in% df$generation){
    return(switch(df$traitement[df$generation == (gen%%10)],
                  "DMI" = val_selectives$traitements_DMI,
                  "QoI" = val_selectives$traitements_QoI,
                  "DMI & QoI" = val_selectives$traitements_DMI_et_QoI)
    )
  }
  return(val_selectives$sans_traitements)
}


# ------------- iteration -------------- # 


writeLines(" --------- Début des itérations --------")

for (gen in 1:nb_generations){
  for (pop in 1:nb_populations){

    idxprev <- which(dt$generation == gen-1 & dt$population == pop)
    idxcurr <- which(dt$generation == gen & dt$population == pop)

    pY_val <- dt$pY_m[idxprev]
    pG_val <- dt$pG_m[idxprev]

    f_YG_val <- dt[idxprev]$f_YG_m
    f_Yg_val <- dt[idxprev]$f_Yg_m
    f_yG_val <- dt[idxprev]$f_yG_m
    f_yg_val <- dt[idxprev]$f_yg_m

    w <- get_fitness(population = pop, gen = gen, planification = planifications_traitements, val_selectives = w_traitements)

    w_bar <- pY_val*pG_val*w["YG"] +
             pY_val*(1 -pG_val)*w["Yg"] +
             (1 - pY_val)*pG_val*w["yG"] +
             (1 - pY_val)*(1 - pG_val)*w["yg"]

    pY_s_val <- pY_val * (pG_val * w["YG"] + (1 - pG_val) * w["Yg"]) / w_bar
    pG_s_val <- pG_val * (pY_val * w["YG"] + (1 - pY_val) * w["yG"]) / w_bar

    f_YG_s_val <- f_YG_val * w["YG"] / w_bar
    f_Yg_s_val <- f_Yg_val * w["Yg"] / w_bar
    f_yG_s_val <- f_yG_val * w["yG"] / w_bar
    f_yg_s_val <- f_yg_val * w["yg"] / w_bar

    f_YG_reprod_val <- pY_s_val * pG_s_val
    f_Yg_reprod_val <- pY_s_val * (1 - pG_s_val)
    f_yG_reprod_val <- (1 - pY_s_val) * pG_s_val
    f_yg_reprod_val <- (1 - pY_s_val) * (1 - pG_s_val)

    dt[idxcurr, `:=`(
      pY = pY_val,
      pG = pG_val,
      pY_s = pY_s_val,
      pG_s = pG_s_val,
      f_YG = f_YG_val,
      f_Yg = f_Yg_val,
      f_yG = f_yG_val,
      f_yg = f_yg_val,
      f_YG_s = f_YG_s_val,
      f_Yg_s = f_Yg_s_val,
      f_yG_s = f_yG_s_val,
      f_yg_s = f_yg_s_val,
      f_YG_reprod = f_YG_reprod_val,
      f_Yg_reprod = f_Yg_reprod_val,
      f_yG_reprod = f_yG_reprod_val,
      f_yg_reprod = f_yg_reprod_val,
      w_YG = w["YG"],
      w_Yg = w["Yg"],
      w_yG = w["yG"],
      w_yg = w["yg"]
    )]
  }
  for(pop in 1:nb_populations){

    idxcurr <- which(dt$generation == gen & dt$population == pop)
    f_YG_m_val <- 0
    f_Yg_m_val <- 0
    f_yG_m_val <- 0
    f_yg_m_val <- 0

    for (pop_from in 1:nb_populations){
      idxpop <- which(dt$generation == gen & dt$population == pop_from)
      m_ij <- m[pop, pop_from]
      f_YG_from <- dt$f_YG_reprod[idxpop]
      f_Yg_from <- dt$f_Yg_reprod[idxpop]
      f_yG_from <- dt$f_yG_reprod[idxpop]
      f_yg_from <- dt$f_yg_reprod[idxpop]

      f_YG_m_val <- f_YG_m_val + m_ij * f_YG_from
      f_Yg_m_val <- f_Yg_m_val + m_ij * f_Yg_from
      f_yG_m_val <- f_yG_m_val + m_ij * f_yG_from
      f_yg_m_val <- f_yg_m_val + m_ij * f_yg_from
    }

    pY_m_val <- f_YG_m_val + f_Yg_m_val
    pG_m_val <- f_YG_m_val + f_yG_m_val

    dt[idxcurr, `:=`(
      f_YG_m = f_YG_m_val,
      f_Yg_m = f_Yg_m_val,
      f_yG_m = f_yG_m_val,
      f_yg_m = f_yg_m_val,
      pY_m = pY_m_val,
      pG_m = pG_m_val
    )]
  }
}

writeLines(" ---------  fin iterations --------")


# Moyenne globale

dt_tot <- dt[, .(
  pY_tot = mean(pY_m),
  pG_tot = mean(pG_m)
), by = generation]


# ------------- graphique -------------- #

# PY et PG au cours des générations pour chaque population
p1 <- ggplot(dt, aes(x = generation, y = pY_m, color = as.factor(population))) +
  geom_line(alpha = 0.7) +
  geom_line(data = dt_tot, aes(x = generation, y = pY_tot),
            color = "black", linewidth = 1.3) +
  labs(title = "Fréquence de l'allèle Y (populations + moyenne globale)",
       x = "Génération",
       y = "Fréquence de l'allèle Y",
       color = "Population") +
  theme_minimal()
p2 <- ggplot(dt, aes(x = generation, y = pG_m, color = as.factor(population))) +
  geom_line(alpha = 0.7) +
  geom_line(data = dt_tot, aes(x = generation, y = pG_tot),
            color = "black", linewidth = 1.3) +
  labs(title = "Fréquence de l'allèle G au cours des générations",
       x = "Génération",
       y = "Fréquence de l'allèle G",
       color = "Population") +
  theme_minimal()

# affiches les deux plot sur un meme graphique

p <- grid.arrange(p1, p2, ncol = 1)

# Nettoyage du nom du scénario pour le nom de fichier
choix_clean <- gsub("[^A-Za-z0-9_]+", "_", choix)

ggsave(paste0("evolution_freq_Y_G_", choix_clean, ".png"),
       p,
       width = 8, height = 10)