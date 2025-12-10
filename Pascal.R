# -------------------- Projet Evolution de l'Oïdium -------------------- #

# Packages

library(tidyverse, help, pos = 2, lib.loc = NULL)
library(data.table)
library(gridExtra)

library(tcltk)

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

#nb_jours_an <- 150 # mi avril à mi-septembre

#nb_jours_nvl_gen <- 10

# Nombre de générations par an :
nb_gen_an <- 15

scenarios_traitement <- c("A:Pas de traitement",
              "B:Uniquements DMI",
              "C:Uniquement QoI",
              "D:Uniquement DMI & QoI",
              "E:Alternance DMI et QoI",
              "F:Populations refuges + traitements DMI",
              "G:Populations refuges + traitements QoI",
              "H:Populations refuges + traitements DMI & QoI",
              "I:Traitements aléatoires")

scenarios_fit <- c("1:Pas de coût de la résistance",
                   "2:Coût modéré additif à la résistance",
                   "3:Coût élevé additif à la résistance",
                   "4:Coût modére et cumulation élevé de la résistance",
                   "5:Traitements peu efficaces",
                   "6:Traitements très efficaces")

scenarios_freq_init <- c("1: toutes différérentes",
                         "2: toutes identiques fabile",
                         "3: toutes identiques modérée",
                         "4: aleatoire faible")

choix_scenarios <- function(options1, options2, options3,
                         titre1 = "Traitement",
                         titre2 = "Fitness",
                         titre3 = "Fréquences initiales") {
  tt <- tktoplevel()
  tkwm.title(tt, "Choix multiples")
  
  frame1 <- tkframe(tt, padx = 10, pady = 10)
  frame2 <- tkframe(tt, padx = 10, pady = 10)
  frame3 <- tkframe(tt, padx = 10, pady = 10)
  
  tkpack(tklabel(frame1, text = titre1))
  tkpack(tklabel(frame2, text = titre2))
  tkpack(tklabel(frame3, text = titre3))
  
  # Listbox 1
  lb1 <- tklistbox(frame1, height = 10, width = 40, selectmode = "single", exportselection = FALSE)
  tkpack(lb1, expand = TRUE)
  
  # Remplissage manuel (évite le split sur les espaces)
  for (i in options1) {
    tkinsert(lb1, "end", i)
  }
  
  # Choix par défaut (1er élément)
tkselection.set(lb1, 0)

  # Listbox 2
  lb2 <- tklistbox(frame2, height = 10, width = 40, selectmode = "single", exportselection = FALSE)
  tkpack(lb2, expand = TRUE)
  
  for (i in options2) {
    tkinsert(lb2, "end", i)
  }

  # Choix par défaut (1er élément)
  tkselection.set(lb2, 0)

  # listbox 3
  lb3 <- tklistbox(frame3, height = 10, width = 40, selectmode = "single", exportselection = FALSE)
  tkpack(lb3, expand = TRUE)

  for (i in options3) {
    tkinsert(lb3, "end", i)
  }
  
  # Choix par défaut (1er élément)
  tkselection.set(lb3, 0)

  validate <- tclVar(0)
  
  tkpack(
    tkbutton(tt, text = "Valider",
             command = function() tclvalue(validate) <<- 1),
    pady = 10
  )
  
  tkpack(frame1, side = "left")
  tkpack(frame2, side = "left")
  tkpack(frame3, side = "left")
  
  # Attente du clic
  tkwait.variable(validate)
  
  # Récupération des choix
  idx1 <- as.integer(tkcurselection(lb1)) + 1
  idx2 <- as.integer(tkcurselection(lb2)) + 1
  idx3 <- as.integer(tkcurselection(lb3)) + 1
  
  traitement <- if(length(idx1)) options1[idx1] else NA
  fitness <- if(length(idx2)) options2[idx2] else NA
  init <- if(length(idx3)) options3[idx3] else NA
  
  tkdestroy(tt)

  return(list(traitement = traitement, fitness = fitness, init = init))
}

choix <- choix_scenarios(scenarios_traitement, scenarios_fit, scenarios_freq_init)

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



planifications_traitements <- switch(choix$traitement,
    "A:Pas de traitement" = planifications_traitements_A,
    "B:Uniquements DMI" = planifications_traitements_B,
    "C:Uniquement QoI" = planifications_traitements_C,
    "D:Uniquement DMI & QoI" = planifications_traitements_D,
    "E:Alternance DMI et QoI" = planifications_traitements_E,
    "F:Populations refuges + traitements DMI" = planifications_traitements_F,
    "G:Populations refuges + traitements QoI" = planifications_traitements_G,
    "H:Populations refuges + traitements DMI & QoI" = planifications_traitements_H,
    "I:Traitements aléatoires" = planifications_traitements_I,
    planifications_traitements_A # par défaut
)


# valeurs selectives avec et sans traitements :

# pas de coût à la résisatnce
w_traitements_1 <- list(
  sans_traitements       = c(YG = 1.0, Yg = 1.0, yG = 1.0, yg = 1.0),
  traitements_DMI        = c(YG = 1.0, Yg = 1.0, yG = 0.2, yg = 0.2),
  traitements_QoI        = c(YG = 1.0, Yg = 0.2, yG = 1.0, yg = 0.2),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.2, yG = 0.2, yg = 0.05)
)
# coût additionnel à la résistance et légerement supérieur pour la res aux DMI
w_traitements_2 <- list(
  sans_traitements       = c(YG = 0.7, Yg = 0.8, yG = 0.9, yg = 1.0),
  traitements_DMI        = c(YG = 0.9, Yg = 1.0, yG = 0.1, yg = 0.2),
  traitements_QoI        = c(YG = 0.8, Yg = 0.1, yG = 1.0, yg = 0.2),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.1, yG = 0.2, yg = 0.05)
)

# coût additionnel fort à la résistance et légerement supérieur pour la res aux DMI
w_traitements_3 <- list(
  sans_traitements       = c(YG = 0.3, Yg = 0.6, yG = 0.7, yg = 1.0),
  traitements_DMI        = c(YG = 0.7, Yg = 1.0, yG = 0.01, yg = 0.2),
  traitements_QoI        = c(YG = 0.6, Yg = 0.01, yG = 1.0, yg = 0.2),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.1, yG = 0.2, yg = 0.05)
)

# double résistance très coûteuse
w_traitements_4 <- list(
  sans_traitements       = c(YG = 0.4, Yg = 0.8, yG = 0.9, yg = 1.0),
  traitements_DMI        = c(YG = 0.6, Yg = 1.0, yG = 0.1, yg = 0.2),
  traitements_QoI        = c(YG = 0.5, Yg = 0.1, yG = 1.0, yg = 0.2),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.1, yG = 0.2, yg = 0.05)
)

# traitements peu efficaces
w_traitements_5 <- list(
  sans_traitements       = c(YG = 0.7, Yg = 0.8, yG = 0.9, yg = 1.0),
  traitements_DMI        = c(YG = 0.9, Yg = 1.0, yG = 0.4, yg = 0.5),
  traitements_QoI        = c(YG = 0.8, Yg = 0.3, yG = 1.0, yg = 0.5),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.10, yG = 0.15, yg = 0.25)
)

# traitements très efficaces
w_traitements_6 <- list(
  sans_traitements       = c(YG = 0.7, Yg = 0.8, yG = 0.9, yg = 1.0),
  traitements_DMI        = c(YG = 0.9, Yg = 1.0, yG = 0.005, yg = 0.01),
  traitements_QoI        = c(YG = 0.8, Yg = 0.005, yG = 1.0, yg = 0.01),
  traitements_DMI_et_QoI = c(YG = 1.0, Yg = 0.005, yG = 0.005, yg = 0.001)
)

w_traitements <- switch(choix$fitness,
                   "1:Pas de coût de la résistance" = w_traitements_1,
                   "2:Coût modéré additif à la résistance" = w_traitements_2,
                   "3:Coût élevé additif à la résistance" = w_traitements_3,
                   "4:Coût modére et cumulation élevé de la résistance" = w_traitements_4,
                   "5:Traitements peu efficaces" = w_traitements_5,
                   "6:Traitements très efficaces" = w_traitements_6,
                    w_traitements_1# par défaut
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


# fréquences initiales toutes diffs :
freq_init_1 <- data.table(
  population = 1:8,
  pY0 = c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2),
  pG0 = c(0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
)
# fréquences indentiques faibles :
freq_init_2 <- data.table(
  population = 1:8,
  pY0 = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),
  pG0 = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05)
)
# fréquences identiques modérées :
freq_init_3 <- data.table(
  population = 1:8,
  pY0 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
  pG0 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
)
# fréquences aléatoires faibles :
freq_init_4 <- data.table(
  population = 1:8,
  pY0 = c(0.1,0.15,0.05,0.2,0.1,0.15,0.05,0.2),
  pG0 = c(0.2,0.1,0.15,0.05,0.2,0.1,0.15,0.05)
)
freq_init <- switch(choix$init,
                        "1: toutes différérentes" = freq_init_1,
                        "2: toutes identiques fabile" = freq_init_2,
                        "3: toutes identiques modérée" = freq_init_3,
                        "4: aleatoire faible" = freq_init_4,
                        freq_init_1# par défaut
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
  if ((gen%%nb_gen_an) %in% df$generation){
    return(switch(df$traitement[df$generation == (gen%%nb_gen_an)],
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

    w <- get_fitness(population = pop,
                     gen = gen,
                     planification = planifications_traitements,
                     val_selectives = w_traitements)

    w_bar <- f_YG_val*w["YG"] +
             f_Yg_val*w["Yg"] +
             f_yG_val*w["yG"] +
             f_yg_val*w["yg"]

    pY_s_val <- (f_YG_val * w["YG"] + f_Yg_val * w["Yg"]) / w_bar
    pG_s_val <- (f_YG_val * w["YG"] + f_yG_val * w["yG"]) / w_bar

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
  pG_tot = mean(pG_m),
  fYG_tot = mean(f_YG_m),
  fYg_tot = mean(f_Yg_m),
  fyG_tot = mean(f_yG_m),
  fyg_tot = mean(f_yg_m)
), by = generation]


# ------------- graphique -------------- #

# PY et PG au cours des générations pour chaque population

plot_allele <- function(dt, dt_tot, var_pop, var_tot, titre, ylab) {
  
  # vecteur des populations
  pops <- levels(as.factor(dt$population))
  
  # couleurs automatiques nommées
  pop_cols <- setNames(scales::hue_pal()(length(pops)), pops)
  
  # ajout de la couleur de la moyenne
  all_cols <- c(pop_cols, "Moyenne" = "black")
  
  ggplot(dt, aes(x = generation, y = .data[[var_pop]],
                 color = as.factor(population))) +
    geom_line(alpha = 0.7) +
    
    geom_line(data = dt_tot,
              aes(x = generation, y = .data[[var_tot]],
                  color = "Moyenne"),
              linewidth = 1.3) +
    
    scale_color_manual(name = "Population",
                       values = all_cols) +
    
    labs(title = titre,
         x = "Génération",
         y = ylab) +
    theme_minimal()
}

py <- plot_allele(dt, dt_tot,
                  var_pop = "pY_m",
                  var_tot = "pY_tot",
                  titre = "Fréquence de l'allèle Y au cours des générations",
                  ylab = "Fréquence de l'allèle Y")

pg <- plot_allele(dt, dt_tot,
                  var_pop = "pG_m",
                  var_tot = "pG_tot",
                  titre = "Fréquence de l'allèle G au cours des générations",
                  ylab = "Fréquence de l'allèle G")


# affiches les deux plot sur un meme graphique

p <- grid.arrange(py, pg, ncol = 1)

# Nettoyage du nom du scénario pour le nom de fichier
choix_clean <- paste(gsub("[^A-Za-z0-9_]+", "_", choix$traitement),
                     gsub("[^A-Za-z0-9_]+", "_", choix$fitness),
                     gsub("[^A-Za-z0-9_]+", "_", choix$init),
                     sep = "_")

ggsave(paste0("evolution_freq_Y_G_", choix_clean, ".png"),
       p,
       width = 8, height = 10)

# frequence génotypiques cours des générations pour chaque population
fYG <- plot_allele(dt, dt_tot,
                  var_pop = "f_YG_m",
                  var_tot = "fYG_tot",
                  titre = "Fréquence du génotype YG au cours des générations",
                  ylab = "Fréquence du génotype YG")

fYg <- plot_allele(dt, dt_tot,
                  var_pop = "f_Yg_m",
                  var_tot = "fYg_tot",
                  titre = "Fréquence du génotype Yg au cours des générations",
                  ylab = "Fréquence du génotype Yg")

fyG <- plot_allele(dt, dt_tot,
                  var_pop = "f_yG_m",
                  var_tot = "fyG_tot",
                  titre = "Fréquence du génotype yG au cours des générations",
                  ylab = "Fréquence du génotype yG")

fyg <- plot_allele(dt, dt_tot,
                  var_pop = "f_yg_m",
                  var_tot = "fyg_tot",
                  titre = "Fréquence du génotype yg au cours des générations",
                  ylab = "Fréquence du génotype yg")


# affiche les plots sur un meme graphique

f <- grid.arrange(fYG, fYg, fyG, fyg, ncol = 2)



ggsave(paste0("evolution_freq_geno_", choix_clean, ".png"),
       f,
       width = 16, height = 10)