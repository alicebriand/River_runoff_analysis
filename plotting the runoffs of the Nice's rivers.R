# Plotting the runoffs of the Nice's rivers

# pathway : "~/Documents/Alice/code/plotting the runoffs of the Nice's rivers

# libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggpmisc)
library(forecast)
library(broom)

# loading -----------------------------------------------------------------

load("data/Hydro France/Y6442010_depuis_2000.Rdata")
load("data/MNCA/Paillon_all_debit.Rdata")
load("data/MNCA/Magnan_all_debit.Rdata")

# Complete missing dates in the date range
Y6442010_depuis_2000 <- Y6442010_depuis_2000 %>%
  complete(date = seq(min(date), max(date), by = "day"))

Paillon_all_debit <- Paillon_all_debit %>%
  complete(date = seq(min(date), max(date), by = "day"))

Magnan_all_debit <- Magnan_all_debit %>%
  complete(date = seq(min(date), max(date), by = "day"))


# # Calcul des dates min et max (en dehors de complete)
# date_min <- min(Paillon_all_debit$date, na.rm = TRUE)
# date_max <- max(Paillon_all_debit$date, na.rm = TRUE)
# 
# # Complétion des dates manquantes
# Paillon_all_debit <- Paillon_all_debit %>%
#   complete(
#     date = seq(date_min, date_max, by = "day"),
#     fill = list(debit = NA)  # Remplace "debit" par le nom de ta colonne
#   )
# 
# Paillon_all_debit <- Paillon_all_debit %>%
#   complete(
#     date = seq(as.Date(min(date)), as.Date(max(date)), by = "day"),
#     fill = list(debit = NA)  # Remplace "debit" par le nom de ta colonne de débit
#   )

# 1. Convertir explicitement la colonne date en Date (si ce n'est pas déjà fait)
# Paillon_all_debit$date <- as.Date(Paillon_all_debit$date)

# 2. Filtrer les NA dans la colonne date (au cas où)
# Paillon_all_debit <- Paillon_all_debit %>% filter(!is.na(date))

# 3. Créer un dataframe avec toutes les dates de la période
all_dates <- data.frame(
  date = seq(
    min(Paillon_all_debit$date, na.rm = TRUE),
    max(Paillon_all_debit$date, na.rm = TRUE),
    by = "day"
  )
)

# 4. Joindre avec le dataframe original
Paillon_all_debit <- all_dates %>%
  left_join(Paillon_all_debit, by = "date")

# scaling function -----------------------------------------------------------------

# Scale one value to another for tidier double-y-axis plots
sec_axis_adjustement_factors <- function(var_to_scale, var_ref) {
  
  index_to_keep <- which(is.finite(var_ref))
  var_ref <- var_ref[index_to_keep]
  
  index_to_keep <- which(is.finite(var_to_scale))
  var_to_scale <- var_to_scale[index_to_keep]
  
  max_var_to_scale <- max(var_to_scale, na.rm = T) 
  min_var_to_scale <- min(var_to_scale, na.rm = T) 
  max_var_ref <- max(var_ref, na.rm = T) 
  min_var_ref <- min(var_ref, na.rm = T) 
  
  diff_to_scale <- max_var_to_scale - min_var_to_scale
  diff_to_scale <- ifelse(diff_to_scale == 0, 1 , diff_to_scale)
  diff_ref <- max_var_ref - min_var_ref
  diff <- diff_ref / diff_to_scale
  
  adjust <- (max_var_ref - max_var_to_scale*diff) 
  
  return(data.frame(diff = diff, adjust = adjust, operation = "scaled var = (var_to_scale * diff) + adjust",
                    trans_axis_operation = "var_to_scale = {scaled_var - adjust} / diff)"))
}

# plotting ----------------------------------------------------------------
# plotting each runoff separately
## Var runoff --------------------------------------------------------------

# we have a big hole in our data from 2001 to 2006
# we have to put a weighted lm on complet datas

# create a new dataframe, starting in 2006

Y6442010_depuis_2006 <- Y6442010_depuis_2000 |> 
  filter(date >= "2006-01-01")

# Then, calculate lm for this part only

# Calculer le modèle de régression linéaire
modele_Var_2006 <- lm(débit ~ date, data = Y6442010_depuis_2006, 
                      weights = rep(1, nrow(Y6442010_depuis_2006)))

# Extraire les résultats avec broom::tidy()
resultats_Var_2006 <- tidy(modele_Var_2006)

# Extraire la valeur p de la pente
p_value_Var_2006 <- resultats_Var_2006$p.value[2]  # 2ème ligne = coefficient de 'date'

# Extraire les coefficients pour l'équation
intercept_Var_2006 <- coef(modele_Var_2006)[1]
slope_Var_2006 <- coef(modele_Var_2006)[2]

# Créer un data.frame pour la ligne de régression (uniquement 2006-2026)
Y6442010_depuis_2006$débit_pred <- predict(modele_Var_2006, Y6442010_depuis_2006)

      # en échelle normale

# Créer le graphique
ggplot() +
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit), color = "blue") +
  geom_line(data = Y6442010_depuis_2006, aes(x = date, y = débit_pred), color = "red", linewidth = 1) +
  # Ajouter l'intervalle de confiance (optionnel)
  geom_smooth(data = Y6442010_depuis_2006,  aes(x = date, y = débit), method = "lm", se = TRUE, color = NA, fill = "pink", alpha = 0.2) +
  # Annoter l'équation et la p-value
  annotate(
    "text",
    x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.9,
    label = paste0(
      "y = ", round(intercept_Var_2006, 3), "", round(slope_Var_2006, 7), " * x",
      "\n", "p = ", ifelse(p_value_Var_2006 < 0.001, "< 0.001", format(p_value_Var_2006, scientific = TRUE, digits = 3))
    ),
    hjust = 0,  # Alignement à gauche
    vjust = 1,  # Alignement en haut
    size = 5
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Var entre 2000 et 2026",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()

# put a weighted linear regression model on our datas
# if peak exceed a threshold, then give them less weight

# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil <- mean(Y6442010_depuis_2006$débit, na.rm = TRUE) + 2 * sd(Y6442010_depuis_2006$débit, na.rm = TRUE)
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
Y6442010_depuis_2006$poids <- ifelse(Y6442010_depuis_2006$débit > seuil, 0.1, 1)


# Calculer le modèle linéaire pondéré
modele_Var_2006_pondéré <- lm(débit ~ date, data = Y6442010_depuis_2006, weights = poids)

# Extraire les résultats
resultats_Var_2006_pondéré <- tidy(modele_Var_2006_pondéré)
p_value_Var_2006_pondéré <- resultats_Var_2006_pondéré$p.value[2]
intercept_Var_2006_pondéré <- coef(modele_Var_2006_pondéré)[1]
slope_Var_2006_pondéré <- coef(modele_Var_2006_pondéré)[2]

# Ajouter les prédictions au data.frame
Y6442010_depuis_2006$débit_pred_pondéré <- predict(modele_Var_2006_pondéré, Y6442010_depuis_2006)

ggplot() +
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit), color = "blue") +
  geom_line(data = Y6442010_depuis_2006, aes(x = date, y = débit_pred_pondéré), color = "springgreen4", linewidth = 1) +
  annotate(
    "text",
    x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.85,
    label = paste0(
      "y = ", round(intercept_Var_2006_pondéré, 3), "", round(slope_Var_2006_pondéré, 7), " * x",
      "\n", "p = ", ifelse(p_value_Var_2006_pondéré < 0.001, "< 0.001", format(p_value_Var_2006_pondéré, scientific = TRUE, digits = 3))
    ),
    hjust = 0,
    vjust = 1,
    size = 5,
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Var entre 2000 et 2026",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()




# compare the linear model and the weighted linear model
ggplot() +
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit), color = "blue") +
  geom_line(data = Y6442010_depuis_2006, aes(x = date, y = débit_pred), color = "red", linewidth = 1) +
  geom_line(data = Y6442010_depuis_2006, aes(x = date, y = débit_pred_pondéré), color = "springgreen4", linewidth = 1) +
  annotate(
    "text",
    x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.9,
    label = paste0(
      "Non pondéré: y = ", round(intercept_Var_2006, 3), " + ", round(slope_Var_2006, 7), " * x",
      "\n", "Pondéré: y = ", round(intercept_Var_2006_pondéré, 3), " + ", round(slope_Var_2006_pondéré, 7), " * x"
    ),
    hjust = 0,
    vjust = 1,
    size = 4
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Comparaison des régressions linéaires (pondérée vs non pondérée)",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()

# the difference is small




     # en échelle log

Y6442010_depuis_2000$log_débit <- log10(Y6442010_depuis_2000$débit)
# Calculer la colonne log_débit
Y6442010_depuis_2006$log_débit <- log10(Y6442010_depuis_2006$débit)
# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil_log <- mean(Y6442010_depuis_2006$log_débit, na.rm = TRUE) + 
  2 * sd(Y6442010_depuis_2006$log_débit, na.rm = TRUE)
Y6442010_depuis_2006$poids <- ifelse(Y6442010_depuis_2006$log_débit > seuil_log, 0.1, 1)


# Calculer le modèle linéaire pondéré en échelle log
modele_log_pondéré <- lm(log_débit ~ date, data = Y6442010_depuis_2006, weights = poids)

# Extraire les résultats
resultats_log_pondéré <- tidy(modele_log_pondéré)
p_value_log_pondéré <- resultats_log_pondéré$p.value[2]
intercept_log_pondéré <- coef(modele_log_pondéré)[1]
slope_log_pondéré <- coef(modele_log_pondéré)[2]

# Créer les prédictions en échelle log
Y6442010_depuis_2006$log_debit_pred <- predict(modele_log_pondéré, Y6442010_depuis_2006)

# Convertir les prédictions en échelle normale
Y6442010_depuis_2006$debit_pred_log <- 10^(Y6442010_depuis_2006$log_debit_pred)

# Créer le graphique en échelle log
ggplot() +
  # Tracer les données en échelle log (utilise log_débit)
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit), color = "blue") +
  # Ajouter la ligne de régression pondérée (en échelle normale, mais le graphique est en log)
  geom_line(data = Y6442010_depuis_2006, aes(x = date, y = debit_pred_log), color = "springgreen4", linewidth = 1) +
  # Échelle log pour l'axe y
  scale_y_log10(
    name = "Débit (m³/s, log10)",
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  # Annoter l'équation et la p-value de la régression pondérée
  annotate(
    "text",
    x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y = 10^(max(log10(Y6442010_depuis_2006$débit), na.rm = TRUE) * 0.85),
    label = paste0(
      "log10(y) = ", round(intercept_log_pondéré, 3), "", round(slope_log_pondéré, 7), " * x",
      "\n", "p = ", ifelse(p_value_log_pondéré < 0.001, "< 0.001", format(p_value_log_pondéré, scientific = TRUE, digits = 3))
    ),
    hjust = 1,
    vjust = -1,
    size = 5,
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Var entre 2000 et 2026 (échelle log10)",
    x = "Date",
    y = "Débit (m³/s, log10)"
  ) +
  theme_minimal()




ggplot(data = Y6442010_depuis_2000, mapping = aes(x = date, y = débit)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  scale_y_log10(
    name = "Débit (m³/s, log)",
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotate(
    "text",
    x = min(Y6442010_depuis_2000$date, na.rm = TRUE),  # Position en haut à gauche
    y = 10^(max(log10(Y6442010_depuis_2000$débit), na.rm = TRUE) * 1),
    label = paste0(
      "log(y) = ", round(intercept, 3), " + ", round(slope, 7), " * x",
      "\n", "p = ", ifelse(p_value < 0.001, "< 0.001", format(p_value, digits = 3))
    ),
    hjust = 0,  # Alignement à gauche
    vjust = 1,  # Alignement en haut
    size = 3.5
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Var entre 2000 et 2026 (échelle log)",
    x = "Date",
    y = "Débit (m³/s, log)"
  ) +
  theme_minimal()

## Paillon runoff ----------------------------------------------------------

# Calculer le modèle de régression linéaire
modele_Paillon_2012 <- lm(ABA_debit_mean ~ date, data = Paillon_all_debit)

# Extraire les résultats avec broom::tidy()
resultats_Paillon_2012 <- tidy(modele_Paillon_2012)

# Extraire la valeur p de la pente
p_value_Paillon <- resultats_Paillon_2012$p.value[2]  # 2ème ligne = coefficient de 'date'

# Extraire les coefficients pour l'équation
intercept_Paillon_2012 <- coef(modele_Paillon_2012)[1]
slope_Paillon_2012 <- coef(modele_Paillon_2012)[2]

# graphique en échelle normale

ggplot(data = Paillon_all_debit, mapping = aes(x = date, y = ABA_debit_mean)) +
  geom_line(color = "gold") +
  # geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  # annotate(
  #   "text",
  #   x = min(Paillon_all_debit$date, na.rm = TRUE),  # Position en haut à gauche
  #   y = max(Paillon_all_debit$ABA_debit_mean, na.rm = TRUE) * 0.9,
  #   label = paste0(
  #     "y = ", round(intercept, 3), " + ", round(slope, 7), " * x",
  #     "\n", "p = ", format(p_value_Paillon, scientific = TRUE, digits = 3)
  #   ),
  #   hjust = 0, 
  #   vjust = 1, 
  #   size = 3.5
  # ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Paillon entre 2012 et 2026",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()

# on regarde si la lm du graph est significativement différente de 0

model_Paillon_2012 <- lm(Débit_moyen ~ date, data = Paillon_all_debit)
summary(model_Paillon_2012)

# en échelle log

ggplot(data = Paillon_all_debit, mapping = aes(x = date, y = ABA_debit_mean)) +
  geom_line(color = "gold") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  scale_y_log10(
    name = "Débit (m³/s, log)",
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotate(
    "text",
    x = min(Paillon_all_debit$date, na.rm = TRUE),  # Position en haut à gauche
    y = 10^(max(log10(Paillon_all_debit$ABA_debit_mean), na.rm = TRUE) * 1),
    label = paste0(
      "log(y) = ", round(intercept_Paillon_2012, 3), " + ", round(slope_Paillon_2012, 7), " * x",
      "\n", "p = ", ifelse(p_value < 0.001, "< 0.001", format(p_value, digits = 3))
    ),
    hjust = 0,  # Alignement à gauche
    vjust = 1,  # Alignement en haut
    size = 3.5
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Paillon entre 2012 et 2026",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()

## Magnan runoff -----------------------------------------------------------

# Calculer le modèle de régression linéaire
modele_Magnan_2014 <- lm(AAM_debit_mean ~ date, data = Magnan_all_debit)

# Extraire les résultats avec broom::tidy()
resultats_Magnan_2014 <- tidy(modele_Magnan_2014)

# Extraire la valeur p de la pente
p_value_Magnan <- resultats_Magnan_2014$p.value[2]  # 2ème ligne = coefficient de 'date'

# Extraire les coefficients pour l'équation
intercept_Magnan <- coef(modele_Magnan_2014)[1]
slope_Magnan <- coef(modele_Magnan_2014)[2]

ggplot(data = Magnan_all_debit, mapping = aes(x = date, y = AAM_debit_mean)) +
  geom_line(color = "violetred") +
  # geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  # annotate(
  #   "text",
  #   x = min(Magnan_all_debit$date, na.rm = TRUE),  # Position en haut à gauche
  #   y = max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) * 0.9,
  #   label = paste0(
  #     "y = ", round(intercept_Magnan, 3), " + ", round(slope_Magnan, 7), " * x",
  #     "\n", "p = ", format(p_value_Magnan, scientific = TRUE, digits = 3)
  #   ),
  #   hjust = 0, 
  #   vjust = 1, 
  #   size = 3.5
  # ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Magnan entre 2014 et 2026",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()


## plotting of rivers together ---------------------------------------------

# without an adjustment

# Créer le graphique
ggplot() +
  # Débit du Var
  geom_line(
    data = Y6442010_depuis_2000,
    aes(x = date, y = débit, color = "Var"),
    linewidth = 0.8
  ) +
  # Débit moyen du Paillon
  geom_line(
    data = Paillon_all_debit,
    aes(x = date, y = ABA_debit_mean, color = "Paillon"),
    linewidth = 0.8
  ) +
  # Débit moyen du Magnan
  geom_line(
    data = Magnan_all_debit,
    aes(x = date, y = AAM_debit_mean, color = "Magnan"),
    linewidth = 0.8
  ) +
  # Échelle de couleurs personnalisée
  scale_color_manual(
    values = c("Var" = "blue", "Paillon" = "gold", "Magnan" = "violetred"),
    name = "Fleuve"
  ) +
  # Échelle des dates
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "1 month"
  ) +
  # Échelles des axes Y (adapte selon tes données)
  scale_y_continuous(
    name = "Débit (m³/s)",
  ) +
  # Titres et labels
  labs(
    title = "Comparaison des débits du Var, du Paillon (ABA) et du Magnan (AAM) de 2012 à 2026",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  # Thème et ajustements visuels
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# plotting with an adjustment
# scaling adjustment

adjust_factors_paillon <- sec_axis_adjustement_factors(Paillon_all_debit$ABA_debit_mean, Y6442010_depuis_2000$débit)
adjust_factors_magnan <- sec_axis_adjustement_factors(Magnan_all_debit$AAM_debit_mean, Y6442010_depuis_2000$débit)

Paillon_all_debit$ABA_debit_mean_ajusté <- Paillon_all_debit$ABA_debit_mean * adjust_factors_paillon$diff + adjust_factors_paillon$adjust
Magnan_all_debit$AAM_debit_mean_ajusté <- Magnan_all_debit$AAM_debit_mean * adjust_factors_magnan$diff + adjust_factors_magnan$adjust

ggplot() +
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit, color = "Var")) +
  geom_line(data = Paillon_all_debit, aes(x = date, y = ABA_debit_mean_ajusté, color = "Paillon")) +
  geom_line(data = Magnan_all_debit, aes(x = date, y = AAM_debit_mean_ajusté, color = "Magnan")) +
  scale_color_manual(values = c("Var" = "blue", "Paillon" = "gold", "Magnan" = "violetred")) +
  scale_y_continuous(
    name = "Débit ajusté (m³/s)"
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "1 month"
  ) +
  labs(
    title = "Comparaison des débits ajustés du Var, du Paillon et du Magnan (2012-2026)",
    x = "Date",
    y = "Débit ajusté (m³/s)"
  ) +
  theme_minimal()


# plotting in log scale

Paillon_all_debit <- Paillon_all_debit %>%
  mutate(Débit_moyen = ifelse(Débit_moyen <= 0 | is.na(Débit_moyen), 0.01, Débit_moyen)) %>% 
  na.omit()

Y6442010_depuis_2000 <- Y6442010_depuis_2000 %>%
  mutate(débit_ajusté = ifelse(débit_ajusté <= 0 | is.na(débit_ajusté), 0.01, débit_ajusté))

Y6442010_depuis_2000 <- Y6442010_depuis_2000 %>%
  mutate(débit_normalisé = (débit_ajusté / max(débit_ajusté, na.rm = TRUE)) * max(Paillon_all_debit$Débit_moyen, na.rm = TRUE))

# Ajuste la transformation pour garantir des valeurs positives
transformed_values <- log10((Y6442010_depuis_2000$débit_ajusté + 0.001) / (adjust_factors$diff)) + abs(log10(abs(adjust_factors$adjust) + 0.001))

ggplot() +
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit, color = "Var")) +
  geom_line(data = Paillon_all_debit, aes(x = date, y = Débit_moyen, color = "Paillon")) +
  scale_color_manual(values = c("Var" = "blue", "Paillon" = "gold")) +
  scale_y_log10(
    name = "Débit (m³/s, log)",
    breaks = scales::breaks_log(n = 10),
  ) +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "1 month"
  ) +
  labs(
    title = "Comparaison entre le débit du Var et du Paillon (2000-2026, échelle log)",
    x = "Date"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Var decomposition -------------------------------------------------------

Y6442010_complet <- Y6442010_depuis_2000  |> 
  group_by(year = lubridate::year(date))  |> 
  filter(!is.na(débit) > 0.9)  |>
  ungroup()

Y6442010_complet <- Y6442010_complet %>%
  filter(!is.na(débit))

ts_data <- ts(Y6442010_complet$débit, frequency = 365)

decomp <- stl(ts_data, s.window = "periodic")
plot(decomp)

tendance <- data.frame(
  date = Y6442010_complet$date,  # Utilise les dates originales
  valeur = as.numeric(decomp$time.series[, "trend"])
)

model_lm <- lm(valeur ~ date, data = tendance)
summary(model_lm)

# Paillon decomposition -------------------------------------------------------

Paillon_complet <- Paillon_all_debit  |> 
  group_by(year = lubridate::year(date))  |> 
  filter(mean(!is.na(débit)) > 0.9)  |> 
  ungroup()

Y6442010_complet <- Y6442010_complet %>%
  filter(!is.na(débit))

ts_data <- ts(Y6442010_complet$débit, frequency = 365)

decomp <- stl(ts_data, s.window = "periodic")
plot(decomp)

tendance <- data.frame(
  date = Y6442010_complet$date,  # Utilise les dates originales
  valeur = as.numeric(decomp$time.series[, "trend"])
)

model_lm <- lm(valeur ~ date, data = tendance)
summary(model_lm)


# Flood analysis ----------------------------------------------------------

Var_crues <- Y6442010_complet %>%
  filter(débit > 200)
