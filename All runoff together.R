# All runoff together

# pathway : "~/River_runoff_analysis/All runoff together"

# This script will load river runoff data at the mouth of the river and combine 
# them in one data frame. After, it will plot results

# libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

# load data ---------------------------------------------------------------

load("data/Hydro France/Y6442010_depuis_2000.Rdata")

load("data/Hydro France/Y6442010_depuis_2014.Rdata")

load("data/MNCA/Paillon_all_debit.Rdata")

load("data/MNCA/Magnan_all_debit.Rdata")

load("data/All_debit.Rdata")

# Complete missing dates in the date range
Y6442010_depuis_2000 <- Y6442010_depuis_2000 |> 
  complete(date = seq(min(date), max(date), by = "day"))

Paillon_all_debit <- Paillon_all_debit |> 
  complete(date = seq(min(date), max(date), by = "day"))

Magnan_all_debit <- Magnan_all_debit |> 
  complete(date = seq(min(date), max(date), by = "day"))

# combine data ------------------------------------------------------------

All_debit <- Y6442010_depuis_2000 %>%
  mutate(date = as.Date(date)) %>%
  select(date, débit) %>% 
  full_join(
    Paillon_all_debit %>% 
      mutate(date = as.Date(date)) %>%
      select(date, ABA_debit_mean),
    by = "date"
  ) %>%
  full_join(
    Magnan_all_debit %>% 
      mutate(date = as.Date(date)) %>%
      distinct(date, .keep_all = TRUE) %>%  # supprime les doublons
      select(date, AAM_debit_mean),
    by = "date"
  ) %>%
  filter(date >= as.Date("2014-01-01")) %>%
  mutate(
    debit_cumule = débit + ABA_debit_mean + AAM_debit_mean
  )

# create a df where the maximum is 500 set a maximum
All_debit_max_500 <- All_debit |> 
  mutate(across(where(is.numeric), ~ pmin(., 500)))

# create a df for the period 2018 / 2019
All_debit_2018_2019 <- All_debit |> 
  filter(date >= as.Date("2018-01-01"), date <= as.Date("2019-01-01"))

# create a df for the period 2015 / 2017
All_debit_log <- All_debit |> 
  mutate(across(where(is.numeric), ~ log(.)))

# mean liquid flow --------------------------------------------------------

# calculate the mean and max flow rate of the Var river
# la moyenne étant sensible aux valeurs extrêmes on calcule un seuil au dessus duquel
# les valeurs sont pondérées
# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil <- mean(Y6442010_depuis_2000$débit, na.rm = TRUE) + 2 * sd(Y6442010_depuis_2000$débit, na.rm = TRUE)
# seuil = 140
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
Y6442010_depuis_2000$poids <- ifelse(Y6442010_depuis_2000$débit > seuil, 0.1, 1)
# calculer la moyenne pondérée
mean_debit_Var_pondérée <- weighted.mean(Y6442010_depuis_2000$débit, w = Y6442010_depuis_2000$poids, 
                                         na.rm = TRUE)
print(mean_debit_Var_pondérée) # 41.94608
max_debit_Var <- max(Y6442010_depuis_2000$débit, na.rm = TRUE)
print(max_debit_Var) # 962



# calculate the mean and max flow rate of the Paillon river
# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil <- mean(Paillon_all_debit$ABA_debit_mean, na.rm = TRUE) + 2 * sd(Paillon_all_debit$ABA_debit_mean, na.rm = TRUE)
# seuil = 85
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
Paillon_all_debit$poids <- ifelse(Paillon_all_debit$ABA_debit_mean > seuil, 0.1, 1)

mean_debit_Paillon_pondérée <- weighted.mean(Paillon_all_debit$ABA_debit_mean, w = Paillon_all_debit$poids, 
                                    na.rm = TRUE)
print(mean_debit_Paillon_pondérée) # 18.60933
max_debit_Paillon <- max(Paillon_all_debit$ABA_debit_mean, na.rm = TRUE)
print(max_debit_Paillon) # 477.0833



# calculate the mean and max flow rate of the Magnan river
# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil <- mean(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) + 2 * sd(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE)
# seuil = 3.8
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
Magnan_all_debit$poids <- ifelse(Magnan_all_debit$AAM_debit_mean > seuil, 0.1, 1)
mean_debit_Magnan_pondérée <- weighted.mean(Magnan_all_debit$AAM_debit_mean, w = Magnan_all_debit$poids,
                                            na.rm = TRUE)
print(mean_debit_Magnan_pondérée) # 0.1490904
max_debit_Magnan <- max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE)
print(max_debit_Magnan) # 63.45656

# plotting ----------------------------------------------------------------

# tous les débit ensemble
ggplot() +
  geom_point(data = Y6442010_depuis_2014, aes(x = date, y = débit, color = "Var"),  size = 0.5) +
  geom_point(data = Paillon_all_debit, aes(x = date, y = ABA_debit_mean, color = "Paillon"), size = 0.5) +
  geom_point(data = Magnan_all_debit, aes(x = date, y = AAM_debit_mean, color = "Magnan"), size = 0.5) +
  geom_point(data = All_debit, aes(x = date, y = debit_cumule, color = "débit cumulé"), size = 0.5)


# juste le débit cumulé

seuil <- mean(All_debit$debit_cumule, na.rm = TRUE) + 2 * sd(All_debit$debit_cumule, na.rm = TRUE)
# 173
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
All_debit$poids <- ifelse(All_debit$debit_cumule > seuil, 0.1, 1)

# Calculer le modèle linéaire pondéré
modele_cumulé_pondéré <- lm(debit_cumule ~ date, data = All_debit, weights = poids)

# Extraire les résultats
resultats_cumulé_pondéré <- tidy(modele_cumulé_pondéré)
p_value_cumulé_pondéré <- resultats_cumulé_pondéré$p.value[2]
intercept_cumulé_pondéré <- coef(modele_cumulé_pondéré)[1]
slope_cumulé_pondéré <- coef(modele_cumulé_pondéré)[2]

# Ajouter les prédictions au data.frame
All_debit$débit_pred_pondéré <- predict(modele_cumulé_pondéré, All_debit)

# ggplot du débit cumulé seulement
ggplot() +
  geom_line(data = All_debit, aes(x = date, y = debit_cumule),
            color = "darkolivegreen3",
            linewidth = 0.5,
            alpha     = 0.8) +
  geom_line(
    data  = All_debit,
    aes(x = date, y = débit_pred_pondéré),
    color     = "#1A7A4A",
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = min(All_debit$date),
    xend = min(All_debit$date),
    y = 0,
    yend = max(All_debit$debit_cumule, na.rm = TRUE),
    color     = "grey50",
    linewidth = 0.4,
    linetype  = "dashed"
  ) +
  annotate(
    "text",
    x     = min(All_debit$date, na.rm = TRUE),
    y     = max(All_debit$debit_cumule, na.rm = TRUE) * 0.90,
    label = paste0("y = ", round(intercept_cumulé_pondéré, 3), " ", 
                   round(slope_cumulé_pondéré, 7), " × x"),
    hjust = -2, vjust = 1,
    size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  annotate(
    "text",
    x     = min(All_debit$date, na.rm = TRUE),
    y     = max(All_debit$debit_cumule, na.rm = TRUE) * 0.82,
    label = paste0("p ", ifelse(p_value_cumulé_pondéré < 0.001, "< 0.001",
                                format(p_value_cumulé_pondéré, scientific = TRUE, digits = 3))),
    hjust = -7, vjust = 1,
    size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier cumulé du Var, du Paillon, du Magnan (2014–2026)",
    subtitle = "Tendance linéaire pondérée estimée depuis 2014",
    x        = NULL,
    y        = "Débit (m³. s⁻¹)",
    caption  = "Source : Banque HydroFrance —  Métropôle Nice Côte d'Azur"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 10,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# on veut faire une décomposition STL pour voir la saisonnalité, la tendance et 
# le reminder

### STL decomposition --------------------------------------------------

# on va utiliser une série temporelle entre 2014 et 2020 car il y a un trop gros 
# trous dans les données
All_debit_2014_2026 <- All_debit |> 
  filter(date >= as.Date("2014-01-01"), date <= as.Date("2026-03-05"))

# 1. Nettoyage initial
All_debit_2014_2026_clean <- All_debit_2014_2026 |>
  filter(!is.na(debit_cumule)) |>
  arrange(date)

# 2. Créer une séquence de dates complète et joindre
All_debit_2014_2026_complete <- data.frame(
  date = seq(min(All_debit_2014_2026_clean$date),
             max(All_debit_2014_2026_clean$date),
             by = "day")
) |>
  # Left join pour créer des NA là où les dates manquent
  left_join(All_debit_2014_2026_clean, by = "date")

# 3. Vérifier que les NA sont bien créés
cat("Lignes totales    :", nrow(All_debit_2014_2026_complete), "\n")
cat("NA après jointure :", sum(is.na(All_debit_2014_2026_complete$debit_cumule)), "\n")

# 4. interpolation nécessaire pour combler les 783 jours
All_debit_2014_2026_complete <- All_debit_2014_2026_complete |>
  mutate(
    # débit = zoo::na.spline(débit, na.rm = FALSE),
    # débit = pmax(débit, 0)
    # Alternative plus conservative :
    debit_cumule = zoo::na.approx(debit_cumule, na.rm = FALSE)  # linéaire
  )

# Vérifier les NA restants — souvent en début/fin de série
cat("NA restants :", sum(is.na(All_debit_2014_2026_complete$debit_cumule)), "\n")

# 3. Série temporelle et décomposition STL
ts_data <- ts(All_debit_2014_2026_complete$debit_cumule, frequency = 365)

decomp <- stl(
  ts_data,
  s.window = "periodic",  # saisonnalité stable dans le temps
  robust   = TRUE          # robuste aux valeurs extrêmes (crues)
)

# 4. Extraire les composantes
# Recréer tendance avec toutes les composantes STL
tendance <- data.frame(
  date      = All_debit_2014_2026_complete$date,
  valeur    = All_debit_2014_2026_complete$debit_cumule,         
  trend     = as.numeric(decomp$time.series[, "trend"]),
  seasonal  = as.numeric(decomp$time.series[, "seasonal"]),
  remainder = as.numeric(decomp$time.series[, "remainder"])
)

# 5. Modèle linéaire sur la tendance
model_lm <- lm(trend ~ date, data = tendance)
summary(model_lm)

slope     <- coef(model_lm)[2]
intercept <- coef(model_lm)[1]
p_value   <- summary(model_lm)$coefficients[2, 4]

cat("Pente :", round(slope * 365, 3), "m³/s par an\n")
# Pente : -4.668 m³/s par an
cat("p-value :", p_value, "\n")
# p-value : 0


# Couleurs
col_debit   <- "steelblue"
col_trend   <- "darkred"
col_season  <- "darkorange"
col_residu  <- "grey50"

# Panel 1 — Débit brut + tendance
p1 <- ggplot(tendance, aes(x = date)) +
  geom_line(aes(y = valeur), color = col_debit, alpha = 0.5, linewidth = 0.4) +
  geom_line(aes(y = trend), color = col_trend, linewidth = 1.2) +
  geom_smooth(aes(y = trend), method = "lm", 
              color = "black", linetype = "dashed", 
              se = TRUE, linewidth = 0.8) +
  annotate(
    "text",
    x     = max(tendance$date),
    y     = max(tendance$valeur, na.rm = TRUE),
    hjust = 1, vjust = 1,
    label = paste0(
      "Pente = ", round(slope * 365, 2), "m³/s/an",
      "\np ", ifelse(p_value < 0.001, "< 0.001", round(p_value, 3))
    ),
    size = 8, fontface = "italic", color = "grey20"
  ) +
  labs(
    title = "Décomposition STL du débit journalier du Var, du Paillon et du Magnan (2014–2026)",
    y     = expression("Débit (m"^3*".s"^-1*")"),
    x     = NULL
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 14, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )


# Panel 2 — Composante saisonnière
p2 <- ggplot(tendance, aes(x = date, y = seasonal)) +
  geom_line(color = col_season, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    y = expression("Saisonnalité (m"^3*".s"^-1*")"),
    x = NULL
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 14, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# Panel 3 — Résidus
p3 <- ggplot(tendance, aes(x = date, y = remainder)) +
  geom_line(color = col_residu, linewidth = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    y = expression("Résidus (m"^3*".s"^-1*")"),
    x = "Date"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 14, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# Assembler avec patchwork
p1 / p2 / p3 +
  plot_layout(heights = c(3, 1.5, 1.5)) &
  theme(
    axis.text      = element_text(size = 10, color = "grey30"),
    axis.title     = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )



# les débits ensembles mais non ajustés

ggplot() +
  geom_line(
    data  = All_debit,
    aes(x = date, y = débit,
    color     = "Var"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit,
    aes(x = date, y = ABA_debit_mean,
    color     = "Paillon"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit,
    aes(x = date, y = AAM_debit_mean,
    color     = "Magnan"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit,
    aes(x = date, y = debit_cumule,
    color     = "Débit cumulé"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Var"          = "blue",
      "Paillon"      = "gold",
      "Magnan"       = "violetred",
      "Débit cumulé" = "darkolivegreen3"
    ),
    guide = guide_legend(override.aes = list(linewidth = 1.2))  # traits plus épais dans la légende
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Var, du Paillon, du Magnan et leur débit cumulé (2014–2026)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance, Métropôle Nice Côte d'Azur"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# plot the df where the maximum is set to 500
ggplot() +
  geom_line(
    data  = All_debit_max_500,
    aes(x = date, y = débit,
        color     = "Var"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_max_500,
    aes(x = date, y = ABA_debit_mean,
        color     = "Paillon"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_max_500,
    aes(x = date, y = AAM_debit_mean,
        color     = "Magnan"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_max_500,
    aes(x = date, y = debit_cumule,
        color     = "Débit cumulé"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Var"          = "blue",
      "Paillon"      = "gold",
      "Magnan"       = "violetred",
      "Débit cumulé" = "darkolivegreen3"
    ),
    guide = guide_legend(override.aes = list(linewidth = 1.2))  # traits plus épais dans la légende
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Var, du Paillon, du Magnan et leur débit cumulé (2014–2026)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance, Métropôle Nice Côte d'Azur"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# plot for the period 2018 / 2019
ggplot() +
  geom_line(
    data  = All_debit_2018_2019,
    aes(x = date, y = débit,
        color     = "Var"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_2018_2019,
    aes(x = date, y = ABA_debit_mean,
        color     = "Paillon"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_2018_2019,
    aes(x = date, y = AAM_debit_mean,
        color     = "Magnan"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_2018_2019,
    aes(x = date, y = debit_cumule,
        color     = "Débit cumulé"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Var"          = "blue",
      "Paillon"      = "gold",
      "Magnan"       = "violetred",
      "Débit cumulé" = "darkolivegreen3"
    ),
    guide = guide_legend(override.aes = list(linewidth = 1.2))  # traits plus épais dans la légende
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Var, du Paillon, du Magnan et leur débit cumulé (2018–2019)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance, Métropôle Nice Côte d'Azur"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# plot for the period 2015 / 2017
ggplot() +
  geom_line(
    data  = All_debit_2015_2017,
    aes(x = date, y = débit,
        color     = "Var"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_2015_2017,
    aes(x = date, y = ABA_debit_mean,
        color     = "Paillon"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_2015_2017,
    aes(x = date, y = AAM_debit_mean,
        color     = "Magnan"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_2015_2017,
    aes(x = date, y = debit_cumule,
        color     = "Débit cumulé"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Var"          = "blue",
      "Paillon"      = "gold",
      "Magnan"       = "violetred",
      "Débit cumulé" = "darkolivegreen3"
    ),
    guide = guide_legend(override.aes = list(linewidth = 1.2))  # traits plus épais dans la légende
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Var, du Paillon, du Magnan et leur débit cumulé (2015–2017)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance, Métropôle Nice Côte d'Azur"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )

# plot in log scale
ggplot() +
  geom_line(
    data  = All_debit_log,
    aes(x = date, y = débit,
        color     = "Var"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_log,
    aes(x = date, y = ABA_debit_mean,
        color     = "Paillon"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_log,
    aes(x = date, y = AAM_debit_mean,
        color     = "Magnan"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = All_debit_log,
    aes(x = date, y = debit_cumule,
        color     = "Débit cumulé"),
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Var"          = "blue",
      "Paillon"      = "gold",
      "Magnan"       = "violetred",
      "Débit cumulé" = "darkolivegreen3"
    ),
    guide = guide_legend(override.aes = list(linewidth = 1.2))  # traits plus épais dans la légende
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Var, du Paillon, du Magnan et leur débit cumulé en échelle log (2015–2026)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance, Métropôle Nice Côte d'Azur"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  ) +
  scale_y_log10(
    breaks = c(0.1, 1, 10, 100, 500),
    labels = scales::comma
  ) +
  annotation_logticks(sides = "l")  # petits tirets log sur l'axe Y



