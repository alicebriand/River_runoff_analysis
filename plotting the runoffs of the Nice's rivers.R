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
library(lubridate)
library(seasonal)
library(WaveletComp)
library(patchwork)

# loading -----------------------------------------------------------------

load("data/Hydro France/Y6442010_depuis_2000.Rdata")
load("data/Hydro France/Y6442010_depuis_2006.Rdata")
load("data/MNCA/Paillon_all_debit.Rdata")
load("data/MNCA/Magnan_all_debit.Rdata")
load("data/Hydro France/Y6442010_depuis_2013.Rdata")

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
  geom_line(
    data  = Y6442010_depuis_2000,
    aes(x = date, y = débit),
    color     = "blue",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_smooth(
    data   = Y6442010_depuis_2006,
    aes(x = date, y = débit),
    method = "lm", se = TRUE,
    color  = NA, fill = "#E74C3C", alpha = 0.12
  ) +
  geom_line(
    data  = Y6442010_depuis_2006,
    aes(x = date, y = débit_pred),
    color     = "#C0392B",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.9,
    label = paste0("y = ", round(intercept_Var_2006, 3), " ", round(slope_Var_2006, 7), " × x"),
    hjust = -1.5, vjust = 1,
    size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate(
    "text",
    x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.82,  # légèrement en dessous
    label = paste0("p ", ifelse(p_value_Var_2006 < 0.001, "< 0.001",
                                  format(p_value_Var_2006, scientific = TRUE, digits = 3))),
    hjust = -4, vjust = 1,
    size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Var (2000–2026)",
    subtitle = "Tendance linéaire estimée depuis 2006",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance - station Pont Napoléon"
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
  geom_line(
    data  = Y6442010_depuis_2000,
    aes(x = date, y = débit),
    color     = "blue",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = Y6442010_depuis_2006,
    aes(x = date, y = débit_pred_pondéré),
    color     = "#1A7A4A",
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = min(Y6442010_depuis_2006$date),
    xend = min(Y6442010_depuis_2006$date),
    y = 0,
    yend = max(Y6442010_depuis_2006$débit, na.rm = TRUE),
    color     = "grey50",
    linewidth = 0.4,
    linetype  = "dashed"
  ) +
  annotate(
    "text",
    x     = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y     = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.90,
    label = paste0("y = ", round(intercept_Var_2006_pondéré, 3), " ", 
                   round(slope_Var_2006_pondéré, 7), " × x"),
    hjust = -1.6, vjust = 1,
    size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  annotate(
    "text",
    x     = min(Y6442010_depuis_2006$date, na.rm = TRUE),
    y     = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.82,
    label = paste0("p ", ifelse(p_value_Var_2006_pondéré < 0.001, "< 0.001",
                                  format(p_value_Var_2006_pondéré, scientific = TRUE, digits = 3))),
    hjust = -5, vjust = 1,
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
    title    = "Débit journalier du Var (2000–2026)",
    subtitle = "Tendance linéaire pondérée estimée depuis 2006",
    x        = NULL,
    y        = "Débit (m³. s⁻¹)",
    caption  = "Source : Banque HydroFrance — station Pont Napoléon"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 12,  color = "grey50", hjust = 0),
    axis.title.y  = element_text(size = 14, margin = margin(r = 10)),
    axis.text     = element_text(size = 10, color = "grey30"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.ticks    = element_line(color = "grey70"),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey96", linewidth = 0.2),
    panel.border  = element_rect(color = "grey70", linewidth = 0.5)
  )


# compare the linear model and the weighted linear model

ggplot() +
  geom_line(
    data  = Y6442010_depuis_2000,
    aes(x = date, y = débit),
    color     = "blue",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = Y6442010_depuis_2006,
    aes(x = date, y = débit_pred),
    color     = "#C0392B",
    linewidth = 0.8
  ) +
  geom_line(
    data  = Y6442010_depuis_2006,
    aes(x = date, y = débit_pred_pondéré),
    color     = "#1A7A4A",
    linewidth = 0.8
  ) +
  annotate("text",
           x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
           y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.90,
           label = paste0("Non pondéré : y = ", round(intercept_Var_2006, 3),
                          " + ", round(slope_Var_2006, 7), " × x"),
           hjust = 0.53, vjust = 1,
           size = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
           y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.84,
           label = paste0("p = ", ifelse(p_value_Var_2006 < 0.001, "< 0.001",
                                         format(p_value_Var_2006, scientific = TRUE, digits = 3))),
           hjust = 1.9, vjust = 1,
           size = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
           y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.75,
           label = paste0("Pondéré : y = ", round(intercept_Var_2006_pondéré, 3),
                          " + ", round(slope_Var_2006_pondéré, 7), " × x"),
           hjust = 0.58, vjust = 1,
           size = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x = min(Y6442010_depuis_2006$date, na.rm = TRUE),
           y = max(Y6442010_depuis_2006$débit, na.rm = TRUE) * 0.69,
           label = paste0("p = ", ifelse(p_value_Var_2006_pondéré < 0.001, "< 0.001",
                                         format(p_value_Var_2006_pondéré, scientific = TRUE, digits = 3))),
           hjust = 1.9, vjust = 1,
           size = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Comparaison des régressions linéaires du débit du Var (2000–2026)",
    subtitle = "Régression non pondérée (rouge) vs pondérée (vert) — estimées depuis 2006",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance — station Pont Napoléon"
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



# the difference is small




     # en échelle log
# 
# Y6442010_depuis_2000$log_débit <- log10(Y6442010_depuis_2000$débit)
# # Calculer la colonne log_débit
# Y6442010_depuis_2006$log_débit <- log10(Y6442010_depuis_2006$débit)
# # Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
# seuil_log <- mean(Y6442010_depuis_2006$log_débit, na.rm = TRUE) + 
#   2 * sd(Y6442010_depuis_2006$log_débit, na.rm = TRUE)
# Y6442010_depuis_2006$poids <- ifelse(Y6442010_depuis_2006$log_débit > seuil_log, 0.1, 1)

# Modèle non pondéré (supprimer weights)
modele_log <- lm(log_débit ~ date, data = Y6442010_depuis_2006)

# Extraire les résultats
resultats_log <- tidy(modele_log)
p_value_log   <- resultats_log$p.value[2]
intercept_log <- coef(modele_log)[1]
slope_log     <- coef(modele_log)[2]

# Prédictions
Y6442010_depuis_2006$log_debit_pred_np <- predict(modele_log, Y6442010_depuis_2006)
Y6442010_depuis_2006$debit_pred_log_np <- 10^(Y6442010_depuis_2006$log_debit_pred_np)

# Calculer le modèle linéaire pondéré en échelle log
# modele_log_pondéré <- lm(log_débit ~ date, data = Y6442010_depuis_2006, weights = poids)
# 
# # Extraire les résultats
# resultats_log_pondéré <- tidy(modele_log_pondéré)
# p_value_log_pondéré <- resultats_log_pondéré$p.value[2]
# intercept_log_pondéré <- coef(modele_log_pondéré)[1]
# slope_log_pondéré <- coef(modele_log_pondéré)[2]
# 
# # Créer les prédictions en échelle log
# Y6442010_depuis_2006$log_debit_pred <- predict(modele_log_pondéré, Y6442010_depuis_2006)
# 
# # Convertir les prédictions en échelle normale
# Y6442010_depuis_2006$debit_pred_log <- 10^(Y6442010_depuis_2006$log_debit_pred)

# Créer le graphique en échelle log


ggplot() +
  geom_line(
    data  = Y6442010_depuis_2000,
    aes(x = date, y = débit),
    color     = "blue",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    data  = Y6442010_depuis_2006,
    aes(x = date, y = debit_pred_log_np),  # _np au lieu de debit_pred_log
    color     = "#C0392B",  # rouge pour distinguer du pondéré
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = min(Y6442010_depuis_2006$date),
    xend = min(Y6442010_depuis_2006$date),
    y = min(Y6442010_depuis_2000$débit, na.rm = TRUE),
    yend = max(Y6442010_depuis_2000$débit, na.rm = TRUE),
    color     = "grey50",
    linewidth = 0.4,
    linetype  = "dashed"
  ) +
  annotate("text",
           x     = min(Y6442010_depuis_2006$date, na.rm = TRUE),
           y     = 10^(max(log10(Y6442010_depuis_2006$débit), na.rm = TRUE) * 0.97),
           label = paste0("log10(y) = ", round(intercept_log, 3),
                          " ", round(slope_log, 7), " × x"),
           color = "#C0392B",
           hjust = 0.58, vjust = 1,
           size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x     = min(Y6442010_depuis_2006$date, na.rm = TRUE),
           y     = 10^(max(log10(Y6442010_depuis_2006$débit), na.rm = TRUE) * 0.93),
           label = paste0("p = ", ifelse(p_value_log < 0.001, "< 0.001",
                                         format(p_value_log, scientific = TRUE, digits = 3))),
           color = "#C0392B",
           hjust = 1.5, vjust = 1,
           size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  labs(
    title    = "Débit journalier du Var (2000–2026, échelle log)",
    subtitle = "Régression linéaire non pondérée en échelle log estimée depuis 2006",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Banque HydroFrance — station Pont Napoléon"
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



## Var TS decomposition ----------------------------------------------------

# use a complete data set
Y6442010_2006_2026 <- Y6442010_depuis_2000 |> 
  filter(date >= "2005-12-31", date < "2026-03-05")

### Fourrier decomposition --------------------------------------------------

#### Robert Website ----------------------------------------------------------

# filter NA
signal <- Y6442010_2006_2026$débit[!is.na(Y6442010_2006_2026$débit)]

fft_result <- fft(signal)
n <- length(signal)

print(fft_result)

# Visualization
par(mfrow = c(2, 1))
plot(signal, type = "l", main = "Original Signal")
plot(Mod(fft_result), type = "l", main = "FFT Magnitudes")


# identify the position of the highest frequency value
magnitudes<- abs(fft_result)  ## or mod()

# Find the frequency with the largest amplitude
max_index_sine <- which.max(magnitudes)

paste("magnitude value: ", magnitudes[max_index_sine] )

paste("index position: ",max_index_sine)


#### Claude ------------------------------------------------------

fft_result <- fft(signal)
n <- length(signal)

# Fréquences en cycles/jour
freq <- (0:(n/2)) / n

# Périodes en jours
periode <- 1 / freq[-1]  # on enlève la fréquence 0 (composante continue)

# Amplitude
amplitude <- Mod(fft_result)[2:(n/2 + 1)]

# Graphique
plot(periode, amplitude, type = "l",
     xlim = c(0, 400),  # zoom sur les périodes < 400 jours
     xlab = "Période (jours)",
     ylab = "Amplitude",
     main = "Spectre FFT du débit du Var (2008 - 2020)")

# Marquer le cycle annuel
abline(v = 365, col = "red", lty = 2)


### STL decomposition --------------------------------------------------

# 1. Nettoyage initial
Y6442010_2006_2026_clean <- Y6442010_2006_2026 |>
  filter(!is.na(débit)) |>
  arrange(date)

# 2. Créer une séquence de dates complète et joindre
Y6442010_2006_2026_complete <- data.frame(
  date = seq(min(Y6442010_2006_2026_clean$date),
             max(Y6442010_2006_2026_clean$date),
             by = "day")
) |>
  # Left join pour créer des NA là où les dates manquent
  left_join(Y6442010_2006_2026_clean, by = "date")

# 3. Vérifier que les NA sont bien créés
cat("Lignes totales    :", nrow(Y6442010_2006_2026_complete), "\n")
cat("NA après jointure :", sum(is.na(Y6442010_2006_2026_complete$débit)), "\n")

# 4. interpolation nécessaire pour combler les 945 jours
Y6442010_2006_2026_complete <- Y6442010_2006_2026_complete |>
  mutate(
    # débit = zoo::na.spline(débit, na.rm = FALSE),
    # débit = pmax(débit, 0)
    # Alternative plus conservative :
    débit = zoo::na.approx(débit, na.rm = FALSE)  # linéaire
  )

# 3. Série temporelle et décomposition STL
ts_data <- ts(Y6442010_2006_2026_complete$débit, frequency = 365)

decomp <- stl(
  ts_data,
  s.window = "periodic",  # saisonnalité stable dans le temps
  robust   = TRUE          # robuste aux valeurs extrêmes (crues)
)

# 4. Extraire les composantes
# Recréer tendance avec toutes les composantes STL
tendance <- data.frame(
  date      = Y6442010_2006_2026_complete$date,
  valeur    = Y6442010_2006_2026_complete$débit,         
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
# Pente : -0.59 m³/s par an
cat("p-value :", p_value, "\n")
# p-value : 1.723787e-165


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
    title = "Décomposition STL du débit journalier du Var (2006–2026)",
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

# maintenant on aimerais savoir si les crues ont augmenté ou pas dans les dernières années
# ces crues sont stockées dans les résidus comme événements aberrants

# on définit tout d'abord un seuil à ne pas dépasser
# ce seuil est défini avec le centile 95

# threshold <- quantile(tendance$remainder, 0.95, na.rm = TRUE) # de 57.3
# étant donné que le débit moyen du Var est de 42 m3/s, je trouve ce seuil très bas
threshold <- 200

# on définit les crues comme étant les événements supérieures à ce seuil
crues <- tendance$remainder > threshold

dates <- tendance$date

dates_crues <- dates[crues]

# on compte les crues par années
df <- data.frame(
  date = dates,
  crue = crues
)

freq_annuelle <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(nb_crues = sum(crue, na.rm = TRUE))

plot(freq_annuelle$year,
     freq_annuelle$nb_crues,
     type = "b")

model_lm <- lm(nb_crues ~ year, data = freq_annuelle)
summary(model_lm)

slope     <- coef(model_lm)[2]
intercept <- coef(model_lm)[1]
p_value   <- summary(model_lm)$coefficients[2, 4]

# Ajouter les prédictions au data.frame
freq_annuelle$débit_pred <- predict(model_lm, freq_annuelle)

ggplot() +
  geom_line(data = freq_annuelle, aes(x = year, y = nb_crues)) +
  # geom_smooth(aes(y = nb_crues), method = "lm", 
  #             color = "black", linetype = "dashed", 
  #             se = TRUE, linewidth = 0.8) +
  geom_line(
    data  = freq_annuelle,
    aes(x = year, y = débit_pred),
    color     = "red",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x     = max(freq_annuelle$year),
    y     = max(freq_annuelle$nb_crues, na.rm = TRUE),
    hjust = 1, vjust = 1,
    label = paste0(
      "Pente = ", round(slope * 365, 2), " crues",
      "\np = ", ifelse(p_value < 0.001, "< 0.001", round(p_value, 3))
    ),
    size = 8, fontface = "italic", color = "grey20", family = "serif"
  ) +
  labs(
    title = "Évolution du nombre de crues par années (2005 - 2026)",
    y     = "nombre de crues",
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


### X11 decomposition --------------------------------------------------

# X11 nécessite une fréquence mensuelle ou trimestrielle
# Si tu as des données journalières, agrège d'abord par mois

debit_mensuel <- Y6442010_depuis_2008 |>
  mutate(mois = floor_date(date, "month")) |>
  group_by(mois) |>
  summarise(debit_mean = mean(débit, na.rm = TRUE)) |>
  filter(!is.na(debit_mean))

# Convertir en ts mensuel
debit_ts_mensuel <- ts(debit_mensuel$debit_mean, 
                       start     = c(2008, 1), 
                       frequency = 12)

# X11 decomposition
x11_result <- seas(debit_ts_mensuel, x11 = "")
plot(x11_result)

# Extraire les composantes
trend     <- trend(x11_result)
seasonal  <- seasonal(x11_result)
remainder <- irregular(x11_result)







# on choisit une période remplie donc entre 2008 et 2019

Y6442010_2008_2019 <- Y6442010_depuis_2000 |> 
  filter(date >= as.Date("2008-01-01"), date <= as.Date("2019-12-31"))

sum(is.na(Y6442010_2008_2019))

# Localiser et caractériser les trous
Y6442010_2008_2019 |>
  mutate(est_na = is.na(débit)) |>
  filter(est_na) |>
  mutate(
    groupe = cumsum(c(1, diff(as.numeric(date)) > 1))
  ) |>
  group_by(groupe) |>
  summarise(
    debut     = min(date),
    fin       = max(date),
    n_jours   = n()
  ) |>
  arrange(debut)

# interpolation
library(zoo)

Y6442010_2008_2019 <- Y6442010_2008_2019 |>
  arrange(date) |>
  mutate(
    debit_interp = na.approx(débit, x = date, na.rm = FALSE)
  )

# Vérifier qu'il ne reste plus de NA
sum(is.na(Y6442010_2008_2019$debit_interp))

# Visualiser pour vérifier que l'interpolation est cohérente
Y6442010_2008_2019 |>
  mutate(est_interpole = is.na(débit) & !is.na(debit_interp)) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = debit_interp), color = "steelblue", linewidth = 0.5) +
  geom_point(
    data = ~ filter(.x, est_interpole),
    aes(y = debit_interp),
    color = "red", size = 1.5
  ) +
  labs(
    title    = "Débit du Var interpolé — 2008–2019",
    subtitle = "Points rouges = valeurs interpolées",
    x        = NULL,
    y        = "Débit (m³/s)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

library(seasonal)
library(tidyverse)

# Agréger en mensuel
debit_mensuel <- Y6442010_2008_2019 |>
  mutate(mois = floor_date(date, "month")) |>
  group_by(mois) |>
  summarise(debit = mean(debit_interp, na.rm = TRUE))

# Vérifier qu'il n'y a plus de NA
sum(is.na(debit_mensuel$debit))

# Créer la série temporelle sur la colonne débit uniquement
debit_ts <- ts(
  data      = debit_mensuel$debit,  # ← juste la colonne
  start     = c(2008, 1),
  frequency = 12
)

# Appliquer X11
x11_result <- seas(debit_ts, x11 = "")

# 3. Inspecter les résultats
summary(x11_result)

# 4. Extraire les composantes
composantes <- data.frame(
  date        = debit_mensuel$mois,
  observed    = as.numeric(original(x11_result)),    # signal brut
  tendance    = as.numeric(trend(x11_result)),        # tendance lissée
  saisonnalite = as.numeric(seasonal(x11_result)),   # composante saisonnière
  residus     = as.numeric(irregular(x11_result))    # résidus
)

# 5. Visualiser
composantes_long <- composantes |>
  pivot_longer(-date, names_to = "composante", values_to = "valeur") |>
  mutate(composante = factor(composante,
                             levels = c("observed", "tendance", "saisonnalite", "residus")))
library(seasonal)
library(patchwork)

# Extraire les composantes
composantes <- data.frame(
  date         = debit_mensuel$mois,
  observed     = as.numeric(original(x11_result)),
  tendance     = as.numeric(trend(x11_result)),
  saisonnalite = as.numeric(seasonal(x11_result)),
  residus      = as.numeric(irregular(x11_result))
)

# Graphique 1 — Signal brut + tendance
p1 <- ggplot(composantes, aes(x = date)) +
  geom_line(aes(y = observed, color = "Signal brut"), linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = tendance, color = "Tendance"), linewidth = 1.1) +
  scale_color_manual(values = c("Signal brut" = "steelblue", "Tendance" = "firebrick")) +
  labs(title = "a) Signal observé et tendance", x = NULL, y = "Débit (m³/s)", color = NULL) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  )

# Graphique 2 — Saisonnalité
p2 <- ggplot(composantes, aes(x = date, y = saisonnalite)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pmin(saisonnalite, 0), ymax = 0), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(saisonnalite, 0)), fill = "chartreuse4", alpha = 0.3) +
  geom_line(color = "grey30", linewidth = 0.6) +
  labs(title = "b) Composante saisonnière", x = NULL, y = "Débit (m³/s)") +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  )

# Graphique 3 — Résidus
p3 <- ggplot(composantes, aes(x = date, y = residus)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pmin(residus, 1), ymax = 1), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 1, ymax = pmax(residus, 1)), fill = "tomato", alpha = 0.3) +
  geom_line(color = "grey30", linewidth = 0.6) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "c) Résidus (irrégulier)", x = NULL, y = "Facteur") +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

# Assembler
(p1 / p2 / p3) +
  plot_annotation(
    title    = "Décomposition X11 du débit du Var — 2008–2019",
    subtitle = "Station Y6442010 — Agrégation mensuelle",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey50")
    )
  )

### ondelette decomposition -------------------------------------------------

# données journalières
wt <- analyze.wavelet(Y6442010_depuis_2008, "débit",
                      loess.span = 0,
                      dt = 1, dj = 1/12,
                      lowerPeriod = 16,
                      upperPeriod = 365 * 4,
                      make.pval = TRUE)

wt.image(wt, main = "Analyse en ondelettes du débit du Var journalier (2008 - 2020)",
         # Axe X : les dates
         show.date   = TRUE,          # affiche les vraies dates
         date.format = "%Y",          # format des dates sur l'axe
         
         # Axe Y : les périodes
         periodlab  = "Période (jour)",
         
         # Légende couleur
         legend.params = list(
           lab        = "Puissance (log2)",
           lab.line   = 3
         ),
         
         # Significativité
         siglvl      = 0.05,          # seuil de significativité
         col.contour = "black")        # couleur des contours significatifs)

# données mensuelles

wt <- analyze.wavelet(debit_mensuel, "debit_mean",
                      loess.span = 0,
                      dt = 1, dj = 1/12,
                      lowerPeriod = 16,
                      upperPeriod = 12 * 4,
                      make.pval = TRUE)

wt.image(wt, 
         main       = "Analyse en ondelettes du débit du Var (2008–2026)",
         # Axe X : les dates
         show.date   = TRUE,          # affiche les vraies dates
         date.format = "%Y",          # format des dates sur l'axe
         # Axe Y : les périodes
         periodlab  = "Période (mois)",
         
         # Légende couleur
         legend.params = list(
           lab        = "Puissance (log2)",
           lab.line   = 3
         ),
         
         # Significativité
         siglvl      = 0.05,          # seuil de significativité
         col.contour = "black")        # couleur des contours significatifs))



## Paillon runoff ----------------------------------------------------------

# # Calculer le modèle de régression linéaire
# modele_Paillon_2012 <- lm(ABA_debit_mean ~ date, data = Paillon_all_debit)
# summary(modele_Paillon_2012)
# 
# # Extraire les résultats avec broom::tidy()
# resultats_Paillon_2012 <- tidy(modele_Paillon_2012)
# 
# # Extraire la valeur p de la pente
# p_value_Paillon_2012 <- resultats_Paillon_2012$p.value[2]  # 2ème ligne = coefficient de 'date'
# 
# # Extraire les coefficients pour l'équation
# intercept_Paillon_2012 <- coef(modele_Paillon_2012)[1]
# slope_Paillon_2012 <- coef(modele_Paillon_2012)[2]

# on fait la même chose mais pour la station Trinité

# Calculer le modèle de régression linéaire
modele_Paillon_2012 <- lm(TRI_debit_mean ~ date, data = Paillon_all_debit)
summary(modele_Paillon_2012)

# Extraire les résultats avec broom::tidy()
resultats_Paillon_2012 <- tidy(modele_Paillon_2012)

# Extraire la valeur p de la pente
p_value_Paillon_2012 <- resultats_Paillon_2012$p.value[2]  # 2ème ligne = coefficient de 'date'

# Extraire les coefficients pour l'équation
intercept_Paillon_2012 <- coef(modele_Paillon_2012)[1]
slope_Paillon_2012 <- coef(modele_Paillon_2012)[2]


# graphique en échelle normale

ggplot(data = Paillon_all_debit, mapping = aes(x = date, y = TRI_debit_mean)) +
  geom_line(
    color     = "gold",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_smooth(
    method = "lm", se = TRUE,
    color  = "#C0392B", fill = "#E74C3C", alpha = 0.12,
    linewidth = 0.8
  ) +
  annotate("text",
           x     = min(Paillon_all_debit$date, na.rm = TRUE),
           y     = max(Paillon_all_debit$TRI_debit_mean, na.rm = TRUE) * 0.90,
           label = paste0("y = ", round(intercept_Paillon_2012, 3),
                          " + ", round(slope_Paillon_2012, 7), " × x"),
           hjust = 0, vjust = 1,
           size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x     = min(Paillon_all_debit$date, na.rm = TRUE),
           y     = max(Paillon_all_debit$TRI_debit_mean, na.rm = TRUE) * 0.82,
           label = paste0("p = ", ifelse(p_value_Paillon_2012 < 0.001, "< 0.001",
                                         format(p_value_Paillon_2012, digits = 3))),
           hjust = 0, vjust = 1,
           size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Paillon à la station Trinité (2013–2026)",
    subtitle = "Régression linéaire non pondérée",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station Trinité"
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

# put a weighted linear regression model on our datas
# if peak exceed a threshold, then give them less weight

# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil_paillon <- mean(Paillon_all_debit$TRI_debit_mean, na.rm = TRUE) + 2 * sd(Paillon_all_debit$TRI_debit_mean, na.rm = TRUE)
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
Paillon_all_debit$poids_TRI <- ifelse(Paillon_all_debit$TRI_debit_mean > seuil_paillon, 0.1, 1)

# Calculer le modèle linéaire pondéré
modele_Paillon_pondéré <- lm(TRI_debit_mean ~ date, data = Paillon_all_debit, weights = poids_TRI)

# Extraire les résultats
resultats_Paillon_pondéré <- tidy(modele_Paillon_pondéré)
p_value_Paillon_pondéré <- resultats_Paillon_pondéré$p.value[2]
intercept_Paillon_pondéré <- coef(modele_Paillon_pondéré)[1]
slope_Paillon_pondéré <- coef(modele_Paillon_pondéré)[2]

# Ajouter les prédictions au data.frame
Paillon_all_debit$débit_pred_pondéré_TRI <- predict(modele_Paillon_pondéré, Paillon_all_debit)

ggplot(data = Paillon_all_debit, aes(x = date, y = TRI_debit_mean)) +
  geom_line(
    color     = "gold",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  # geom_smooth(
  #   method = "lm", se = TRUE,
  #   color  = NA, fill = "#1A7A4A", alpha = 0.12
  # ) +
  geom_line(
    aes(y = débit_pred_pondéré_TRI),
    color     = "#1A7A4A",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x     = min(Paillon_all_debit$date, na.rm = TRUE),
    y     = max(Paillon_all_debit$TRI_debit_mean, na.rm = TRUE) * 0.90,
    label = paste0("y = ", round(intercept_Paillon_pondéré, 3),
                   " + ", round(slope_Paillon_pondéré, 7), " × x"),
    hjust = 0, vjust = 1,
    size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  annotate(
    "text",
    x     = min(Paillon_all_debit$date, na.rm = TRUE),
    y     = max(Paillon_all_debit$TRI_debit_mean, na.rm = TRUE) * 0.80,
    label = paste0("p = ", ifelse(p_value_Paillon_pondéré < 0.001, "< 0.001",
                                  format(p_value_Paillon_pondéré, digits = 3))),
    hjust = 0, vjust = 1,
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
    title    = "Débit journalier du Paillon à la station Trinité (2013–2026)",
    subtitle = "Régression linéaire pondérée — pics atténués (seuil = μ + 2σ)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station Trinité"
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




# en échelle log

ggplot(data = Paillon_all_debit, mapping = aes(x = date, y = ABA_debit_mean)) +
  geom_line(
    color     = "gold",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_smooth(
    method = "lm", se = TRUE,
    color  = "#C0392B", fill = "#E74C3C", alpha = 0.12,
    linewidth = 0.8
  ) +
  annotate("text",
           x     = min(Paillon_all_debit$date, na.rm = TRUE),
           y     = 10^(max(log10(Paillon_all_debit$ABA_debit_mean), na.rm = TRUE) * 0.99),
           label = paste0("log10(y) = ", round(intercept_Paillon_2012, 3),
                          " ", round(slope_Paillon_2012, 7), " × x"),
           hjust = 0, vjust = 1,
           size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x     = min(Paillon_all_debit$date, na.rm = TRUE),
           y     = 10^(max(log10(Paillon_all_debit$ABA_debit_mean), na.rm = TRUE) * 0.94),
           label = paste0("p = ", ifelse(p_value_Paillon_2012 < 0.001, "< 0.001",
                                         format(p_value_Paillon_2012, digits = 3))),
           hjust = 0, vjust = 2,
           size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  labs(
    title    = "Débit journalier du Paillon (2013–2026, échelle log)",
    subtitle = "Régression linéaire non pondérée",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station Abattoir"
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


# on compare les stations abattoir et trinité

ggplot() +
  geom_line(data = Paillon_all_debit, mapping = aes(x = date, y = ABA_debit_mean, color = "Abattoir")) +
  geom_line(data = Paillon_all_debit, mapping = aes(x = date, y = TRI_debit_mean, color = "Trinité")) +
  scale_color_manual(
    name   = "Station",
    values = c("Abattoir" = "#E6A817", "Trinité" = "#4A90D9")
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  labs(
    title    = "Comparaison des débits aux stations Abattoir et Trinité (2013–2026)",
    # subtitle = "Régression linéaire non pondérée",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station Abattoir et Trinité"
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

# Comparaison Abattoir Var

Y6442010_depuis_2013 <- Y6442010_depuis_2000 |> 
  filter(date >= "2013-01-01")


ggplot() +
  geom_line(data = Y6442010_depuis_2013, mapping = aes(x = date, y = débit, color = "Var")) +
  geom_line(data = Paillon_all_debit, mapping = aes(x = date, y = ABA_debit_mean, color = "Paillon - ABA")) +
  scale_color_manual(
    name   = "Station",
    values = c("Var" = "blue", "Paillon - ABA" = "#E6A817")
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  labs(
    title    = "Comparaison des débits du Var et du Paillon (station Abattoir) (2013–2026)",
    # subtitle = "Régression linéaire non pondérée",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur et HydroFrance — station Abattoir"
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

## Paillon TS decomposition ----------------------------------------------------

### Fourrier decomposition --------------------------------------------------

#### Robert Website ----------------------------------------------------------

# filter NA
signal <- Paillon_all_debit$ABA_debit_mean[!is.na(Paillon_all_debit$ABA_debit_mean)]

fft_result <- fft(signal)
n <- length(signal)

print(fft_result)

# Visualization
par(mfrow = c(2, 1))
plot(signal, type = "l", main = "Original Signal")
plot(Mod(fft_result), type = "l", main = "FFT Magnitudes")


# identify the position of the highest frequency value
magnitudes<- abs(fft_result)  ## or mod()

# Find the frequency with the largest amplitude
max_index_sine <- which.max(magnitudes)

paste("magnitude value: ", magnitudes[max_index_sine] )

paste("index position: ",max_index_sine)


#### Claude ------------------------------------------------------

fft_result <- fft(signal)
n <- length(signal)

# Fréquences en cycles/jour
freq <- (0:(n/2)) / n

# Périodes en jours
periode <- 1 / freq[-1]  # on enlève la fréquence 0 (composante continue)

# Amplitude
amplitude <- Mod(fft_result)[2:(n/2 + 1)]

# Graphique
plot(periode, amplitude, type = "l",
     xlim = c(0, 400),  # zoom sur les périodes < 400 jours
     xlab = "Période (jours)",
     ylab = "Amplitude",
     main = "Spectre FFT du débit du Paillon (2013 - 2026)")

# Marquer le cycle annuel
abline(v = 365, col = "red", lty = 2)


### STL decomposition --------------------------------------------------

Paillon_all_debit <- Paillon_all_debit  |>
  group_by(year = lubridate::year(date))  |>
  filter(!is.na(ABA_debit_mean) > 0.9)  |>
  ungroup()

ts_data <- ts(Paillon_all_debit$ABA_debit_mean, frequency = 365) # 365 pour données journalières

decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)
plot(decomp)

tendance <- data.frame(
  date = Paillon_all_debit$date,  # Utilise les dates originales
  valeur = as.numeric(decomp$time.series[, "trend"])
)

model_lm <- lm(valeur ~ date, data = tendance)
summary(model_lm)

### X11 decomposition --------------------------------------------------

# X11 nécessite une fréquence mensuelle ou trimestrielle
# Si tu as des données journalières, agrège d'abord par mois

debit_mensuel <- Paillon_all_debit |>
  mutate(mois = floor_date(date, "month")) |>
  group_by(mois) |>
  summarise(debit_mean = mean(ABA_debit_mean, na.rm = TRUE)) |>
  filter(!is.na(debit_mean))

# Convertir en ts mensuel
debit_ts_mensuel <- ts(debit_mensuel$debit_mean, 
                       start     = c(2013, 1), 
                       frequency = 12)

# X11 decomposition
x11_result <- seas(debit_ts_mensuel, x11 = "")
plot(x11_result)

# Extraire les composantes
trend     <- trend(x11_result)
seasonal  <- seasonal(x11_result)
remainder <- irregular(x11_result)


### ondelette decomposition -------------------------------------------------

# données journalières
wt <- analyze.wavelet(Paillon_all_debit, "ABA_debit_mean",
                      loess.span = 0,
                      dt = 1, dj = 1/12,
                      lowerPeriod = 16,
                      upperPeriod = 365 * 4,
                      make.pval = TRUE)

wt.image(wt, main = "Analyse en ondelettes du débit du Paillon journalier (2013 - 2026)",
         # Axe X : les dates
         show.date   = TRUE,          # affiche les vraies dates
         date.format = "%Y",          # format des dates sur l'axe
         
         # Axe Y : les périodes
         periodlab  = "Période (jour)",
         
         # Légende couleur
         legend.params = list(
           lab        = "Puissance (log2)",
           lab.line   = 3
         ),
         
         # Significativité
         siglvl      = 0.05,          # seuil de significativité
         col.contour = "black")        # couleur des contours significatifs)

# données mensuelles

wt <- analyze.wavelet(debit_mensuel, "debit_mean",
                      loess.span = 0,
                      dt = 1, dj = 1/12,
                      lowerPeriod = 16,
                      upperPeriod = 12 * 4,
                      make.pval = TRUE)

wt.image(wt, 
         main       = "Analyse en ondelettes du débit du Var (2008–2026)",
         # Axe X : les dates
         show.date   = TRUE,          # affiche les vraies dates
         date.format = "%Y",          # format des dates sur l'axe
         # Axe Y : les périodes
         periodlab  = "Période (mois)",
         
         # Légende couleur
         legend.params = list(
           lab        = "Puissance (log2)",
           lab.line   = 3
         ),
         
         # Significativité
         siglvl      = 0.05,          # seuil de significativité
         col.contour = "black")        # couleur des contours significatifs))






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

ggplot(data = Magnan_all_debit, aes(x = date, y = AAM_debit_mean)) +
  geom_line(
    color     = "violetred",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_smooth(
    method = "lm", se = TRUE,
    color  = "#C0392B", fill = "#E74C3C", alpha = 0.12,
    linewidth = 0.8
  ) +
  annotate("text",
           x     = min(Magnan_all_debit$date, na.rm = TRUE),
           y     = max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) * 0.90,
           label = paste0("y = ", round(intercept_Magnan, 3),
                          " + ", round(slope_Magnan, 7), " × x"),
           hjust = 0, vjust = 1,
           size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate("text",
           x     = min(Magnan_all_debit$date, na.rm = TRUE),
           y     = max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) * 0.80,
           label = paste0("p = ", ifelse(p_value_Magnan < 0.001, "< 0.001",
                                         format(p_value_Magnan, scientific = TRUE, digits = 3))),
           hjust = 0, vjust = 1,
           size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Débit journalier du Magnan (2014–2026)",
    subtitle = "Régression linéaire avec intervalle de confiance à 95 %",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station Magnan"
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

crues_magnan <- Magnan_all_debit |> 
  filter(AAM_debit_mean > 20)

# put a weighted linear regression model on our datas
# if peak exceed a threshold, then give them less weight

# Définir un seuil (par exemple, 2 écarts-types au-dessus de la moyenne)
seuil_magnan <- mean(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) + 2 * sd(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE)
# Attribuer des poids : 1 pour les valeurs normales, 0.1 pour les pics
Magnan_all_debit$poids_AAM <- ifelse(Magnan_all_debit$AAM_debit_mean > seuil_magnan, 0.1, 1)

# Calculer le modèle linéaire pondéré
modele_Magnan_pondéré <- lm(AAM_debit_mean ~ date, data = Magnan_all_debit, weights = poids_AAM)

# Extraire les résultats
resultats_Magnan_pondéré <- tidy(modele_Magnan_pondéré)
p_value_Magnan_pondéré <- resultats_Magnan_pondéré$p.value[2]
intercept_Magnan_pondéré <- coef(modele_Magnan_pondéré)[1]
slope_Magnan_pondéré <- coef(modele_Magnan_pondéré)[2]

# Ajouter les prédictions au data.frame
Magnan_all_debit$débit_pred_pondéré_AAM <- predict(modele_Magnan_pondéré, Magnan_all_debit)

ggplot(data = Magnan_all_debit, aes(x = date, y = AAM_debit_mean)) +
  geom_line(
    color     = "violetred",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_smooth(
    method = "lm", se = TRUE,
    color  = NA, fill = "#1A7A4A", alpha = 0.12
  ) +
  geom_line(
    aes(y = débit_pred_pondéré_AAM),
    color     = "#1A7A4A",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x     = min(Magnan_all_debit$date, na.rm = TRUE),
    y     = max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) * 0.90,
    label = paste0("y = ", round(intercept_Magnan_pondéré, 3),
                   " + ", round(slope_Magnan_pondéré, 7), " × x"),
    hjust = 0, vjust = 1,
    size  = 8, color = "#1A7A4A", fontface = "italic", family = "serif"
  ) +
  annotate(
    "text",
    x     = min(Magnan_all_debit$date, na.rm = TRUE),
    y     = max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE) * 0.80,
    label = paste0("p = ", ifelse(p_value_Magnan_pondéré < 0.001, "< 0.001",
                                  format(p_value_Magnan_pondéré, digits = 3))),
    hjust = 0, vjust = 2,
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
    title    = "Débit journalier du Magnan à la station AAM (2014–2026)",
    subtitle = "Régression linéaire pondérée — pics atténués (seuil = μ + 2σ)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station AAM"
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

# en échelle log
Magnan_all_debit$log_débit <- log10(Magnan_all_debit$AAM_debit_mean)

# Modèle non pondéré (supprimer weights)
modele_log <- lm(log_débit ~ date, data = Magnan_all_debit)

# Extraire les résultats
resultats_log <- tidy(modele_log)
p_value_log   <- resultats_log$p.value[2]
intercept_log <- coef(modele_log)[1]
slope_log     <- coef(modele_log)[2]

# Prédictions
Magnan_all_debit$log_debit_pred_np <- predict(modele_log, Magnan_all_debit)
Magnan_all_debit$debit_pred_log_np <- 10^(Magnan_all_debit$log_debit_pred_np)

# Calculer le modèle linéaire pondéré en échelle log
# modele_log_pondéré <- lm(log_débit ~ date, data = Y6442010_depuis_2006, weights = poids)
# 
# # Extraire les résultats
# resultats_log_pondéré <- tidy(modele_log_pondéré)
# p_value_log_pondéré <- resultats_log_pondéré$p.value[2]
# intercept_log_pondéré <- coef(modele_log_pondéré)[1]
# slope_log_pondéré <- coef(modele_log_pondéré)[2]
# 
# # Créer les prédictions en échelle log
# Y6442010_depuis_2006$log_debit_pred <- predict(modele_log_pondéré, Y6442010_depuis_2006)
# 
# # Convertir les prédictions en échelle normale
# Y6442010_depuis_2006$debit_pred_log <- 10^(Y6442010_depuis_2006$log_debit_pred)

# Créer le graphique en échelle log

ggplot(data = Magnan_all_debit, aes(x = date, y = AAM_debit_mean)) +
  geom_line(
    color     = "violetred",
    linewidth = 0.5,
    alpha     = 0.8
  ) +
  geom_line(
    aes(y = debit_pred_log_np),
    color     = "#C0392B",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x     = min(Magnan_all_debit$date, na.rm = TRUE),
    y     = 10^(max(log10(Magnan_all_debit$AAM_debit_mean), na.rm = TRUE) * 0.99),
    label = paste0("log10(y) = ", round(intercept_log, 3),
                   " + ", round(slope_log, 7), " × x"),
    hjust = 0, vjust = 1,
    size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  annotate(
    "text",
    x     = min(Magnan_all_debit$date, na.rm = TRUE),
    y     = 10^(max(log10(Magnan_all_debit$AAM_debit_mean), na.rm = TRUE) * 0.94),
    label = paste0("p = ", ifelse(p_value_log < 0.001, "< 0.001",
                                  format(p_value_log, scientific = TRUE, digits = 3))),
    hjust = 0, vjust = 3,
    size  = 8, color = "#C0392B", fontface = "italic", family = "serif"
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  labs(
    title    = "Débit journalier du Magnan à la station AAM(2014–2026, échelle log)",
    subtitle = "Régression linéaire non pondérée en échelle log",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Source : Métropole Nice Côte d'Azur — station AAM"
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


## Magnan TS decomposition ----------------------------------------------------

### Fourrier decomposition --------------------------------------------------

#### Robert Website ----------------------------------------------------------

# filter NA
signal <- Magnan_all_debit$AAM_debit_mean[!is.na(Magnan_all_debit$AAM_debit_mean)]

fft_result <- fft(signal)
n <- length(signal)

print(fft_result)

# Visualization
par(mfrow = c(2, 1))
plot(signal, type = "l", main = "Original Signal")
plot(Mod(fft_result), type = "l", main = "FFT Magnitudes")


# identify the position of the highest frequency value
magnitudes<- abs(fft_result)  ## or mod()

# Find the frequency with the largest amplitude
max_index_sine <- which.max(magnitudes)

paste("magnitude value: ", magnitudes[max_index_sine] )

paste("index position: ",max_index_sine)


#### Claude ------------------------------------------------------

fft_result <- fft(signal)
n <- length(signal)

# Fréquences en cycles/jour
freq <- (0:(n/2)) / n

# Périodes en jours
periode <- 1 / freq[-1]  # on enlève la fréquence 0 (composante continue)

# Amplitude
amplitude <- Mod(fft_result)[2:(n/2 + 1)]

# Graphique
plot(periode, amplitude, type = "l",
     xlim = c(0, 400),  # zoom sur les périodes < 400 jours
     xlab = "Période (jours)",
     ylab = "Amplitude",
     main = "Spectre FFT du débit du Magnan (2014 - 2026)")

# Marquer le cycle annuel
abline(v = 365, col = "red", lty = 2)


### STL decomposition --------------------------------------------------

Magnan_all_debit <- Magnan_all_debit  |>
  group_by(year = lubridate::year(date))  |>
  filter(!is.na(AAM_debit_mean) > 0.9)  |>
  ungroup()

ts_data <- ts(Magnan_all_debit$AAM_debit_mean, frequency = 365) # 365 pour données journalières

decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)
plot(decomp)

tendance <- data.frame(
  date = Magnan_all_debit$date,  # Utilise les dates originales
  valeur = as.numeric(decomp$time.series[, "trend"])
)

model_lm <- lm(valeur ~ date, data = tendance)
summary(model_lm)

### X11 decomposition --------------------------------------------------

# X11 nécessite une fréquence mensuelle ou trimestrielle
# Si tu as des données journalières, agrège d'abord par mois

debit_mensuel <- Magnan_all_debit |>
  mutate(mois = floor_date(date, "month")) |>
  group_by(mois) |>
  summarise(debit_mean = mean(AAM_debit_mean, na.rm = TRUE)) |>
  filter(!is.na(debit_mean))

# Convertir en ts mensuel
debit_ts_mensuel <- ts(debit_mensuel$debit_mean, 
                       start     = c(2014, 1), 
                       frequency = 12)

# X11 decomposition
x11_result <- seas(debit_ts_mensuel, x11 = "")
plot(x11_result)

# Extraire les composantes
trend     <- trend(x11_result)
seasonal  <- seasonal(x11_result)
remainder <- irregular(x11_result)


### ondelette decomposition -------------------------------------------------

# données journalières
wt <- analyze.wavelet(Magnan_all_debit, "AAM_debit_mean",
                      loess.span = 0,
                      dt = 1, dj = 1/12,
                      lowerPeriod = 16,
                      upperPeriod = 365 * 4,
                      make.pval = TRUE)

wt.image(wt, main = "Analyse en ondelettes du débit du Magnan journalier (2014 - 2026)",
         # Axe X : les dates
         show.date   = TRUE,          # affiche les vraies dates
         date.format = "%Y",          # format des dates sur l'axe
         
         # Axe Y : les périodes
         periodlab  = "Période (jour)",
         
         # Légende couleur
         legend.params = list(
           lab        = "Puissance (log2)",
           lab.line   = 3
         ),
         
         # Significativité
         siglvl      = 0.05,          # seuil de significativité
         col.contour = "black")        # couleur des contours significatifs)

# données mensuelles

wt <- analyze.wavelet(debit_mensuel, "debit_mean",
                      loess.span = 0,
                      dt = 1, dj = 1/12,
                      lowerPeriod = 16,
                      upperPeriod = 12 * 4,
                      make.pval = TRUE)

wt.image(wt, 
         main       = "Analyse en ondelettes du débit mensuel du Magnan (2014–2026)",
         # Axe X : les dates
         show.date   = TRUE,          # affiche les vraies dates
         date.format = "%Y",          # format des dates sur l'axe
         # Axe Y : les périodes
         periodlab  = "Période (mois)",
         
         # Légende couleur
         legend.params = list(
           lab        = "Puissance (log2)",
           lab.line   = 3
         ),
         
         # Significativité
         siglvl      = 0.05,          # seuil de significativité
         col.contour = "black")        # couleur des contours significatifs))







## plotting of rivers together ---------------------------------------------

# without an adjustment

ggplot() +
  geom_line(
    data = Y6442010_depuis_2013,
    aes(x = date, y = débit, color = "Var"),
    linewidth = 0.5, alpha = 0.8
  ) +
  geom_line(
    data = Paillon_all_debit,
    aes(x = date, y = ABA_debit_mean, color = "Paillon ABA"),
    linewidth = 0.5, alpha = 0.8
  ) +
  geom_line(
    data = Magnan_all_debit,
    aes(x = date, y = AAM_debit_mean, color = "Magnan AAM"),
    linewidth = 0.5, alpha = 0.8
  ) +
  scale_color_manual(
    name   = "Cours d'eau",
    values = c("Var" = "blue", "Paillon ABA" = "gold", "Magnan AAM" = "violetred")
  ) +
  scale_x_date(
    date_breaks       = "2 years",
    date_labels       = "%Y",
    date_minor_breaks = "1 year",
    expand            = expansion(mult = 0.01)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title    = "Comparaison des débits journaliers du Var, du Paillon et du Magnan (2013–2026)",
    subtitle = "Stations : Pont Napoléon (Var), Abattoir (Paillon), Magnan (Magnan)",
    x        = NULL,
    y        = "Débit (m³ s⁻¹)",
    caption  = "Sources : Banque HydroFrance, Métropole Nice Côte d'Azur"
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
    panel.border  = element_rect(color = "grey70", linewidth = 0.5),
    legend.position  = "top",
    legend.title     = element_text(size = 10, face = "bold"),
    legend.text      = element_text(size = 9, color = "grey30"),
    legend.key.width = unit(1.5, "cm")
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
