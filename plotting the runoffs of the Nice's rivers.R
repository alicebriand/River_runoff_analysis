# Plotting the runoffs of the Nice's rivers

# pathway : "~/Documents/Alice/code/plotting the runoffs of the Nice's rivers

# libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggpmisc)
library(forecast)

# loading -----------------------------------------------------------------

load("data/Hydro France/Y6442010_depuis_2000.Rdata")
load("~/Documents/Alice/Rapports_annuels/Paillon.csv/Paillon_all_debit.Rdata")

# Paillon_all_debit <- Paillon_all_debit |> 
#   rename(date = "Date")
# 
# save(Paillon_all_debit, file = "~/Documents/Alice/Rapports_annuels/Paillon.csv/Paillon_all_debit.Rdata")

# Complete missing dates in the date range
Y6442010_depuis_2000 <- Y6442010_depuis_2000 %>%
  complete(date = seq(min(date), max(date), by = "day"))

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

# Var runoff
  
  ggplot(data = Y6442010_depuis_2000, mapping = aes(x = date, y = débit)) +
    geom_line(color = "blue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
    stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
      label.x.npc = "right",
      label.y.npc = 0.15,
      parse = TRUE
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
    )
  

# Paillon runoff

ggplot(data = Paillon_all_debit, mapping = aes(x = date, y = Débit_moyen)) +
  geom_line(color = "blue") +
  scale_x_date(
    breaks = "1 year",
    date_labels = "%Y",
    minor_breaks = "2 year"
  ) +
  labs(
    title = "Débit journalier du Var entre 2012 et 2026",
    x = "Date",
    y = "Débit (m³/s)"
  )

# plotting without an adjustment

ggplot() +
  geom_line(data = Y6442010_depuis_2000, aes(x = date, y = débit, color = "Var")) +
  geom_line(data = Paillon_all_debit, aes(x = date, y = Débit_moyen, color = "Paillon")) +
  scale_color_manual(values = c("Var" = "blue", "Paillon" = "gold")) +
  scale_y_continuous(
    name = "Débit du Var (m³/s)",  # Axe principal (gauche) pour le Var
    sec.axis = sec_axis(~ ., name = "Débit du Paillon (m³/s)")) +  # Axe secondaire (droite) pour le Paillon
  scale_x_date(
    breaks = "1 year",  
    date_labels = "%Y", 
    minor_breaks = "1 month" 
  ) +
  labs(
    title = "Comparaison entre le débit du Var (Pont Napoléon) et du Paillon (ABA) de 2012 à 2026",
    x = "Date"
  ) +
  theme_minimal()

# plotting with an adjustment
# scaling adjustment

adjust_factors <- sec_axis_adjustement_factors(Y6442010_depuis_2000$débit, Paillon_all_debit$Débit_moyen)

Y6442010_depuis_2000$débit_ajusté <- Y6442010_depuis_2000$débit * adjust_factors$diff + adjust_factors$adjust

ggplot() +
  geom_line(data =Y6442010_depuis_2000, aes(x = date, y = débit_ajusté, color = "Var")) +
  geom_line(data = Paillon_all_debit, aes(x = date, y = Débit_moyen, color = "Paillon")) +
  scale_color_manual(values = c("Var" = "blue", "Paillon" = "gold")) +
  scale_y_continuous(
    name = "Débit du Var ajusté (m³/s)",
    sec.axis = sec_axis(
      ~ (. * adjust_factors$diff) + adjust_factors$adjust, 
    name = "Débit du Paillon (m³/s)")
  ) +
  scale_x_date(
    breaks = "1 year",  
    date_labels = "%Y",  
    minor_breaks = "1 month"  
  ) +
  labs(
    title = "Comparaison entre le débit du Var (Pont Napoléon) et du Paillon (ABA) de 2012 à 2026 - ajusté",
    x = "Date"
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
