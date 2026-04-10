# All runoff together

# pathway : "~/River_runoff_analysis/All runoff together"

# This script will load river runoff data at the mouth of the river and combine 
# them in one data frame. After, it will plot results

# libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

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

mean_debit_Var <- mean(Y6442010_depuis_2000$débit, na.rm = TRUE)
print(mean_debit_Var) # 46.81932
max_debit_Var <- max(Y6442010_depuis_2000$débit, na.rm = TRUE)
print(max_debit_Var) # 962

# calculate the mean and max flow rate of the Paillon river

mean_debit_Paillon <- mean(Paillon_all_debit$ABA_debit_mean, na.rm = TRUE)
print(mean_debit_Paillon) # 23.05077
max_debit_Paillon <- max(Paillon_all_debit$ABA_debit_mean, na.rm = TRUE)
print(max_debit_Paillon) # 477.0833

# calculate the mean and max flow rate of the Magnan river

mean_debit_Magnan <- mean(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE)
print(mean_debit_Magnan) # 0.2312836
max_debit_Magnan <- max(Magnan_all_debit$AAM_debit_mean, na.rm = TRUE)
print(max_debit_Magnan) # 63.45656

# plotting ----------------------------------------------------------------

ggplot() +
  geom_point(data = Y6442010_depuis_2014, aes(x = date, y = débit, color = "Var"),  size = 0.5) +
  geom_point(data = Paillon_all_debit, aes(x = date, y = ABA_debit_mean, color = "Paillon"), size = 0.5) +
  geom_point(data = Magnan_all_debit, aes(x = date, y = AAM_debit_mean, color = "Magnan"), size = 0.5) +
  geom_point(data = All_debit, aes(x = date, y = debit_cumule, color = "débit cumulé"), size = 0.5)

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



