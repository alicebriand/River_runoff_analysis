# Runoff comparison of Nices's rivers

# pathway : "~/Documents/Alice/Rapports annuels/"

# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidync)
library(ncdf4)    # For reading NetCDF files
library(lubridate) # For working with dates
library(reshape2) # For data reshaping
library(ggplot2)


# loading Nices's rivers data ---------------------------------------------
# loading Var data ---------------------------------------------

# loading file
Y6442010_Hydro <- read_csv("~/Documents/Alice/Hydro France/Y644201002_QmnJ(n=1_non-glissant)_2016_2026.csv")

# renaming columns
Y6442010_Hydro <- Y6442010_Hydro %>% 
  rename(Date = `Date (TU)`, débit = `Valeur (en m³/s)`)

Y6442010_Hydro_complete <- Y6442010_Hydro %>%
  complete(Date = seq(min(Date), max(Date), by = "day"))

# loading Paillon data ---------------------------------------------

Paillon_variable_names <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012012-000000_01012013-000000.csv",
                                 delim = ",", locale = locale(decimal_mark = ","), 
                                 skip = 1, n_max = 1, col_names = FALSE)

Paillon_variable_names_2 <- pivot_longer(Paillon_variable_names, cols = X1:X48) %>% 
  dplyr::filter(!is.na(value))

Paillon_variable_names_3 <- as.vector(Paillon_variable_names_2$value)

## 2012 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------

Paillon_variable_names_2012 <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012012-000000_01012013-000000.csv",
                                      delim = ",", locale = locale(decimal_mark = "."), 
                                      skip = 3, n_max = 1, col_names = FALSE)

Paillon_variable_names_stations_2012 <- mutate_all(Paillon_variable_names_2012, .funs = as.character) %>% 
  pivot_longer(cols = X1:X48) %>% 
  mutate(var_name = c("",rep(Paillon_variable_names_3[1], 11), rep(Paillon_variable_names_3[2], 10), 
                      rep(Paillon_variable_names_3[3], 9), rep(Paillon_variable_names_3[4], 9), 
                      rep(Paillon_variable_names_3[5], 8))) %>% 
  mutate(station_Paillon = paste(value, var_name, sep = " - "))


Paillon_2012 <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012012-000000_01012013-000000.csv",
                       delim = ",", locale = locale(decimal_mark = "."), 
                       skip = 4, col_names = Paillon_variable_names_stations_2012$station_Paillon) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = `ABA - NIVEAUX D'EAU en cm`:`LUC - Temperature en °C`) %>% 
  separate(name, into = c("station", "variable"), sep = " - ") %>% 
  dplyr::rename(Date_time = ` DATE ET HEURE  - `) %>%  
  dplyr::filter(!is.na(value)) %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"), 
         value = as.numeric(value)) 

### Treatment -----------------------------------------------------------

Paillon_2012_debit <- Paillon_2012 %>% 
  dplyr::filter(variable == "DEBITS m3/s", station == "ABA") %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  group_by(Date, station) %>% 
  summarise(Débit_moyen = mean(as.numeric(value), na.rm = TRUE), 
            Débit_std = sd(as.numeric(value), na.rm = TRUE),
            Débit_max = max(as.numeric(value), na.rm = TRUE), 
            Débit_min = min(as.numeric(value), na.rm = TRUE)
  )




## 2013 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------

Paillon_variable_names_2013 <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012013-000000_01012014-000000.csv",
                                          delim = ",", locale = locale(decimal_mark = "."), 
                                          skip = 3, n_max = 1, col_names = FALSE)

Paillon_variable_names_stations_2013 <- mutate_all(Paillon_variable_names_2013, .funs = as.character) %>% 
  pivot_longer(cols = X1:X48) %>% 
  mutate(var_name = c("",rep(Paillon_variable_names_3[1], 11), rep(Paillon_variable_names_3[2], 10), 
                      rep(Paillon_variable_names_3[3], 9), rep(Paillon_variable_names_3[4], 9), 
                      rep(Paillon_variable_names_3[5], 8))) %>% 
  mutate(station_Paillon = paste(value, var_name, sep = " - "))


Paillon_2013 <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012013-000000_01012014-000000.csv",
                           delim = ",", locale = locale(decimal_mark = "."), 
                           skip = 4, col_names = Paillon_variable_names_stations_2013$station_Paillon) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = `ABA - NIVEAUX D'EAU en cm`:`LUC - Temperature en °C`) %>% 
  separate(name, into = c("station", "variable"), sep = " - ") %>% 
  dplyr::rename(Date_time = ` DATE ET HEURE  - `) %>%  
  dplyr::filter(!is.na(value)) %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"), 
         value = as.numeric(value)) 

### Treatment -----------------------------------------------------------

Paillon_2013_debit <- Paillon_2013 %>% 
  dplyr::filter(variable == "DEBITS m3/s", station == "ABA") %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  group_by(Date, station) %>% 
  summarise(Débit_moyen = mean(as.numeric(value), na.rm = TRUE), 
            Débit_std = sd(as.numeric(value), na.rm = TRUE),
            Débit_max = max(as.numeric(value), na.rm = TRUE), 
            Débit_min = min(as.numeric(value), na.rm = TRUE)
  )

## 2014 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------

Paillon_variable_names_2014 <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012014-000000_01012015-000000.csv+",
                                          delim = ",", locale = locale(decimal_mark = "."), 
                                          skip = 3, n_max = 1, col_names = FALSE)

Paillon_variable_names_stations_2013 <- mutate_all(Paillon_variable_names_2013, .funs = as.character) %>% 
  pivot_longer(cols = X1:X48) %>% 
  mutate(var_name = c("",rep(Paillon_variable_names_3[1], 11), rep(Paillon_variable_names_3[2], 10), 
                      rep(Paillon_variable_names_3[3], 9), rep(Paillon_variable_names_3[4], 9), 
                      rep(Paillon_variable_names_3[5], 8))) %>% 
  mutate(station_Paillon = paste(value, var_name, sep = " - "))


Paillon_2013 <- read_delim("Paillon.csv/Rapport_PAILLON_PERIODE_01012013-000000_01012014-000000.csv",
                           delim = ",", locale = locale(decimal_mark = "."), 
                           skip = 4, col_names = Paillon_variable_names_stations_2013$station_Paillon) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = `ABA - NIVEAUX D'EAU en cm`:`LUC - Temperature en °C`) %>% 
  separate(name, into = c("station", "variable"), sep = " - ") %>% 
  dplyr::rename(Date_time = ` DATE ET HEURE  - `) %>%  
  dplyr::filter(!is.na(value)) %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"), 
         value = as.numeric(value)) 

### Treatment -----------------------------------------------------------

Paillon_2013_debit <- Paillon_2013 %>% 
  dplyr::filter(variable == "DEBITS m3/s", station == "ABA") %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  group_by(Date, station) %>% 
  summarise(Débit_moyen = mean(as.numeric(value), na.rm = TRUE), 
            Débit_std = sd(as.numeric(value), na.rm = TRUE),
            Débit_max = max(as.numeric(value), na.rm = TRUE), 
            Débit_min = min(as.numeric(value), na.rm = TRUE)
  )
## 2015 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2016 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2017 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2018 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2019 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2020 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2021---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2022 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2023 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2024 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------

## 2025 ---------------------------------------------
### Pre-treatment -----------------------------------------------------------
### Treatment -----------------------------------------------------------



### Paillon tout -----------------------------------------------------------

Paillon_all <- bind_rows(Paillon_2012_debit, ...)
save(Paillon_all, file = "Paillon.csv/Paillon_all.Rdata")

load("Paillon.csv/Paillon_all.Rdata")



















Paillon_2012 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012012-000000_01012013-000000.csv")
Paillon_2013 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012013-000000_01012014-000000.csv")
Paillon_2014 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012014-000000_01012015-000000.csv")
Paillon_2015 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012015-000000_01012016-000000.csv")
Paillon_2016 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012016-000000_01012017-000000.csv")
Paillon_2017 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012017-000000_01012018-000000.csv")
Paillon_2018 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012018-000000_01012019-000000.csv")
Paillon_2019 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012019-000000_01012020-000000.csv")
Paillon_2020 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012020-000000_01012021-000000.csv")
Paillon_2021 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012021-000000_01012022-000000.csv")
Paillon_2022 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012022-000000_01012023-000000.csv")
Paillon_2023 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012023-000000_01012024-000000.csv")
Paillon_2024 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012024-000000_01012025-000000.csv")
Paillon_2025 <- read_csv("~/Documents/Alice/Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012025-000000_01012026-000000.csv")





















Paillon_2012 <- Paillon_2012 %>% 
  select(
  )

liste_paillon <- list(
  Paillon_2013_debit, Paillon_2014_debit, Paillon_2015_debit,
  Paillon_2016_debit, Paillon_2017_debit, Paillon_2018_debit,
  Paillon_2019_debit, Paillon_2020_debit, Paillon_2021_debit,
  Paillon_2022_debit, Paillon_2023_debit, Paillon_2024_debit,
  Paillon_2025_debit
)

list_Var <- list(
  Var_2021_debit, Var_2022_debit, Var_2025_debit
)

Paillon_tout <- bind_rows(liste_paillon) %>%
  arrange(jour)
Var_tout <- bind_rows(list_Var) %>% 
  arrange(date)

Paillon_tout <- Paillon_tout %>%
  mutate(
    across(ends_with("_debit"), as.numeric),
    across(ends_with("_niveau_eau"), as.numeric),
    across(ends_with("_cumul_pluie"), as.numeric),
    across(ends_with("_altitude"), as.numeric)
  )
# On crée un data frame avec seulement Paillon ABA
Paillon_ABA <- Paillon_tout %>% 
  select(jour, ABA_debit_mean)

# la variable jour n'est plus une date donc on la remplace par une date

Paillon_ABA$jour <- as.Date(Paillon_ABA$jour, format = "%Y-%m-%d")

# On crée une série temporelle
Paillon_ABA_xts <- xts(
  Paillon_ABA$ABA_debit_mean,
  order.by = Paillon_ABA$jour
)

ggplot(Paillon_ABA, aes(jour, ABA_debit_mean)) +
  geom_line() +
  labs(
    title = "Débit du Paillon – série multi-annuelle entre 2013 et 2025",
    x = "Date",
    y = "Débit (m³/s)"
  ) +
  theme_minimal()

# ne marche pas
