## Analyse temporelle des crues du Paillon pour la période entre 2013 et 2025

## Set working directory

# Load libraries

library(dplyr)
library(readr)
library(timeSeries)
library(lubridate)

# ________________________________________________________________________________________________

Paillon_2012 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012012-000000_01012013-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2013 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012013-000000_01012014-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2014 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012014-000000_01012015-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2015 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012015-000000_01012016-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2016 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012016-000000_01012017-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2017 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012017-000000_01012018-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2018 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012018-000000_01012019-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2019 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012019-000000_01012020-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2020 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012020-000000_01012021-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2021 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012021-000000_01012022-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2022 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012022-000000_01012023-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2023 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012023-000000_01012024-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2024 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012024-000000_01012025-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Paillon_2025 <- read_delim("Rapports annuels/Paillon.csv/Rapport_PAILLON_PERIODE_01012025-000000_01012026-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))

# ________________________________________________________________________________________________
## Analyse de la base de données de 2012

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2012) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie","LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2012 <- Paillon_2012 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

# On ne s'intéresse qu'au débit liquide, donc on ne garde que les colonnes du débit liquide
# dans notre data frame

Paillon_2012_debit <- Paillon_2012 %>%
  select("date_heure","ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

# On supprime les 3 premières lignes

Paillon_2012_debit <- Paillon_2012_debit[-c(1:3), ]

# On a des mesures toutes les 6 min, hors si on veut comparer nos résultats on doit 
# aggréger nos données par heure

# Ensuite on calcule la moyenne du débit par heure --> ça doit être en jour

# D'abord, on converti toutes les valeurs du tableaux pour les variables finissant par
# debit en variables numériques
Paillon_2012_debit <- Paillon_2012_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

# On peut ensuite calculer la moyenne par heure à chaque station

Paillon_2012_debit <- Paillon_2012_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )


# ________________________________________________________________________________________________
## Analyse de la base de données de 2013

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2013) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie","LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2013 <- Paillon_2013 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

# On ne s'intéresse qu'au débit liquide, donc on ne garde que les colonnes du débit liquide
# dans notre data frame

Paillon_2013_debit <- Paillon_2013 %>%
  select("date_heure","ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

# On supprime les 3 premières lignes

Paillon_2013_debit <- Paillon_2013_debit[-c(1:3), ]

# On a des mesures toutes les 6 min, hors si on veut comparer nos résultats on doit 
# aggréger nos données par heure

# Ensuite on calcule la moyenne du débit par heure --> ça doit être en jour

# D'abord, on converti toutes les valeurs du tableaux pour les variables finissant par
# debit en variables numériques
Paillon_2013_debit <- Paillon_2013_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

# On peut ensuite calculer la moyenne par heure à chaque station

Paillon_2013_debit <- Paillon_2013_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2014

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2014) <- c("date_heure",
                        "ABA_niveau_eau", "LABEL_niveau_eau1", "STA_niveau_eau", "LABEL_niveau_eau2",
                        "TRI_niveau_eau", "LABEL_niveau_eau3", "PDR_niveau_eau", "LABEL_niveau_eau4",
                        "LAC_niveau_eau", "LABEL_niveau_eau5", "MDP_niveau_eau", "LABEL_niveau_eau6",
                        "CON_niveau_eau", "LABEL_niveau_eau7", "PALAG_niveau_eau", "LABEL_niveau_eau8", "PAS_niveau_eau",
                        "LABEL_niveau_eau9", "COT_niveau_eau", "LABEL_niveau_eau10", "ESC_niveau_eau", "LABEL_niveau_eau11",
                        "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                        "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                        "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                        "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                        "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                        "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                        "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                        "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                        "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2014 <- Paillon_2014 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2014_debit <- Paillon_2014 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
        "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2014_debit <- Paillon_2014_debit[-c(1:3), ]

Paillon_2014_debit <- Paillon_2014_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

# we have to filter our data because we have weird datas ex : 500 and 0

Paillon_2014_debit <- Paillon_2014_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))


Paillon_2014_debit <- Paillon_2014_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2015

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2015) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau", 
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2015 <- Paillon_2015 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2015_debit <- Paillon_2015 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2015_debit <- Paillon_2015_debit[-c(1:3), ]

Paillon_2015_debit <- Paillon_2015_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )


Paillon_2015_debit <- Paillon_2015_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2016

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2016) <- c("date_heure",
                            "ABA_niveau_eau", "LABEL_niveau_eau1", "STA_niveau_eau", "LABEL_niveau_eau2",
                            "TRI_niveau_eau", "LABEL_niveau_eau3", "PDR_niveau_eau", "LABEL_niveau_eau4",
                            "LAC_niveau_eau", "LABEL_niveau_eau5", "MDP_niveau_eau", "LABEL_niveau_eau6",
                            "CON_niveau_eau", "LABEL_niveau_eau7", "PALAG_niveau_eau", "LABEL_niveau_eau8", "PAS_niveau_eau",
                            "LABEL_niveau_eau9", "COT_niveau_eau", "LABEL_niveau_eau10", "ESC_niveau_eau", "LABEL_niveau_eau11",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2016 <- Paillon_2016 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2016_debit <- Paillon_2016 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2016_debit <- Paillon_2016_debit[-c(1:3), ]

Paillon_2016_debit <- Paillon_2016_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )


Paillon_2016_debit <- Paillon_2016_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2017

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2017) <- c("date_heure",
                            "ABA_niveau_eau", "LABEL_niveau_eau1", "STA_niveau_eau", "LABEL_niveau_eau2",
                            "TRI_niveau_eau", "LABEL_niveau_eau3", "PDR_niveau_eau", "LABEL_niveau_eau4",
                            "LAC_niveau_eau", "LABEL_niveau_eau5", "MDP_niveau_eau", "LABEL_niveau_eau6",
                            "CON_niveau_eau", "LABEL_niveau_eau7", "PALAG_niveau_eau", "LABEL_niveau_eau8", "PAS_niveau_eau",
                            "LABEL_niveau_eau9", "COT_niveau_eau", "LABEL_niveau_eau10", "ESC_niveau_eau", "LABEL_niveau_eau11",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2017 <- Paillon_2017 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2017_debit <- Paillon_2017 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2017_debit <- Paillon_2017_debit[-c(1:3), ]

Paillon_2017_debit <- Paillon_2017_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2017_debit <- Paillon_2017_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2018

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2018) <- c("date_heure",
                            "ABA_niveau_eau", "LABEL_niveau_eau1", "STA_niveau_eau", "LABEL_niveau_eau2",
                            "TRI_niveau_eau", "LABEL_niveau_eau3", "PDR_niveau_eau", "LABEL_niveau_eau4",
                            "LAC_niveau_eau", "LABEL_niveau_eau5", "MDP_niveau_eau", "LABEL_niveau_eau6",
                            "CON_niveau_eau", "LABEL_niveau_eau7", "PALAG_niveau_eau", "LABEL_niveau_eau8", "PAS_niveau_eau",
                            "LABEL_niveau_eau9", "COT_niveau_eau", "LABEL_niveau_eau10", "ESC_niveau_eau", "LABEL_niveau_eau11",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2018 <- Paillon_2018 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2018_debit <- Paillon_2018 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2018_debit <- Paillon_2018_debit[-c(1:3), ]

Paillon_2018_debit <- Paillon_2018_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2018_debit <- Paillon_2018_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2019

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2019) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2019 <- Paillon_2019 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2019_debit <- Paillon_2019 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2019_debit <- Paillon_2019_debit[-c(1:3), ]

Paillon_2019_debit <- Paillon_2019_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2019_debit <- Paillon_2019_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2020

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2020) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2020 <- Paillon_2020 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2020_debit <- Paillon_2020 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2020_debit <- Paillon_2020_debit[-c(1:3), ]

Paillon_2020_debit <- Paillon_2020_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2020_debit <- Paillon_2020_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2021

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2021) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2021 <- Paillon_2021 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2021_debit <- Paillon_2021 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2021_debit <- Paillon_2021_debit[-c(1:3), ]

Paillon_2021_debit <- Paillon_2021_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2021_debit <- Paillon_2021_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2022

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2022) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2022 <- Paillon_2022 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2022_debit <- Paillon_2022 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2022_debit <- Paillon_2022_debit[-c(1:3), ]

Paillon_2022_debit <- Paillon_2022_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2022_debit <- Paillon_2022_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ________________________________________________________________________________________________
## Analyse de la base de données de 2023

# Remplacer les noms de colonnes par des noms simples
colnames(Paillon_2023) <- c("date_heure",
                            "ABA_niveau_eau", "STA_niveau_eau",
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau",
                            "CON_niveau_eau", "PALAG_niveau_eau", "PAS_niveau_eau",
                            "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude", "LAC_altitude",
                            "MDP_altitude", "CON_altitude", "PALAG_altitude", "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie","PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie", "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature", "COA_temperature",
                            "LUC_temperature")

# Convertir la colonne date_heure en objet datetime

Paillon_2023 <- Paillon_2023 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2023_debit <- Paillon_2023 %>%
  select("date_heure", "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
         "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit")

Paillon_2023_debit <- Paillon_2023_debit[-c(1:3), ]

Paillon_2023_debit <- Paillon_2023_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2023_debit <- Paillon_2023_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )


# Analyse de la base de données de 2024 -----------------------------------

colnames(Paillon_2024) <- c("date_heure","ABA_niveau_eau", "STA_niveau_eau", 
                            "TRI_niveau_eau", "PDR_niveau_eau",
                            "LAC_niveau_eau", "MDP_niveau_eau", "CON_niveau_eau", "PALAG_niveau_eau",
                            "PAS_niveau_eau", "COT_niveau_eau", "ESC_niveau_eau",
                            "ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude",
                            "LAC_altitude", "MDP_altitude", "CON_altitude", "PALAG_altitude",
                            "PAS_altitude", "COT_altitude",
                            "ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
                            "MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
                            "ABA_cumul_pluie", "PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
                            "CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
                            "LUC_cumul_pluie",
                            "PDR_temperature", "LAG_temperature", "MDP_temperature",
                            "CON_temperature", "BAI_temperature", "ESC_temperature",
                            "COA_temperature", "LUC_temperature",
                            # Ajoute ici les noms des colonnes supplémentaires (12 colonnes manquantes)
                            "colonne_48", "colonne_49", "colonne_50", "colonne_51", "colonne_52",
                            "colonne_53", "colonne_54", "colonne_55", "colonne_56", "colonne_57",
                            "colonne_58", "colonne_59"
)

Paillon_2024 <- Paillon_2024 %>%
  mutate(date_heure = parse_datetime(date_heure,
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))
Paillon_2024_debit <- Paillon_2024 %>%
  select(1,34:42)

Paillon_2024_debit <- Paillon_2024_debit %>% 
  rename(
    ABA_debit = "LAG_cumul_pluie",
    STA_debit = "MDP_cumul_pluie",
    TRI_debit = "CON_cumul_pluie",
    PDR_debit = "BAI_cumul_pluie",
    LAC_debit = "ESC_cumul_pluie",
    MDP_debit = "COA_cumul_pluie",
    CON_debit = "LUC_cumul_pluie",
    PALAG_debit = "PDR_temperature",
    PAS_debit = "LAG_temperature"
  )

Paillon_2024_debit <- Paillon_2024_debit[-c(1:3), ]

Paillon_2024_debit <- Paillon_2024_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2024_debit <- Paillon_2024_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )


# Analyse de la base de données de 2025 -----------------------------------

# Remplacer les noms de colonnes par des noms simples

colnames(Paillon_2025) <- c("date_heure","ABA_niveau_eau", "STA_niveau_eau", 
                            "TRI_niveau_eau", "PDR_niveau_eau",
"LAC_niveau_eau", "MDP_niveau_eau", "CON_niveau_eau", "PALAG_niveau_eau",
"PAS_niveau_eau", "COT_niveau_eau", "ESC_niveau_eau",
"ABA_altitude", "STA_altitude", "TRI_altitude", "PDR_altitude",
"LAC_altitude", "MDP_altitude", "CON_altitude", "PALAG_altitude",
"PAS_altitude", "COT_altitude",
"ABA_debit", "STA_debit", "TRI_debit", "PDR_debit", "LAC_debit",
"MDP_debit", "CON_debit", "PALAG_debit", "PAS_debit",
"ABA_cumul_pluie", "PDR_cumul_pluie", "LAG_cumul_pluie", "MDP_cumul_pluie",
"CON_cumul_pluie", "BAI_cumul_pluie", "ESC_cumul_pluie", "COA_cumul_pluie",
"LUC_cumul_pluie",
"PDR_temperature", "LAG_temperature", "MDP_temperature",
"CON_temperature", "BAI_temperature", "ESC_temperature",
"COA_temperature", "LUC_temperature",
# Ajoute ici les noms des colonnes supplémentaires (12 colonnes manquantes)
"colonne_48", "colonne_49", "colonne_50", "colonne_51", "colonne_52",
"colonne_53", "colonne_54", "colonne_55", "colonne_56", "colonne_57",
"colonne_58", "colonne_59"
)

Paillon_2025 <- Paillon_2025 %>%
  mutate(date_heure = parse_datetime(date_heure,
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Paillon_2025_debit <- Paillon_2025 %>%
  select(1,34:42)

Paillon_2025_debit <- Paillon_2025_debit %>% 
  rename(
    ABA_debit = "LAG_cumul_pluie",
    STA_debit = "MDP_cumul_pluie",
    TRI_debit = "CON_cumul_pluie",
    PDR_debit = "BAI_cumul_pluie",
    LAC_debit = "ESC_cumul_pluie",
    MDP_debit = "COA_cumul_pluie",
    CON_debit = "LUC_cumul_pluie",
    PALAG_debit = "PDR_temperature",
    PAS_debit = "LAG_temperature"
  )

Paillon_2025_debit <- Paillon_2025_debit[-c(1:3), ]

Paillon_2025_debit <- Paillon_2025_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Paillon_2025_debit <- Paillon_2025_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )


# group all data ----------------------------------------------------------

liste_Paillon_all_debit <-list(Paillon_2012_debit, Paillon_2013_debit, Paillon_2014_debit, Paillon_2015_debit,
     Paillon_2016_debit, Paillon_2017_debit, Paillon_2018_debit, Paillon_2019_debit,
     Paillon_2020_debit, Paillon_2021_debit, Paillon_2022_debit, Paillon_2023_debit,
     Paillon_2024_debit, Paillon_2025_debit)

Paillon_all_debit <- bind_rows(liste_Paillon_all_debit)

Paillon_all_debit <- Paillon_all_debit %>% 
  rename(date = "jour")

save(Paillon_all_debit, file = "data/MNCA/Paillon_all_debit.Rdata")
