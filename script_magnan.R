## Analyse temporelle des crues du Magnan pour la période entre 2013 et 2025

# 13/03/2026
# Alice Briand

# Load libraries

library(dplyr)
library(readr)
library(timeSeries)
library(lubridate)

# loading files -----------------------------------------------------------

Magnan_2014 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012014-000000_01012015-000000.csv",
                           delim = ";", locale = locale(decimal_mark = ","))
Magnan_2015 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012015-000000_01012016-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2016 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012016-000000_01012017-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2017 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012017-000000_01012018-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2018 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012018-000000_01012019-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2019 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012019-000000_08012020-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2020 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012020-000000_08012021-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2021 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012021-000000_01012022-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2022 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012022-000000_01012023-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2023 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012023-000000_01012024-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2024 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012024-000000_01012025-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))
Magnan_2025 <- read_delim("P:/Stage/River_runoff_analysis/Rapports annuels/Magnan.csv/Rapport_MAGNAN_PERIODE_01012025-000000_01012026-000000.csv",
                          delim = ";", locale = locale(decimal_mark = ","))

# 2014 --------------------------------------------------------------------

colnames(Magnan_2014) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit", "COS_cumul_pluie", "LABEL_cumul_pluie")

Magnan_2014 <- Magnan_2014 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2014_debit <- Magnan_2014 %>%
  select("date_heure", "AAM_debit")

Magnan_2014_debit <- Magnan_2014_debit[-c(1:3), ]

Magnan_2014_debit <- Magnan_2014_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2014_debit <- Magnan_2014_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2014_debit <- Magnan_2014_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2015 --------------------------------------------------------------------

colnames(Magnan_2015) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit", "COS_cumul_pluie", "LABEL_cumul_pluie",
                           "NA")

Magnan_2015 <- Magnan_2015 %>%
  mutate(date_heure = parse_datetime(date_heure, 
         format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2015_debit <- Magnan_2015 %>%
  select("date_heure", "AAM_debit")

Magnan_2015_debit <- Magnan_2015_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2015_debit <- Magnan_2015_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2015_debit <- Magnan_2015_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2016 --------------------------------------------------------------------

colnames(Magnan_2016) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2016 <- Magnan_2016 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2016_debit <- Magnan_2016 %>%
  select("date_heure", "AAM_debit")

Magnan_2016_debit <- Magnan_2016_debit[-c(1:3), ]

Magnan_2016_debit <- Magnan_2016_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2016_debit <- Magnan_2016_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2016_debit <- Magnan_2016_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2017 --------------------------------------------------------------------

colnames(Magnan_2017) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2017 <- Magnan_2017 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2017_debit <- Magnan_2017 %>%
  select("date_heure", "AAM_debit")

Magnan_2017_debit <- Magnan_2017_debit[-c(1:3), ]

Magnan_2017_debit <- Magnan_2017_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2017_debit <- Magnan_2017_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2017_debit <- Magnan_2017_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2018 --------------------------------------------------------------------

colnames(Magnan_2018) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2018 <- Magnan_2018 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2018_debit <- Magnan_2018 %>%
  select("date_heure", "AAM_debit")

Magnan_2018_debit <- Magnan_2018_debit[-c(1:3), ]

Magnan_2018_debit <- Magnan_2018_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2018_debit <- Magnan_2018_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2018_debit <- Magnan_2018_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2019 --------------------------------------------------------------------

colnames(Magnan_2019) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2019 <- Magnan_2019 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2019_debit <- Magnan_2019 %>%
  select("date_heure", "AAM_debit")

Magnan_2019_debit <- Magnan_2019_debit[-c(1:3), ]

Magnan_2019_debit <- Magnan_2019_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2019_debit <- Magnan_2019_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2019_debit <- Magnan_2019_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2020 --------------------------------------------------------------------

colnames(Magnan_2020) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2020 <- Magnan_2020 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2020_debit <- Magnan_2020 %>%
  select("date_heure", "AAM_debit")

Magnan_2020_debit <- Magnan_2020_debit[-c(1:3), ]

Magnan_2020_debit <- Magnan_2020_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2020_debit <- Magnan_2020_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2020_debit <- Magnan_2020_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2021 --------------------------------------------------------------------

colnames(Magnan_2021) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2021 <- Magnan_2021 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2021_debit <- Magnan_2021 %>%
  select("date_heure", "AAM_debit")

Magnan_2021_debit <- Magnan_2021_debit[-c(1:3), ]

Magnan_2021_debit <- Magnan_2021_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2021_debit <- Magnan_2021_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2021_debit <- Magnan_2021_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2022 --------------------------------------------------------------------

colnames(Magnan_2022) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2022 <- Magnan_2022 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2022_debit <- Magnan_2022 %>%
  select("date_heure", "AAM_debit")

Magnan_2022_debit <- Magnan_2022_debit[-c(1:3), ]

Magnan_2022_debit <- Magnan_2022_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2022_debit <- Magnan_2022_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2022_debit <- Magnan_2022_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2023 --------------------------------------------------------------------

colnames(Magnan_2023) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2023 <- Magnan_2023 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2023_debit <- Magnan_2023 %>%
  select("date_heure", "AAM_debit")

Magnan_2023_debit <- Magnan_2023_debit[-c(1:3), ]

Magnan_2023_debit <- Magnan_2023_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2023_debit <- Magnan_2023_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2023_debit <- Magnan_2023_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2024 --------------------------------------------------------------------

colnames(Magnan_2024) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2024 <- Magnan_2024 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2024_debit <- Magnan_2024 %>%
  select("date_heure", "AAM_debit")

Magnan_2024_debit <- Magnan_2024_debit[-c(1:3), ]

Magnan_2024_debit <- Magnan_2024_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2024_debit <- Magnan_2024_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2024_debit <- Magnan_2024_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2025 --------------------------------------------------------------------

colnames(Magnan_2025) <- c("date_heure","AAM_niveau_eau","LABEL_niveau_eau_1", "LABEL_niveau_eau_2",
                           "LABEL_niveau_eau_3", "LABEL_niveau_eau_4", "LABEL_niveau_eau_5", "LABEL_niveau_eau_6" ,"altitude_AAM",
                           "LABEL_altitude", "AAM_debit", "LABEL_debit_1", "LABEL_debit_2",
                           "LABEL_debit_3", "COS_cumul_pluie", "LABEL_cumul_pluie_1", "LABEL_cumul_pluie_2",
                           "LABEL_cumul_pluie_3", "NA_1", "NA_2", "NA_3")

Magnan_2025 <- Magnan_2025 %>%
  mutate(date_heure = parse_datetime(date_heure, 
                                     format = "%d/%m/%Y %H:%M:%S", na = c("", "NA")))

Magnan_2025_debit <- Magnan_2025 %>%
  select("date_heure", "AAM_debit")

Magnan_2025_debit <- Magnan_2025_debit[-c(1:3), ]

Magnan_2025_debit <- Magnan_2025_debit %>%
  mutate(
    across(ends_with("_debit"),
           ~ as.numeric(gsub(",", ".", .)))
  )

Magnan_2025_debit <- Magnan_2025_debit %>%
  dplyr::filter(!if_any(everything(), ~ .x %in% c(0, 500)))

Magnan_2025_debit <- Magnan_2025_debit %>%
  dplyr::select(date_heure, dplyr::ends_with("_debit")) %>%
  dplyr::filter(!is.na(.data$date_heure)) %>%
  dplyr::mutate(jour = lubridate::floor_date(.data$date_heure, "day")) %>%
  dplyr::group_by(jour) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_debit"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mad =  ~ mad(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# group all data ----------------------------------------------------------

liste_Magnan_all_debit <-list(Magnan_2014_debit, Magnan_2015_debit, Magnan_2016_debit,
                              Magnan_2017_debit, Magnan_2018_debit, Magnan_2019_debit,
                              Magnan_2020_debit, Magnan_2021_debit, Magnan_2022_debit,
                              Magnan_2023_debit, Magnan_2024_debit, Magnan_2025_debit)

Magnan_all_debit <- bind_rows(liste_Magnan_all_debit)

Magnan_all_debit <- Magnan_all_debit %>% 
  rename(date = "jour")

save(Magnan_all_debit, file = "P:/Stage/River_runoff_analysis/data/MNCA/Magnan_all_debit.Rdata")
