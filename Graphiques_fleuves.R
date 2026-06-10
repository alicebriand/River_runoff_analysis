# Directory : River_runoff_analysis/Graphiques_fleuves
# 26/05/2026

# Ce script cartographie les différents fleuves français

# Libraries
library(tidyverse)
library(sf)
library(ggspatial)   
library(ggrepel)    
library(patchwork)
library(magick)
library(tidyterra)
library(terra)
library(giscoR)

# Load high-res shape files
# Tuto here : https://www.etiennebacher.com/posts/2021-12-27-mapping-french-rivers-network/
if(!exists("borders_FR")) borders_FR <- read_sf("data/River_flow_ressources/FRANCE_shapefile/")
if(!exists("rivers_FR")) rivers_FR <- st_intersection(read_sf("data/River_flow_ressources/HydroRIVERS_v10_eu_shp/"), borders_FR)

ggplot() +
  geom_sf(data = borders_FR, color = "black", fill = "lightgoldenrod", inherit.aes = FALSE) +
  geom_sf(data = rivers_FR, color = "steelblue", inherit.aes = FALSE, linewidth = 0.2) +
  coord_sf(xlim = c(6.8, 7.45), ylim = c(43.15, 43.8))

# plotting ----------------------------------------------------------------

coastline_giscoR <- gisco_get_coastallines(resolution = "01")
countries_giscoR  <- gisco_get_countries(region = "Europe", resolution = "01")

# on définit les deux zones d'études
make_box <- function(xmin, xmax, ymin, ymax, crs = 4326) {
  coords <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)
  st_sfc(st_polygon(list(coords)), crs = crs)
}

box_entière     <- make_box(6.8925000, 7.4200000, 43.2136389, 43.7300000)
box_paillon <- make_box(7.21000000, 7.36000000, 43.60000000, 43.73000000)

# on isole les fleuves d'intérêts

target_rivers <- c("Var", "Paillon", "Magnan")

rivers_highlight <- rivers_FR |>
  filter(if_any(where(is.character), ~ str_detect(., regex(paste(target_rivers, collapse = "|"), ignore_case = TRUE))))

# ── Palette ──────────────────────────────────────────────────────────────────
col_sea    <- "#a8d8ea"   # bleu mer
col_land   <- "lightgoldenrod"
col_rivers <- "steelblue"
col_hi     <- "#003f7f"   # bleu foncé fleuves cibles
col_box1   <- "#e05c1a"   # orange zone 1
col_box2   <- "#9b2d8a"   # violet zone 2

# On zoome sur la bbox de la zone + un petit buffer
zone_focus  <- box_entière
buffer_deg  <- 0.05   # ~5 km
bbox_focus  <- st_bbox(zone_focus)

p_main <- ggplot() +
  theme(panel.background = element_rect(fill = col_sea)) +
  geom_sf(data = borders_FR, color = "black", fill = col_land, inherit.aes = FALSE) +
  geom_sf(data = rivers_FR,  color = col_rivers, inherit.aes = FALSE, linewidth = 0.35, alpha = 0.7) +
  geom_sf(data = rivers_highlight, color = col_hi, inherit.aes = FALSE, linewidth = 1) +
  # Rectangle zone d'étude
  geom_sf(data = zone_focus, fill = col_box1, color = col_box1,
          alpha = 0.12, linewidth = 1.4, linetype = "dashed") +
  coord_sf(
    xlim = c(bbox_focus["xmin"] - buffer_deg, bbox_focus["xmax"] + buffer_deg),
    ylim = c(bbox_focus["ymin"] - buffer_deg, bbox_focus["ymax"] + buffer_deg)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  labs(
    title    = "Zone d'étude"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = col_sea, color = NA),
    panel.grid       = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    axis.title       = element_blank()
  )

# Coordonnées de la zone zoomée
box_zoom <- make_box(7.110978, 7.360000, 43.523182, 43.730000)
bbox_zoom <- st_bbox(box_zoom)

# Graphique zoomé
p_zoom <- ggplot() +
  geom_sf(data = borders_FR, color = "black", fill = col_land, inherit.aes = FALSE) +
  geom_sf(data = rivers_FR,  color = col_rivers, inherit.aes = FALSE, 
          linewidth = 0.35, alpha = 0.7) +
  geom_sf(data = rivers_highlight, color = col_hi, inherit.aes = FALSE, linewidth = 1.2) +
  # Labels des fleuves
  # geom_sf_label(data = rivers_highlight,
  #               aes(label = NAME),   # ← adapte le nom de la colonne si besoin
  #               size = 3, color = col_hi, fill = "white",
  #               label.padding = unit(0.15, "lines")) +
  coord_sf(
    xlim = c(bbox_zoom["xmin"], bbox_zoom["xmax"]),
    ylim = c(bbox_zoom["ymin"], bbox_zoom["ymax"])
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(title = "Zoom — Embouchures") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = col_sea, color = NA),
    panel.grid       = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    axis.title       = element_blank()
  )

# Assemblage
p_main | p_zoom

# autre méthode -----------------------------------------------------------

# Lister tous les fichiers du dossier
list.files("~/Downloads/S2B_MSIL2A_20201003T101759_N0500_R065_T32TLP_20230412T082558.SAFE(1)/", recursive = TRUE)

list.files("~/Downloads/S2B_MSIL2A_20201003T101759_N0500_R065_T32TLP_20230412T082558.SAFE(1)/", recursive = TRUE, full.names = TRUE)

# Chemin vers le fichier TCI
tci_path <- "~/Downloads/S2B_MSIL2A_20201003T101759_N0500_R065_T32TLP_20230412T082558.SAFE(1)/S2B_MSIL2A_20201003T101759_N0500_R065_T32TLP_20230412T082558.SAFE/GRANULE/L2A_T32TLP_A018681_20201003T102147/IMG_DATA/R10m/T32TLP_20201003T101759_TCI_10m.jp2"

# Charger le raster
tci <- rast(tci_path)

# Vérifier le système de coordonnées
crs(tci)
ext(tci)

# Recadrer sur ta zone d'intérêt (en WGS84 d'abord)
zone <- ext(6.8925000, 7.4200000, 43.2736389, 43.7300000)


# Reprojeter la zone dans le CRS du raster (UTM 32N)
zone_sf <- st_bbox(c(xmin = 6.8925000, xmax = 7.4200000,
                     ymin = 43.2736389, ymax = 43.7300000),
                   crs = 4326) |>
  st_as_sfc() |>
  st_transform(crs(tci))

# Recadrer et reprojeter en WGS84
tci_crop_main <- tci |>
  crop(zone_sf) |>
  project("EPSG:4326")

p_main <- ggplot() +
  geom_spatraster_rgb(data = tci_crop_main) +
  coord_sf(xlim   = c(6.8925000, 7.4200000),
           ylim   = c(43.2736389, 43.7300000),
           expand = FALSE,
           datum  = sf::st_crs(4326)) +
  annotation_scale(
    location   = "bl",
    width_hint = 0.3,
    bar_cols   = c("white", "black"),  # ← barres blanc/noir visibles sur fond sombre
    text_col   = "white",              # ← texte blanc
    line_col   = "white"               # ← bordure blanche
  ) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(x = "Longitude (°E)", y = "Latitude (°N)") +
  theme_bw(base_size = 15) +
  theme(axis.title = element_text(face = "bold"))

# Recadrer sur ta zone d'intérêt (en WGS84 d'abord)
zone <- ext(7.110978, 7.360000, 43.523182, 43.730000)

# Reprojeter la zone dans le CRS du raster (UTM 32N)
zone_sf <- st_bbox(c(xmin = 7.110978, xmax = 7.360000,
                     ymin = 43.523182, ymax = 43.730000),
                   crs = 4326) |>
  st_as_sfc() |>
  st_transform(crs(tci))

# Recadrer et reprojeter en WGS84
tci_crop_zoom <- tci |>
  crop(zone_sf) |>
  project("EPSG:4326")

p_zoom <- ggplot() +
  geom_spatraster_rgb(data = tci_crop_zoom) +
  coord_sf(xlim   = c(7.110978, 7.360000),
           ylim   = c(43.523182, 43.730000),
           expand = FALSE,
           datum  = sf::st_crs(4326)) +
  annotation_scale(
    location   = "bl",
    width_hint = 0.3,
    bar_cols   = c("white", "black"),  # ← barres blanc/noir visibles sur fond sombre
    text_col   = "white",              # ← texte blanc
    line_col   = "white"               # ← bordure blanche
  ) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(x       = "Longitude (°E)",
       y       = "Latitude (°N)",
       caption = "Source : Copernicus/ESA — MSI L2A") +
  theme_bw(base_size = 15) +
  theme(
    axis.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 15, color = "black", hjust = 0)
  )

# Assemblage
(p_main | p_zoom) +
  plot_annotation(
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 13),
      plot.caption = element_text(color = "grey50", size = 10, hjust = 0)
    )
  )
