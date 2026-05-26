# Directory : River_runoff_analysis/Graphiques_fleuves
# 26/05/2026

# Ce script cartographie les différents fleuves français

# Libraries
library(tidyverse)
library(sf)
library(ggspatial)   
library(ggrepel)    

# Load high-res shape files
# Tuto here : https://www.etiennebacher.com/posts/2021-12-27-mapping-french-rivers-network/
if(!exists("borders_FR")) borders_FR <- read_sf("data/River_flow_ressources/FRANCE_shapefile/")
if(!exists("rivers_FR")) rivers_FR <- st_intersection(read_sf("data/River_flow_ressources/HydroRIVERS_v10_eu_shp/"), borders_FR)

ggplot() +
  geom_sf(data = borders_FR, color = "black", fill = "lightgoldenrod", inherit.aes = FALSE) +
  geom_sf(data = rivers_FR, color = "steelblue", inherit.aes = FALSE, linewidth = 0.2) +
  coord_sf(xlim = c(6.8, 7.45), ylim = c(43.15, 43.8))

# plotting ----------------------------------------------------------------

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

# une avec les deux zones d'études () : 

ggplot() +
  # Fond mer (couleur unie si pas de tuiles dispo hors-ligne)
  theme(panel.background = element_rect(fill = col_sea)) +
  # Terre
  geom_sf(data = borders_FR, color = "black", fill = col_land, inherit.aes = FALSE) +
  # Réseau hydrographique
  geom_sf(data = rivers_FR, color = col_rivers, inherit.aes = FALSE, linewidth = 0.2, alpha = 0.7) +
  # Fleuves cibles en bleu foncé
  geom_sf(data = rivers_highlight, color = col_hi, inherit.aes = FALSE, linewidth = 0.8) +
  # Zones d'étude
  geom_sf(data = box_entière,     fill = NA, color = col_box1, linewidth = 1.2, linetype = "dashed") +
  geom_sf(data = box_paillon, fill = NA, color = col_box2, linewidth = 1.2, linetype = "dashed") +
  # Labels des zones
  annotate("label", x = 6.925, y = 43.76, label = "Zone entière",
           color = col_box1, size = 4, fontface = "bold", fill = "white", label.size = 0) +
  annotate("label", x = 7.325, y = 43.76, label = "Zone Paillon / Magnan",
           color = col_box2, size = 4, fontface = "bold", fill = "white", label.size = 0) +
  # Emprise globale
  coord_sf(xlim = c(6.8, 7.45), ylim = c(43.15, 43.8)) +
  # Échelle + flèche nord
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  labs(
    title    = "Zones d'études",
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = col_sea, color = NA),
    panel.grid       = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    axis.title       = element_blank()
  )

# une avec une seule zone d'étude

# On zoome sur la bbox de la zone + un petit buffer
zone_focus  <- box_entière
buffer_deg  <- 0.05   # ~5 km
bbox_focus  <- st_bbox(zone_focus)

ggplot() +
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

# ajouter un carré pour définir la zone d'étude
# ajouter un fond version mer

