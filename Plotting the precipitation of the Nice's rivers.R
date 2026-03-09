# Plotting the precipitation of the Nice's rivers

# pathway : "~/Documents/Alice/code/Plotting the precipitation of the Nice's rivers

# libraries ---------------------------------------------------------------

library(ggplot2)
library(scales)

# loading -----------------------------------------------------------------

load("Hydro France/Y6442010_Hydro.Rdata")

load("Rapports_annuels/Paillon.csv/Paillon_all_pluie.Rdata")

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

# plotting precipitation ----------------------------------------------------------------

# plotting without an adjustment

ggplot() +
  geom_line(data = Paillon_all_pluie, aes(x = Date, y = pluie_moyen)) +
  scale_x_date(
    breaks = "1 year",  # Affiche une étiquette par an
    date_labels = "%b %Y",  # Format : "Mois Année" (ex. "Jan 2020")
    minor_breaks = "1 month"  # Affiche des petites graduations pour les mois
  ) +
  labs(
    title = "Précipitation moyenne sur le Paillon entre 2012 à 2026",
    y = "précipitation moyenne",
    x = "Date"
  ) +
  theme_minimal()


