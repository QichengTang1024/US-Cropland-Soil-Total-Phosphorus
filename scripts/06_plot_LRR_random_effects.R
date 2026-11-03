source("scripts/00_setup.R")

# Supplemental figure: land resource region random effects.
   library(sf)
   library(dplyr)
   library(tibble)
   library(ggplot2)
   library(ggpattern)
   library(viridis)
   
# Read and prepare LRR polygons
   
   lrr_data <- st_read(
     "data/gis/MLRA_52_2022/MLRA_52_2022/MLRA_52.shp"
   )
   
   LRR_T5 <- ranef(T5_long_refit)$LRR
   
   lrr_val_df <- LRR_T5 %>%
     as.data.frame() %>%
     tibble::rownames_to_column(
       var = "LRRSYM"
     ) %>%
     rename(
       Random_effect = `(Intercept)`
     )
   
   conus_bbox <- st_bbox(
     c(
       xmin = -125,
       xmax = -66,
       ymin = 24,
       ymax = 50
     ),
     crs = st_crs(lrr_data)
   )
   
   lrr_conus <- st_crop(
     lrr_data,
     conus_bbox
   )
   
   lrr_merged <- lrr_conus %>%
     
     group_by(
       LRRSYM
     ) %>%
     
     summarise(
       geometry = st_union(geometry),
       .groups = "drop"
     ) %>%
     
     st_as_sf()
   
# Join fitted LRR effects; distinguish unsampled regions.
   
   lrr_merged <- lrr_merged %>%
     
     left_join(
       lrr_val_df,
       by = "LRRSYM"
     )
   
# Plot and export LRR map
   
   p_LRR <- ggplot() +
     
   geom_sf(
     data = filter(
       lrr_merged,
       !is.na(Random_effect)
     ),
     
     aes(
       fill = Random_effect
     ),
     
     color = "white",
     linewidth = 0.25
   ) +
     
   geom_sf_pattern(
     data = filter(
       lrr_merged,
       is.na(Random_effect)
     ),
     
     pattern = "stripe",
     
     fill = "grey90",
     pattern_fill = "grey45",
     pattern_colour = "grey45",
     
     pattern_angle = 45,
     pattern_density = 0.35,
     pattern_spacing = 0.025,
     
     color = "white",
     linewidth = 0.25
   ) +
     
   scale_fill_viridis_c(
     option = "C",
     
     name = expression(
       "LRR random effect (" *
         log[10] ~ mg ~ kg^{-1} *
         ")"
     )
   ) +
     
   coord_sf(
     xlim = c(-125, -66),
     ylim = c(24, 50),
     expand = FALSE
   ) +
     
   labs(
     x = NULL,
     y = NULL
   ) +
     
   theme_void(
     base_size = 12
   ) +
     
     theme(
       
       legend.position = "right",
       
       legend.title = element_text(
         size = 12,
         face = "bold"
       ),
       
       legend.text = element_text(
         size = 11
       )
     )
   
   print(p_LRR)
   
   ggsave(
     filename = "outputs/figures/Figure_LRR_random_effects_TPsurf.png",
     plot = p_LRR,
     width = 8.5,
     height = 5.5,
     dpi = 300,
     bg = "white"
   )
