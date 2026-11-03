source("scripts/00_setup.R")

# Figure 3: state random effects for both responses.

library(usmap)
library(tidyverse)
library(sf)
library(ggpattern)
library(ggrepel)
library(patchwork)
library(scales)

# State polygons and farm production regions

FPR_df <- read.csv("data/reference/FPR.csv")

states_map <- usmap::us_map("states")

states_map <- states_map %>%
  left_join(
    FPR_df,
    by = c("abbr" = "States")
  )

states_conus <- states_map %>%
  filter(
    !abbr %in% c("AK", "HI")
  )

fpr_conus <- states_conus %>%
  group_by(FPR) %>%
  summarise(
    geometry = st_union(geom),
    .groups = "drop"
  ) %>%
  st_as_sf()

# Extract state effects from fitted models

T5_states <- ranef(T5_long_refit)$State

T5C_states <- ranef(T5C_long_refit)$State

T5_df <- T5_states %>%
  rownames_to_column(
    var = "abbr"
  ) %>%
  rename(
    value = `(Intercept)`
  )

T5C_df <- T5C_states %>%
  rownames_to_column(
    var = "abbr"
  ) %>%
  rename(
    value = `(Intercept)`
  )

states_conus_t5 <- left_join(
  states_conus,
  T5_df,
  by = "abbr"
)

states_conus_t5c <- left_join(
  states_conus,
  T5C_df,
  by = "abbr"
)

# Use a shared fill scale for direct panel comparison.

shared_limits <- range(
  c(
    states_conus_t5$value,
    states_conus_t5c$value
  ),
  na.rm = TRUE
)

states_conus_t5 <- states_conus_t5 %>%
  mutate(
    
    scaled_value = scales::rescale(
      value,
      from = shared_limits
    ),
    
    label_color = case_when(
      
      is.na(value) ~ "black",
      
      scaled_value < 0.35 ~ "white",
      
      TRUE ~ "black"
    )
  )

states_conus_t5c <- states_conus_t5c %>%
  mutate(
    
    scaled_value = scales::rescale(
      value,
      from = shared_limits
    ),
    
    label_color = case_when(
      
      is.na(value) ~ "black",
      
      scaled_value < 0.35 ~ "white",
      
      TRUE ~ "black"
    )
  )

# Move crowded Northeast state labels outside the map.

crowded_states <- c(
  "MD",
  "DE",
  "NJ",
  "CT",
  "RI",
  "MA"
)

# Map function and panel assembly

make_state_map <- function(
    map_data,
    panel_label
) {
  
  label_sf <- map_data
  
  st_geometry(label_sf) <- st_point_on_surface(
    st_geometry(map_data)
  )
  
  label_xy <- st_coordinates(
    label_sf
  )
  
  label_df <- bind_cols(
    
    st_drop_geometry(
      label_sf
    ),
    
    tibble(
      x = label_xy[, 1],
      y = label_xy[, 2]
    )
  )
  
  normal_labels <- label_sf %>%
    filter(
      !abbr %in% crowded_states
    )
  
  crowded_labels <- label_df %>%
    filter(
      abbr %in% crowded_states
    )
  
  map_bbox <- st_bbox(
    map_data
  )
  
  map_width <- as.numeric(
    map_bbox["xmax"] -
      map_bbox["xmin"]
  )
  
  label_nudge <- map_width * 0.035
  
  ggplot() +
    
  geom_sf(
    data = filter(
      map_data,
      !is.na(value)
    ),
    aes(
      fill = value
    ),
    color = "white",
    linewidth = 0.25
  ) +
    
  geom_sf_pattern(
    data = filter(
      map_data,
      is.na(value)
    ),
    fill = "lightgray",
    pattern = "stripe",
    pattern_fill = "gray40",
    pattern_angle = 45,
    pattern_density = 0.5,
    pattern_spacing = 0.02,
    color = "white",
    linewidth = 0.25
  ) +
    
  geom_sf(
    data = fpr_conus,
    fill = NA,
    color = "black",
    linewidth = 1.2
  ) +
    
  geom_sf_text(
    data = normal_labels,
    
    aes(
      label = abbr,
      color = label_color
    ),
    
    size = 3.2,
    
    fontface = "bold",
    
    show.legend = FALSE
  ) +
    
  ggrepel::geom_text_repel(
    data = crowded_labels,
    
    aes(
      x = x,
      y = y,
      label = abbr
    ),
    
    nudge_x = label_nudge,
    
    direction = "y",
    
    hjust = 0,
    
    size = 3.2,
    fontface = "bold",
    color = "black",
    
    box.padding = 0.30,
    point.padding = 0.12,
    force = 2,
    
    max.overlaps = Inf,
    
    min.segment.length = 0,
    segment.color = "black",
    segment.linewidth = 0.4,
    
    seed = 123
  ) +
    
  scale_color_identity() +
    
  scale_fill_viridis_c(
    option = "C",
    limits = shared_limits,
    
    name = expression(
      alpha[State] ~
        "(log"[10] ~ "mg/kg)"
    )
  ) +
    
  coord_sf(
    xlim = c(
      map_bbox["xmin"],
      map_bbox["xmax"] +
        map_width * 0.065
    ),
    expand = FALSE,
    clip = "off"
  ) +
    
  labs(
    tag = panel_label
  ) +
    
  theme_void() +
    
    theme(
      
      plot.tag = element_text(
        size = 11,
        face = "bold"
      ),
      
      plot.tag.position = c(
        0.01,
        0.99
      ),
      
      legend.position = "right",
      legend.title = element_text(
        size = 11
      ),
      
      legend.text = element_text(
        size = 10
      ),
      
      plot.margin = margin(
        t = 0,
        r = 6,
        b = 0,
        l = 0
      )
    )
}

p_a <- make_state_map(
  states_conus_t5,
  "(a)"
)

p_b <- make_state_map(
  states_conus_t5c,
  "(b)"
)

Figure_state <- p_a / p_b +
  
  plot_layout(
    heights = c(
      1,
      1
    )
  ) 
  
Figure_state

ggsave(
  filename = "outputs/figures/Figure_state_random_effects.png",
  plot = Figure_state,
  width = 7.2,
  height = 7.5,
  dpi = 300,
  bg = "white"
)
