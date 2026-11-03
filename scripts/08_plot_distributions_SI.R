# Supplemental: distributions before and after transformation.
source("scripts/00_setup.R")

library(dplyr)
library(tidyr)
library(ggplot2)

Input_Data <- readRDS("data/processed/Input_Data.rds")

Input_Data$TP_diff <- Input_Data$Top5_P - Input_Data$C_P

# Apply the same three-site exclusion as the main models.

n_extreme_negative <- sum(Input_Data$TP_diff <= -500, na.rm = TRUE)

message("Number of observations removed: ", n_extreme_negative)

Input_Data <- Input_Data %>%
  filter(is.na(TP_diff) | TP_diff > -500)

# Histogram plotting function

plot_histograms <- function(data, figure_title){
  
  Plot_Data <- data %>%
    transmute(
      TPsurf       = Top5_P,
      TPdiff       = TP_diff,
      Al           = Top5_Al,
      Ca           = Top5_Ca,
      Fe           = Top5_Fe,
      TC           = A_C_Tot,
      IC           = A_C_Inorg,
      OC           = A_C_Org,
      `P Surplus`  = Psurplus_l,
      Fertilizer   = Fertilizer_l,
      Manure       = Manure_l
    ) %>%
    pivot_longer(
      everything(),
      names_to = "Variable",
      values_to = "Value"
    )
  
  ggplot(Plot_Data,
         aes(x = Value,
             y = after_stat(density))) +
    
    geom_histogram(
      bins = 30,
      colour = "black",
      fill = "grey75"
    ) +
    
    facet_wrap(
      ~Variable,
      scales = "free",
      ncol = 3
    ) +
    
    labs(
      x = NULL,
      y = "Density",
      title = figure_title
    ) +
    
    theme_bw(base_size = 11) +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      strip.text = element_text(
        face = "bold"
      ),
      panel.grid = element_blank()
    )
  
}

Fig_Before <- plot_histograms(
  Input_Data,
  "Before Transformation"
)

ggsave(
  "outputs/figures/FigS_Before_Transformation.tiff",
  Fig_Before,
  width = 10,
  height = 9,
  dpi = 300,
  compression = "lzw"
)

# Apply model transformations

Input_Data <- Input_Data %>%
  mutate(across(
    c(Psurplus_l, Fertilizer_l, Manure_l,
      Psurplus_m, Fertilizer_m, Manure_m,
      Psurplus_s, Fertilizer_s, Manure_s),
    ~ . * 1.12
  )) %>%
  
  mutate(across(
    c(Top5_Al, Top5_Ca, Top5_Fe,
      A_C_Tot, A_C_Inorg, A_C_Org, pH),
    ~ log10(. + 0.1)
  )) %>%
  
  mutate(across(
    c(Psurplus_l, Psurplus_m, Psurplus_s),
    ~ log10(. + 1000)
  )) %>%
  
  mutate(across(
    c(Fertilizer_l, Manure_l,
      Fertilizer_m, Manure_m,
      Fertilizer_s, Manure_s),
    ~ log10(.)
  )) %>%
  
  mutate(
    Top5_P = log10(Top5_P),
    TP_diff = log10(TP_diff + 500)
  )

Fig_After <- plot_histograms(
  Input_Data,
  "After Transformation"
)

ggsave(
  "outputs/figures/FigS_After_Transformation.tiff",
  Fig_After,
  width = 10,
  height = 9,
  dpi = 300,
  compression = "lzw"
)
