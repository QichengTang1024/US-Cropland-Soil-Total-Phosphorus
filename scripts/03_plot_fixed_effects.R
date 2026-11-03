source("scripts/00_setup.R")

# Figure 2: effect of one SD increase in each transformed predictor.
  
  library(lme4)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
# Standardization: coefficient * SD(predictor) on fitted model rows.
  one_sd_effects <- function(model) {
    coefs <- lme4::fixef(model)
    predictors <- setdiff(names(coefs), "(Intercept)")
    fitted_rows <- model.frame(model)
    coefs[predictors] <- coefs[predictors] *
      vapply(fitted_rows[predictors], stats::sd, numeric(1))
    coefs
  }

  coef_TPsurf <- one_sd_effects(T5_long_refit)
  coef_TPdiff <- one_sd_effects(T5C_long_refit)
  
  df_TPsurf <- data.frame(
    Variable = names(coef_TPsurf),
    TPsurf   = as.numeric(coef_TPsurf)
  ) |>
    filter(Variable != "(Intercept)")
  
  df_TPdiff <- data.frame(
    Variable = names(coef_TPdiff),
    TPdiff   = as.numeric(coef_TPdiff)
  ) |>
    filter(Variable != "(Intercept)")
  
  plot_data <- full_join(
    df_TPsurf,
    df_TPdiff,
    by = "Variable"
  ) |>
    pivot_longer(
      cols = c(TPsurf, TPdiff),
      names_to = "Model",
      values_to = "Effect"
    ) |>
    mutate(
      
      Variable = dplyr::recode(
        Variable,
        "Org_C" = "OC"
      ),
      
      Model = factor(
        Model,
        levels = c("TPsurf", "TPdiff")
      )
    )
  
# Predictor display order
  
  desired_order <- c(
    "Ca",
    "Fe",
    "OC",
    "pH",
    "SpP",
    "MnP",
    "FrP"
  )
  
  plot_data <- plot_data |>
    
    filter(
      Variable %in% desired_order
    ) |>
    
    mutate(
      Variable = factor(
        Variable,
        levels = desired_order
      )
    )
  
  effect_max <- max(
    plot_data$Effect,
    na.rm = TRUE
  )
  
  effect_min <- min(
    plot_data$Effect,
    na.rm = TRUE
  )
  
  upper_limit <- ifelse(
    effect_max > 0,
    effect_max * 1.25,
    0.02
  )
  
  lower_limit <- ifelse(
    effect_min < 0,
    effect_min * 1.18,
    0
  )
  
  label_y <- effect_max * 1.14
  
# Plot standardized fixed effects
  
  p2 <- ggplot(
    plot_data,
    aes(
      x = Variable,
      y = Effect,
      fill = Model
    )
  ) +
    
  geom_hline(
    yintercept = 0,
    linewidth = 0.45,
    colour = "black"
  ) +
    
  geom_col(
    position = position_dodge2(
      width = 0.72,
      preserve = "single",
      padding = 0.08
    ),
    width = 0.55,
    na.rm = TRUE
  ) +
    
  geom_vline(
    xintercept = 4.5,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "grey55"
  ) +
    
  annotate(
    geom = "text",
    x = 2.5,
    y = label_y,
    label = "Geochemical",
    fontface = "bold",
    size = 4.2
  ) +
    
    annotate(
      geom = "text",
      x = 6,
      y = label_y,
      label = "Agricultural inputs",
      fontface = "bold",
      size = 4.2
    ) +
    
  scale_x_discrete(
    labels = c(
      "Ca"  = "Ca",
      "Fe"  = "Fe",
      "OC"  = "OC",
      "pH"  = "pH",
      "SpP" = "SpP",
      "MnP" = "MaP",
      "FrP" = "FrP"
    )
  ) +
    
  scale_fill_manual(
    values = c(
      "TPsurf" = "#9ECAE1",
      "TPdiff" = "#FC9272"
    ),
    
    breaks = c(
      "TPsurf",
      "TPdiff"
    ),
    
    labels = c(
      "TPsurf" = "TPsurf",
      "TPdiff" = "TPdiff"
    ),
    
    name = "Model"
  ) +
    
  scale_y_continuous(
    limits = c(
      lower_limit,
      upper_limit
    ),
    
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
    
  labs(
    x = "Predictor variable",
    
    y = expression(
      Delta * TP * "," ~
        log[10] * "(mg " * kg^{-1} * ")" 
    )
  ) +
    
    coord_cartesian(
      clip = "off"
    ) +
    
  theme_classic(
    base_size = 10
  ) +
    
    theme(
      
      axis.text.x = element_text(
        size = 11,
        face = "bold"
      ),
      
      axis.text.y = element_text(
        size = 10
      ),
      
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        margin = margin(t = 6)
      ),
      
      axis.title.y = element_text(
        size = 11,
        face = "bold",
        margin = margin(r = 7)
      ),
      
      axis.line = element_line(
        linewidth = 0.45,
        colour = "black"
      ),
      
      axis.ticks = element_line(
        linewidth = 0.4,
        colour = "black"
      ),
      
      panel.grid = element_blank(),
      
      legend.position = "right",
      
      legend.text = element_text(
        size = 10
      ),
      
      legend.title = element_text(
        size = 10,
        face = "bold"
      ),
      
      plot.margin = margin(
        t = 8,
        r = 8,
        b = 7,
        l = 7
      )
    )
  
# Export Figure 2
  
  print(p2)
  
  ggsave(
    filename = "outputs/figures/Figure2_effect_sizes_with_pH.png",
    plot = p2,
    width = 5.8,
    height = 3.9,
    dpi = 300,
    bg = "white"
  )
