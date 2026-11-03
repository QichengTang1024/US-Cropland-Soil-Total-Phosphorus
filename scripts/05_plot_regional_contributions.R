source("scripts/00_setup.R")

# Figure 4: regional fixed- and random-effect contributions.
  
  library(lme4)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  
# Farm Production Regions
  
  add_region <- function(df) {
    
    df %>%
      mutate(
        
        State = as.character(State),
        
        Region = case_when(
          
          State %in% c(
            "WA", "MT", "OR", "ID", "WY",
            "CA", "NV", "UT", "CO", "AZ", "NM"
          ) ~ "West",
          
          State %in% c(
            "ND", "SD", "NE", "KS", "OK", "TX"
          ) ~ "Plains",
          
          State %in% c(
            "MN", "IA", "MO", "WI",
            "IL", "MI", "IN", "OH"
          ) ~ "Midwest",
          
          State %in% c(
            "AR", "LA", "MS", "AL",
            "GA", "SC", "FL"
          ) ~ "South",
          
          State %in% c(
            "TN", "NC", "KY", "WV", "VA",
            "MD", "DE", "PA", "NJ", "NY",
            "CT", "MA", "NH", "VT", "ME"
          ) ~ "Atlantic",
          
          TRUE ~ "Other"
        )
      )
  }
  
# Use observations present in the fitted models.
  
  T5_model_data <- model.frame(
    T5_long_refit
  ) %>%
    add_region()
  
  T5C_model_data <- model.frame(
    T5C_long_refit
  ) %>%
    add_region()
  
  region_order <- c(
    "Atlantic",
    "Midwest",
    "Plains",
    "South",
    "West"
  )
  
# Fixed contribution = beta * (regional mean - national mean).
  
  calculate_fixed_contribution <- function(
    data,
    model,
    predictor_map,
    model_name
  ) {
    
    model_variables <- names(
      predictor_map
    )
    
    beta_tbl <- tibble(
      
      Model_Variable = model_variables,
      
      Beta = as.numeric(
        fixef(model)[model_variables]
      ),
      
      Variable = unname(
        predictor_map[model_variables]
      )
    )
    
    predictor_long <- data %>%
      
      filter(
        Region %in% region_order
      ) %>%
      
      select(
        Region,
        all_of(model_variables)
      ) %>%
      
      pivot_longer(
        cols = -Region,
        names_to = "Model_Variable",
        values_to = "Predictor_Value"
      )
    
    national_mean <- predictor_long %>%
      
      group_by(
        Model_Variable
      ) %>%
      
      summarise(
        
        National_Mean = mean(
          Predictor_Value,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      )
    
    regional_mean <- predictor_long %>%
      
      group_by(
        Region,
        Model_Variable
      ) %>%
      
      summarise(
        
        Regional_Mean = mean(
          Predictor_Value,
          na.rm = TRUE
        ),
        
        N = sum(
          !is.na(Predictor_Value)
        ),
        
        .groups = "drop"
      )
    
    regional_mean %>%
      
      left_join(
        national_mean,
        by = "Model_Variable"
      ) %>%
      
      left_join(
        beta_tbl,
        by = "Model_Variable"
      ) %>%
      
      mutate(
        
        Centered_Mean =
          Regional_Mean -
          National_Mean,
        
        Contribution =
          Beta *
          Centered_Mean,
        
        Model = model_name,
        
        Component = "Fixed effect"
      ) %>%
      
      select(
        Model,
        Region,
        Variable,
        Beta,
        Regional_Mean,
        National_Mean,
        Centered_Mean,
        Contribution,
        Component
      )
  }
  
# Regional RE contribution = mean fitted RE (no national-mean subtraction).
  
  calculate_random_contribution <- function(
    data,
    model,
    group_vars,
    model_name
  ) {
    
    temp <- data %>%
      
      filter(
        Region %in% region_order
      ) %>%
      
      select(
        Region,
        all_of(group_vars)
      )
    
    for (g in group_vars) {
      
      re_object <- ranef(model)[[g]]
      
      re_tbl <- tibble(
        
        Level = rownames(
          re_object
        ),
        
        RE_value = re_object[
          , "(Intercept)"
        ]
      )
      
      names(re_tbl) <- c(
        g,
        paste0(
          "RE_",
          g
        )
      )
      
      temp[[g]] <- as.character(
        temp[[g]]
      )
      
      re_tbl[[g]] <- as.character(
        re_tbl[[g]]
      )
      
      temp <- temp %>%
        
        left_join(
          re_tbl,
          by = g
        )
    }
    
    RE_columns <- paste0(
      "RE_",
      group_vars
    )
    
    temp <- temp %>%
      
      mutate(
        
        RE_total = rowSums(
          across(
            all_of(
              RE_columns
            )
          )
        )
      )
    
    national_RE <- mean(
      temp$RE_total,
      na.rm = TRUE
    )
    
    message(
      model_name,
      " national mean RE = ",
      round(
        national_RE,
        6
      )
    )
    
    temp %>%
      
      group_by(
        Region
      ) %>%
      
      summarise(
        
        Regional_RE = mean(
          RE_total,
          na.rm = TRUE
        ),
        
        N = n(),
        
        .groups = "drop"
      ) %>%
      
      mutate(
        
        Contribution = Regional_RE,
        
        Variable = "Random effects",
        
        Model = model_name,
        
        Component = "Random effect"
      ) %>%
      
      select(
        Model,
        Region,
        Variable,
        Regional_RE,
        Contribution,
        Component
      )
  }
  
# TPsurf contributions
  
  TPsurf_predictors <- c(
    
    Ca = "Ca",
    
    Fe = "Fe",
    
    Org_C = "OC",
    
    MnP = "MnP",
    
    FrP = "FrP"
  )
  
  TPsurf_fixed <- calculate_fixed_contribution(
    
    data = T5_model_data,
    
    model = T5_long_refit,
    
    predictor_map = TPsurf_predictors,
    
    model_name = "TPsurf"
  )
  
  TPsurf_random <- calculate_random_contribution(
    
    data = T5_model_data,
    
    model = T5_long_refit,
    
    group_vars = c(
      "State",
      "LRR"
    ),
    
    model_name = "TPsurf"
  )
  
# TPdiff contributions
  
  TPdiff_predictors <- c(
    
    Ca = "Ca",
    
    Org_C = "OC",
    
    pH = "pH",
    
    SpP = "SpP",
    
    FrP = "FrP"
  )
  
  TPdiff_fixed <- calculate_fixed_contribution(
    
    data = T5C_model_data,
    
    model = T5C_long_refit,
    
    predictor_map = TPdiff_predictors,
    
    model_name = "TPdiff"
  )
  
  TPdiff_random <- calculate_random_contribution(
    
    data = T5C_model_data,
    
    model = T5C_long_refit,
    
    group_vars = "State",
    
    model_name = "TPdiff"
  )
  
  Contribution_All <- bind_rows(
    
    TPsurf_fixed %>%
      select(
        Model,
        Region,
        Variable,
        Contribution,
        Component
      ),
    
    TPsurf_random %>%
      select(
        Model,
        Region,
        Variable,
        Contribution,
        Component
      ),
    
    TPdiff_fixed %>%
      select(
        Model,
        Region,
        Variable,
        Contribution,
        Component
      ),
    
    TPdiff_random %>%
      select(
        Model,
        Region,
        Variable,
        Contribution,
        Component
      )
  )
  
# Combine predictor and region contributions
  
  library(patchwork)
  
  region_order <- c(
    "Atlantic",
    "Midwest",
    "Plains",
    "South",
    "West"
  )
  
  var_order <- c(
    "Ca",
    "Fe",
    "OC",
    "pH",
    "SpP",
    "MnP",
    "FrP",
    "Random effects"
  )
  
  Contribution_All <- Contribution_All %>%
    mutate(
      
      Region = factor(
        Region,
        levels = region_order
      ),
      
      Variable = factor(
        Variable,
        levels = var_order
      ),
      
      Model = factor(
        Model,
        levels = c(
          "TPsurf",
          "TPdiff"
        )
      )
    )
  
  Contribution_All %>%
    arrange(
      Model,
      Region,
      Variable
    ) %>%
    print(
      n = Inf
    )
  
  predictor_colors <- c(
    
    Ca = "#FC9272",
    
    Fe = "#9E9AC8",
    
    OC = "#FDAE6B",
    
    pH = "#756BB1",
    
    SpP = "#74C476",
    
    MnP = "#6BAED6",
    
    FrP = "#31A354",
    
    `Random effects` = "#636363"
  )
  
# Keep the same y-axis across regions and both model panels.
  
  shared_y_range <- range(
    Contribution_All$Contribution,
    na.rm = TRUE
  )
  
  y_padding <- diff(
    shared_y_range
  ) * 0.08
  
  shared_y_limits <- c(
    shared_y_range[1] - y_padding,
    shared_y_range[2] + y_padding
  )
  
  shared_y_breaks <- pretty(
    shared_y_limits,
    n = 5
  )
  
# Plot regional contributions
  
  make_regional_plot <- function(
    plot_data,
    panel_title,
    show_x_title = TRUE
  ) {
    
    ggplot(
      plot_data,
      aes(
        x = Variable,
        y = Contribution,
        fill = Variable
      )
    ) +
      
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
      
    geom_col(
      width = 0.68
    ) +
      
    facet_wrap(
      ~ Region,
      nrow = 1,
      scales = "fixed"
    ) +
      
    scale_x_discrete(
      drop = TRUE,
      
      labels = c(
        Ca = "Ca",
        Fe = "Fe",
        OC = "OC",
        pH = "pH",
        SpP = "SpP",
        MnP = "MaP",
        FrP = "FrP",
        `Random effects` = "RE"
      )
    ) +
      
    scale_fill_manual(
      values = predictor_colors,
      guide = "none",
      drop = TRUE
    ) +
      
    scale_y_continuous(
      limits = shared_y_limits,
      breaks = shared_y_breaks,
      expand = expansion(
        mult = c(
          0,
          0.02
        )
      )
    ) +
      
    labs(
      
      title = panel_title,
      
      x = if (
        show_x_title
      ) {
        "Predictor / component"
      } else {
        NULL
      },
      
      y = expression(
        "Contribution to TP (" *
          log[10] ~ mg ~ kg^{-1} *
          ")"
      )
    ) +
      
    theme_classic(
      base_size = 12
    ) +
      
      theme(
        
        strip.background = element_blank(),
        
        strip.text = element_text(
          size = 13,
          face = "bold"
        ),
        
        axis.text.x = element_text(
          size = 12,
          face = "bold",
          angle = 45,
          hjust = 1,
          vjust = 1
        ),
        
        axis.text.y = element_text(
          size = 12
        ),
        
        axis.title.x = element_text(
          size = 13,
          face = "bold",
          margin = margin(
            t = 8
          )
        ),
        
        axis.title.y = element_text(
          size = 15,
          face = "bold",
          margin = margin(
            r = 10
          )
        ),
        
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0,
          margin = margin(
            b = 4
          )
        ),
        
        axis.line = element_line(
          linewidth = 0.5
        ),
        
        axis.ticks = element_line(
          linewidth = 0.4
        ),
        
        panel.spacing.x = grid::unit(
          0.8,
          "lines"
        ),
        
        legend.position = "none"
      )
  }

  p_TPsurf <- Contribution_All %>%
    filter(Model == "TPsurf") %>%
    make_regional_plot(
      panel_title = "(a) TPsurf",
      show_x_title = FALSE
    )

  p_TPdiff <- Contribution_All %>%
    
    filter(
      Model == "TPdiff"
    ) %>%
    
    make_regional_plot(
      panel_title = "(b) TPdiff",
      show_x_title = TRUE
    )
  
# Assemble and save Figure 4
  
  p <- p_TPsurf /
    p_TPdiff +
    plot_layout(
      heights = c(
        1,
        1
      )
    )
  
  print(p)
  
  ggsave(
    
    filename =
      "outputs/figures/Figure4_regional_contributions_faceted_by_FPR.png",
    
    plot = p,
    
    width = 11.5,
    
    height = 6.8,
    
    dpi = 300,
    
    bg = "white"
  )
