# Supplemental: state-effect correlations with and without Texas.
source("scripts/00_setup.R")

library(tidyverse)
library(patchwork)
library(ggrepel)

T5_states <- ranef(T5_long_refit)$State
T5C_states <- ranef(T5C_long_refit)$State

T5_df <- T5_states %>%
  rownames_to_column(var = "abbr") %>%
  rename(TPsurf = `(Intercept)`)

T5C_df <- T5C_states %>%
  rownames_to_column(var = "abbr") %>%
  rename(TPdiff = `(Intercept)`)

state_re <- inner_join(T5_df, T5C_df, by = "abbr")

state_re_noTX <- state_re %>%
  filter(abbr != "TX")

# Compute correlations and model fits

cor_with_TX <- cor(
  state_re$TPsurf,
  state_re$TPdiff,
  use = "complete.obs"
)

cor_without_TX <- cor(
  state_re_noTX$TPsurf,
  state_re_noTX$TPdiff,
  use = "complete.obs"
)

lm_with_TX <- lm(TPdiff ~ TPsurf, data = state_re)
lm_without_TX <- lm(TPdiff ~ TPsurf, data = state_re_noTX)

slope_with_TX <- coef(summary(lm_with_TX))["TPsurf", "Estimate"]
p_with_TX <- coef(summary(lm_with_TX))["TPsurf", "Pr(>|t|)"]

slope_without_TX <- coef(summary(lm_without_TX))["TPsurf", "Estimate"]
p_without_TX <- coef(summary(lm_without_TX))["TPsurf", "Pr(>|t|)"]

format_p <- function(p) {
  if (p < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", sprintf("%.3f", p))
  }
}

sig_star <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}

# With Texas

p_with_TX_plot <- ggplot(
  state_re,
  aes(x = TPsurf, y = TPdiff)
) +
  geom_point(size = 2.5) +
  
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  geom_text_repel(
    aes(label = abbr),
    size = 3.2,
    max.overlaps = Inf
  ) +
  
  labs(
    tag = "(a)",
    
    title = paste0(
      "With Texas: r = ",
      sprintf("%.2f", cor_with_TX),
      "; slope = ",
      sprintf("%.2f", slope_with_TX),
      "; ",
      format_p(p_with_TX),
      " ",
      sig_star(p_with_TX)
    ),
    
    x = expression(
      TP[surf] ~ state ~ random-effect ~ intercept~
        (log[10]~mg~kg^{-1})
    ),
    
    y = expression(
      TP[diff] ~ state ~ random-effect ~ intercept~
        (log[10]~mg~kg^{-1})
    )
  ) +
  
  theme_classic() +
  
  theme(
    plot.tag = element_text(
      size = 14,
      face = "bold"
    ),
    plot.title = element_text(
      size = 11
    )
  )

# Without Texas

p_without_TX_plot <- ggplot(
  state_re_noTX,
  aes(x = TPsurf, y = TPdiff)
) +
  geom_point(size = 2.5) +
  
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  geom_text_repel(
    aes(label = abbr),
    size = 3.2,
    max.overlaps = Inf
  ) +
  
  labs(
    tag = "(b)",
    
    title = paste0(
      "Without Texas: r = ",
      sprintf("%.2f", cor_without_TX),
      "; slope = ",
      sprintf("%.2f", slope_without_TX),
      "; ",
      format_p(p_without_TX),
      " ",
      sig_star(p_without_TX)
    ),
    
    x = expression(
      TP[surf] ~ state ~ random-effect ~ intercept~
        (log[10]~mg~kg^{-1})
    ),
    
    y = expression(
      TP[diff] ~ state ~ random-effect ~ intercept~
        (log[10]~mg~kg^{-1})
    )
  ) +
  
  theme_classic() +
  
  theme(
    plot.tag = element_text(
      size = 14,
      face = "bold"
    ),
    plot.title = element_text(
      size = 11
    )
  )

Figure_state_correlation <-
  p_with_TX_plot + p_without_TX_plot +
  plot_layout(ncol = 2)

Figure_state_correlation

cat(
  "With Texas:\n",
  "r =", round(cor_with_TX, 3),
  "\nslope =", round(slope_with_TX, 3),
  "\np-value =", p_with_TX,
  "\n\n"
)

cat(
  "Without Texas:\n",
  "r =", round(cor_without_TX, 3),
  "\nslope =", round(slope_without_TX, 3),
  "\np-value =", p_without_TX,
  "\n"
)
