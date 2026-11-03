# Fit TPsurf and TPdiff mixed models (long, medium, short inventory periods).
source("scripts/00_setup.R")
library(lme4)
library(lmerTest)
library(dplyr)
library(MuMIn)

   Input_Data <- readRDS("data/processed/Input_Data.rds")

   Input_Data$TP_diff <- Input_Data$Top5_P - Input_Data$C_P
   
   count <- sum(Input_Data$TP_diff <= -500, na.rm = TRUE)
   
# Exclude TPdiff <= -500 mg/kg sites from both responses (common sample).
   exclude_extreme <- !is.na(Input_Data$TP_diff) & Input_Data$TP_diff <= -500
   Input_Data <- Input_Data[!exclude_extreme, , drop = FALSE]
   cat("Sites excluded from BOTH TPsurf and TPdiff (TP_diff <= -500 mg/kg):", count, "\n")
   
# Transform predictors and responses on the scale used in the manuscript.

    r21 <- cor(Input_Data$Psurplus_l, Input_Data$Psurplus_m)^2
    r22 <- cor(Input_Data$Fertilizer_l, Input_Data$Fertilizer_m)^2
    r23 <- cor(Input_Data$Manure_l, Input_Data$Manure_m)^2
   
    Input_Data <- Input_Data %>%
      mutate(across(
        c(Psurplus_l, Fertilizer_l, Manure_l, Psurplus_m, Fertilizer_m, Manure_m, Psurplus_s, Fertilizer_s, Manure_s),
        ~ . * 1.12
      )) %>%
      mutate(across(c(Top5_Al, Top5_Ca, Top5_Fe, A_C_Tot, A_C_Inorg, A_C_Org, pH), ~ log10(. + 0.1))) %>%
      mutate(across(c(Psurplus_l, Psurplus_m, Psurplus_s), ~ log10(. + 1000))) %>%
      mutate(across(c(Fertilizer_l, Manure_l, Fertilizer_m, Manure_m, Fertilizer_s, Manure_s), ~ log10(.))) %>%
      mutate(Top5_P = log10(Top5_P),
             TP_diff = log10(TP_diff + 500)) 
    
    Input_Data_v1 <- Input_Data
    
    common_cols <- c("Top5_Al", "Top5_Ca", "Top5_Fe", "A_C_Tot", "A_C_Inorg", "A_C_Org", "pH", "LRR", "StateID", "SO")
    
    Psurplus_cols <- list(
      long = c("Psurplus_l", "Fertilizer_l", "Manure_l"),
      medium = c("Psurplus_m", "Fertilizer_m", "Manure_m"),
      short = c("Psurplus_s", "Fertilizer_s", "Manure_s")
    )
    
    T5_long <- Input_Data[, c("Top5_P", common_cols, Psurplus_cols$long)]
    T5_medium <- Input_Data[, c("Top5_P", common_cols, Psurplus_cols$medium)]
    T5_short <- Input_Data[, c("Top5_P", common_cols, Psurplus_cols$short)]
    
    T5C_long <- Input_Data[, c("TP_diff", common_cols, Psurplus_cols$long)]
    T5C_medium <- Input_Data[, c("TP_diff", common_cols, Psurplus_cols$medium)]
    T5C_short <- Input_Data[, c("TP_diff", common_cols, Psurplus_cols$short)]
    stopifnot(nrow(T5_long) == nrow(T5C_long))
    cat("Data rows after common exclusions (both responses):", nrow(T5_long), "\n")

    new_names <- c("TP", "Al", "Ca", "Fe", "Tot_C", "Inorg_C", 
               "Org_C", "pH","LRR", "State", "SO", "SpP", "FrP", "MnP")

    variable_list <- list(
      T5_long = T5_long,
      T5_medium = T5_medium,
      T5_short = T5_short,
      T5C_long = T5C_long,
      T5C_medium = T5C_medium,
      T5C_short = T5C_short
    )

  for (var in names(variable_list)) {
    names(variable_list[[var]]) <- new_names
  }
  
  list2env(variable_list, envir = .GlobalEnv)

# Omit total C from candidate terms because it overlaps with IC + OC.
model_formula <- TP ~ Al + Ca + Fe + Inorg_C + Org_C + pH +
  SpP + FrP + MnP + (1 | LRR) + (1 | State) + (1 | SO)

  T5_long_model = lmer(model_formula, data = T5_long)
  T5_medium_model = lmer(model_formula, data = T5_medium)
  T5_short_model = lmer(model_formula, data = T5_short)
  T5C_long_model = lmer(model_formula, data = T5C_long)
  T5C_medium_model = lmer(model_formula, data = T5C_medium)
  T5C_short_model = lmer(model_formula, data = T5C_short)

  step(T5_long_model)   
  step(T5_medium_model) 
  step(T5_short_model)  
  step(T5C_long_model) 
  step(T5C_medium_model) 
  step(T5C_short_model)  
  
  T5_long_refit <- lmer(TP ~ Ca + Fe + Org_C + FrP + MnP + (1 | LRR) + (1 | State),data = T5_long)
  T5_medium_refit <- lmer(TP ~ Ca + Fe + Org_C + FrP + MnP + (1 | LRR) + (1 | State),data = T5_medium)
  T5_short_refit <- lmer(TP ~ Ca + Fe + Org_C + pH + SpP + (1 | State),data = T5_short)
  
  T5C_long_refit <- lmer(TP ~ Ca + Org_C + pH + SpP + FrP + (1 | State),data = T5C_long)
  T5C_medium_refit <- lmer(TP ~ Ca + Org_C + pH + SpP + FrP + (1 | State),data = T5C_medium)
  T5C_short_refit <- lmer(TP ~ Al + Ca + Org_C + pH + MnP + (1 | State),data = T5C_short)

  T5_ranef <- ranef(T5_long_refit)
  T5C_ranef <- ranef(T5C_long_refit)
  
  T5_state <- T5_ranef[["State"]]
  T5_LRR <- T5_ranef[["LRR"]]
  
  T5C_state <- T5C_ranef[["State"]]
  
  write.csv(T5_state,"outputs/tables/T5_state.csv")
  write.csv(T5_LRR,"outputs/tables/T5_LRR.csv")
  
  write.csv(T5C_state,"outputs/tables/T5C_state.csv")
  
  T5_long_refit
  
  T5C_long_refit

  final_models <- list(
    T5_long_refit = T5_long_refit,
    T5_medium_refit = T5_medium_refit,
    T5_short_refit = T5_short_refit,
    T5C_long_refit = T5C_long_refit,
    T5C_medium_refit = T5C_medium_refit,
    T5C_short_refit = T5C_short_refit
  )
  
  r2_list <- lapply(final_models, r.squaredGLMM)
  
  for (name in names(r2_list)) {
    cat("\nR² for", name, ":\n")
    print(r2_list[[name]])
  }
