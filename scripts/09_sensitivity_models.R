# Optional sensitivity analysis; run after the main model script.
source("scripts/00_setup.R")

  T5_long_refit_na <- lmer(TP ~ Ca + Fe + Org_C + (1 | LRR) + (1 | State),data = T5_long)
  T5C_long_refit_na <- lmer(TP ~ Ca + Org_C + pH + (1 | State),data = T5C_long)
 
  na_models <- list(
    T5_long_refit = T5_long_refit_na,
    T5C_long_refit = T5C_long_refit_na
  )
  
  r2_list <- lapply(na_models, r.squaredGLMM)
  
  for (name in names(r2_list)) {
    cat("\nR² for", name, ":\n")
    print(r2_list[[name]])
  }
  
# Fit models without agricultural P variables.
  
  model_formula_na <- TP ~ Al + Ca + Fe + Inorg_C + Org_C + pH +
     (1 | LRR) + (1 | State) + (1 | SO)
  
  T5_long_na = lmer(model_formula_na, data = T5_long)
  
  step(T5_long_na) 
  
  T5_narefit <- lmer(TP ~ Ca + Fe + Org_C + pH + (1 | State),data = T5_long)
  
  r.squaredGLMM(T5_narefit)
  
  model_formula_nad <- TP ~ Al + Ca + Fe + Inorg_C + Org_C + pH +
    (1 | LRR) + (1 | State) + (1 | SO)
  
  T5C_long_na = lmer(model_formula_na, data = T5C_long)

  step(T5C_long_na)  
  
  T5C_narefit <- lmer(TP ~ Al + Ca + Org_C + pH + (1 | State),data = T5C_long)
  
  r.squaredGLMM(T5C_narefit)
