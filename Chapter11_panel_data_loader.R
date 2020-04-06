catholic_panel_data<-function(
  N=100
  , true_treatment_size=9
  , treatment_assignment="no_self_selection"
  , pretreatment_divergence_scale_k= 0.05)
{
  # Parameters
  # N - number of students
  # true_treatment_size = size of the true mean treatment effect
  # treatment_assignment = 
  #   "no_self_selection" - no self selection or selection on pretreatment
  #   "self_selection_effect_size" - selection on the size of the effect
  #   "self_selection_pretreatment" - selection on pretreatment outcome
  #   "pretreatment_divergence_scale_k" - 
  #     if under self_selection_pretreatment, 0.05 - positive bias, -0.05- negative bias
    
  ##########
  # 0) DATA
  
  # Define causal links size and effect
  o <- rnorm(N, mean = 0, sd = 1)
  
  # Define exogenous unobserved
  e <- rnorm(N, mean = 0, sd = 1)
  
  # Define perturbations
  e_x <- rnorm(N, mean = 0, sd = 1)
  e_u <- rnorm(N, mean = 0, sd = 1)
  
  # Define Common cause variables
  x <- o + e_x
  u <- o + e_u
  
  # Define perturbations for outcome
  nu_8_cf  <- rnorm(N, mean = 0, sd = sqrt(10)) 
  nu_9_cf  <- rnorm(N, mean = 0, sd = sqrt(10))
  nu_10_cf <- rnorm(N, mean = 0, sd = sqrt(10))
  
  # Define counterfactuals
  y_8_cf  <- 98  + o + u + x + e + nu_8_cf
  y_9_cf  <- 99  + o + u + x + e + nu_9_cf
  y_10_cf <- 100 + o + u + x + e + nu_10_cf
  
  # Define lifts
  delta_one_baseline   <- rnorm(N, mean = true_treatment_size, sd = 1)
  delta_two_individual <- rnorm(N, mean = 0, sd = 1)
  
  # Define treatment effect
  y_9_t    <- y_9_cf  + delta_one_baseline + delta_two_individual
  y_10_t   <- y_10_cf + (1 + delta_one_baseline) + delta_two_individual
  
  ##########
  # 1) TREATMENT ASSIGNMENT
  # treatment_assignment = 
  #   "no_self_selection" - no self selection or selection on pretreatment
  #   "self_selection_effect_size" - selection on the size of the effect
  #   "self_selection_pretreatment" - selection on pretreatment outcome
  #   "pretreatment_divergence_scale_k" - 
  #     if under self_selection_pretreatment, 0.05 - positive bias, -0.05- negative bias
  
  if (treatment_assignment == "no_self_selection"){
    p <- exp(-3.8 + o + u) / (1 + exp(-3.8 + o + u))
  } else if(treatment_assignment == "self_selection_effect_size")
  {
    p <- exp(-3.8 + o + u + 5*delta_two_individual) / (1 + exp(-3.8 + o + u + 5*delta_two_individual))
  } else if (treatment_assignment == "self_selection_pretreatment"){
    pretreatment_delta = y_8_cf-mean(y_8_cf)
    pretreatment_deflection=pretreatment_divergence_scale_k*pretreatment_delta
    p <- exp(-3.8 + o + u + pretreatment_deflection) / (1 + exp(-3.8 + o + u + pretreatment_deflection))
  } else{
    error("Error in selection mechanism - unrecongized seleciton mechanism")
    return(NA)
  }

  d<-rep(NA, N)
  for(i in 1:N){
    d[i] = rbinom(1, 1, prob = p[i])
  }
    
  # Observed
  y_8_obs  <- y_8_cf
  y_10_obs <- ifelse(d==1, y_10_t, y_10_cf) 
  
  #########
  # O) CONSOLIDATED DATA SET
  df <- data.frame(
    d
    , p
    , y_8_cf
    ,y_9_cf
    ,y_10_cf
    , y_9_t
    , y_10_t
    , y_8_obs
    , y_10_obs
    , o
    , x
  )
  
  # Sort by treatment
  df<-df[order(df$d, decreasing=TRUE), ]
  
  # Full data frame for debugging
  df_full <- df
  
  # Subset dataframe for modelling
  df <- subset(df_full, select=c(
    "d"
    , "y_8_obs"
    , "y_10_obs"
    , "o"
    , "x"))
  
  l = list(df=df, df_full=df_full)
  
  return(l)
}
