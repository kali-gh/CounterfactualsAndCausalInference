library(plyr)
par(mfrow = c(2,2))
source("Chapter11_panel_data_loader.R")

set.seed(9)

# Define function to estimate the causal effects
estimate_causal_effects<-function(
  N=1000
  , true_treatment_size=9
  , treatment_assignment="no_self_selection"
  , pretreatment_divergence_scale_k= 0.05
  , name="d"
  , verbose=F)
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
  
  df <- catholic_panel_data(
    N=N
    , true_treatment_size=true_treatment_size
    , treatment_assignment=treatment_assignment
    , pretreatment_divergence_scale_k= pretreatment_divergence_scale_k)$df
  
  if(verbose) print(df)
  
  # Naive estimator
  means<-ddply(.data=df, .(d), summarize, mean=mean(y_10_obs))
  if(verbose) print("Means")
  if(verbose) print(means)
  if(verbose) print(paste("Naive treatment effect :", with(means, round(means[d==1, "mean"] - means[d==0, "mean"], 2))))
  
  # Naive regression estimator
  naive_fit<-lm(y_10_obs ~ d, df)
  
  # Change score estimator
  df$outcome <- df$y_10_obs - df$y_8_obs
  change_score_fit<- lm(outcome ~ d, df)
  
  # Ancova fit
  ancova_fit <- lm(y_10_obs ~  d + y_8_obs, df)
  
  # Ancova fit controlling for other confounding
  ancova_fit_full <- lm(y_10_obs ~ d + y_8_obs + o + x, df)
  
  if(verbose) print("Treatement effect : naive")
  if(verbose) print(summary(naive_fit)$coef)
  
  if(verbose) print("Treatement effect : change score")
  if(verbose) print(summary(change_score_fit)$coef)
  
  if(verbose) print("Treatement effect : ancova")
  if(verbose) print(summary(ancova_fit)$coef)
  
  if(verbose) print("Treatement effect : ancova_full")
  if(verbose) print(summary(ancova_fit_full)$coef)
  
  out <- data.frame(
    self_selection                 = 
      ifelse(
        treatment_assignment == "self_selection_effect_size", "Yes", "No")
    , self_selection_pos_pre_test  = ifelse(
        treatment_assignment == "self_selection_pretreatment" && pretreatment_divergence_scale_k > 0, "Yes", "No")
    , self_selection_neg_pre_test  = ifelse(
        treatment_assignment == "self_selection_pretreatment" && pretreatment_divergence_scale_k < 0, "Yes", "No")
    
    , naive_fit      = round(naive_fit$coefficients[2],2)
    , change_score   = round(change_score_fit$coefficients[2],2)
    , ancova         = round(ancova_fit$coefficients[2],2)
    , ancova_full    = round(ancova_fit_full$coefficients[2],2)
  )
  if(verbose) print(t(out))
  new_out =data.frame(t(out))
  new_out = rename(new_out, c("d"=name))
  return(new_out)
}

# Run estimates
no_self_selection_res               <- estimate_causal_effects(
  treatment_assignment="no_self_selection"
  , name="1"
)
self_selection_effect_size_res      <- estimate_causal_effects(
  treatment_assignment="self_selection_effect_size"
  , name="2"
)
self_selection_pretreatment_pos_res <- estimate_causal_effects(
  treatment_assignment="self_selection_pretreatment"
  , pretreatment_divergence_scale_k= 0.05
  , name="3"
)
self_selection_pretreatment_neg_res <- estimate_causal_effects(
  treatment_assignment="self_selection_pretreatment"
  , pretreatment_divergence_scale_k= -0.05
  , name="4"
)

xout=cbind(
  no_self_selection_res
  , self_selection_effect_size_res
  , self_selection_pretreatment_pos_res
  , self_selection_pretreatment_neg_res
)

print(xout)

