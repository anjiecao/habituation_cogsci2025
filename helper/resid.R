

get_hab_residual <- function(data,  random_structure =list(subject = pdDiag(a + b ~ 1))){
  
  mixed_exp_model <- nlme(
    trial_looking_time ~ a * exp(b * trial_number),  # Simplified exponential decay model
    data = data,
    fixed = a + b ~ 1,                # Fixed effects for a and b
    random = random_structure,         # Random effect for b (decay rate) by subject
    start = c(a = mean(data$trial_looking_time), 
              b = -0.05)  # Starting values
  )
  
  data$resid_hab <- resid(mixed_exp_model)
  
  return(data)
  
}

get_dev_residual <- function(data){
  
  deviant_model <- lme(
    trial_looking_time ~ 1,  # Deviant trial modeled as intercept-only
    random = ~ 1 | subject,  # Random intercept to account for subject variability
    data = data
  )
  
  data$resid_dev <- resid(deviant_model)
  
  return(data)
}
