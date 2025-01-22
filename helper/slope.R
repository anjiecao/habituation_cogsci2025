

# running the information 
get_individual_slope <- function(data, random_structure =  list(subject = pdDiag(a + b ~ 1))){
  
  mixed_exp_model <- nlme(
    trial_looking_time ~ a * exp(b * trial_number),  # Simplified exponential decay model
    data = data,
    fixed = a + b ~ 1,                # Fixed effects for a and b
    random = random_structure,         # Random effect for b (decay rate) by subject
    start = c(a = mean(data$trial_looking_time), 
              b = -0.05)  # Starting values
  )
  
  # Extract random effects (individual decay rates)
  random_effects <- ranef(mixed_exp_model)$b
  
  # Combine fixed effect with random effect for individual slopes
  fixed_decay_rate <- fixef(mixed_exp_model)["b"]
  individual_decay_rates <- fixed_decay_rate + random_effects
  
  # Create a data frame to store the slopes for each participant
  slopes <- data.frame(
    subject = rownames(ranef(mixed_exp_model)),
    decay_rate = individual_decay_rates
  )
  
  
  return(slopes)
}


get_individual_intercept <- function(data, random_structure =  list(subject = pdDiag(a + b ~ 1))){
  
  mixed_exp_model <- nlme(
    trial_looking_time ~ a * exp(b * trial_number),  # Simplified exponential decay model
    data = data,
    fixed = a + b ~ 1,                # Fixed effects for a and b
    random = random_structure,         # Random effect for b (decay rate) by subject
    start = c(a = mean(data$trial_looking_time), 
              b = -0.05)  # Starting values
  )
  
  # Extract random effects (individual decay rates)
  random_effects <- ranef(mixed_exp_model)$a
  
  # Combine fixed effect with random effect for individual slopes
  fixed_intercept <- fixef(mixed_exp_model)["a"]
  individual_intercept <- fixed_intercept + random_effects
  
  # Create a data frame to store the slopes for each participant
  intercepts <- data.frame(
    subject = rownames(ranef(mixed_exp_model)),
    intercept = individual_intercept
  )
  
  
  return(intercepts)
}

