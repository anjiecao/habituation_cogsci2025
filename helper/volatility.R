

get_residual_sd <- function(resid_data){
  
  resid_summary <- resid_data %>% 
    group_by(subject) %>% 
    summarise(sd_resid = sd(resid_hab, na.rm = TRUE))
  
  return(resid_summary)
  
}





get_volatility_measures <- function(data){
  
  # Initialize an empty list to store GARCH volatility results
  garch_volatility_list <- list()
  
  # Group by participant and calculate each volatility measure
  volatility_summary <- data %>%
    group_by(subject) %>%
    summarize(
      # 1. Standard Deviation
      sd_volatility = sd(trial_looking_time, na.rm = TRUE),
      
      # 2. Mean Absolute Deviation (MAD)
      mad_volatility = mean(abs(trial_looking_time - mean(trial_looking_time, na.rm = TRUE)), na.rm = TRUE),
      
      # 3. Coefficient of Variation (CV)
      cv_volatility = sd(trial_looking_time, na.rm = TRUE) / mean(trial_looking_time, na.rm = TRUE),
      
      # 4. Range-Based Volatility
      range_volatility = max(trial_looking_time, na.rm = TRUE) - min(trial_looking_time, na.rm = TRUE),
      
      # 5. Median Absolute Deviation (MAD - Robust Version)
      mad_robust_volatility = median(abs(trial_looking_time - median(trial_looking_time, na.rm = TRUE)), na.rm = TRUE)
    )
  
  
  # Calculate GARCH-based volatility for each participant and add to the volatility_summary
  for (subject_id in unique(data$subject)) {
    # Subset data for each participant
    participant_data <- filter(data, subject == subject_id)$trial_looking_time
    
    # Specify and fit GARCH model if there are enough data points
    if (length(participant_data) > 5) {  # Minimum data points to fit GARCH - doesn't work 
      spec <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(1, 1)))
      fit <- tryCatch(ugarchfit(spec = spec, data = participant_data), error = function(e) NULL)
      
      if (!is.null(fit)) {
        # Extract the average conditional volatility from the GARCH model
        avg_garch_volatility <- mean(sigma(fit), na.rm = TRUE)
      } else {
        avg_garch_volatility <- NA
      }
    } else {
      avg_garch_volatility <- NA
    }
    
    # Add the GARCH volatility to the list for each subject
    garch_volatility_list[[as.character(subject_id)]] <- avg_garch_volatility
  }
  
  # Convert GARCH volatility list to data frame and merge with other volatility measures
  garch_volatility_df <- data.frame(
    subject = names(garch_volatility_list),
    garch_volatility = unlist(garch_volatility_list)
  )
  
  # Combine the GARCH results with the other volatility measures
  volatility_summary <- left_join(volatility_summary, garch_volatility_df, by = "subject")
  
  return(volatility_summary)
}