


compare_models <- function(model_list, model_names){
  
  AICs <- sapply(model_list, AIC)
  BICs <- sapply(model_list, BIC)
  
  model_comparison <- data.frame(
    Model = model_names,
    AIC = AICs,
    BIC = BICs
  )
  
  return(model_comparison)
  
}
