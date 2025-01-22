
fit_nlme_full <- function(data, start_vals){
  fit_nlme <- nlme(
    trial_looking_time ~ a * exp(b * trial_number),
    data   = data,
    fixed  = a + b ~ 1,
    random = list(subject = pdDiag(a + b ~ 1)),  # random effect only on b
    start  = start_vals,
    control = nlmeControl(
      pnlsMaxIter = 50,
      msMaxIter   = 200,
      tolerance   = 1e-6,
      opt         = "nlminb"
    )
  )
} 


fit_nlme_b_only <- function(data, start_vals){
  fit_nlme <- nlme(
    trial_looking_time ~ a * exp(b * trial_number),
    data   = data,
    fixed  = a + b ~ 1,
    random = b ~ 1 | subject,  # random effect only on b
    start  = start_vals,
    control = nlmeControl(
      pnlsMaxIter = 50,
      msMaxIter   = 200,
      tolerance   = 1e-6,
      opt         = "nlminb"
    )
  )
} 

fit_nlme_a_only <- function(data, start_vals){
  fit_nlme <- nlme(
    trial_looking_time ~ a * exp(b * trial_number),
    data   = data,
    fixed  = a + b ~ 1,
    random = a ~ 1 | subject,  # random effect only on b
    start  = start_vals,
    control = nlmeControl(
      pnlsMaxIter = 50,
      msMaxIter   = 200,
      tolerance   = 1e-6,
      opt         = "nlminb"
    )
  )
} 