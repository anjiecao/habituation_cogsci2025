

get_diff_raw_looking_time <- function(hab_data, dev_data){
  # calculate the diff between the last trial and the deviant
  hab_dev_diff <- hab_data %>% 
    group_by(subject) %>% 
    filter(trial_number == max(trial_number)) %>% 
    rename(last_hab_looking_time = trial_looking_time) %>% 
    select(subject, last_hab_looking_time) %>% 
    left_join(
      dev_data  %>% 
        rename(dev_looking_time = trial_looking_time) %>% 
        select(subject, dev_looking_time), 
      by = c("subject")
    ) %>% 
    mutate(
      hab_dev_diff = dev_looking_time - last_hab_looking_time, 
      log_hab_dev_diff = log(dev_looking_time)- log(last_hab_looking_time))
  
  return(hab_dev_diff)
}



calculate_resid_diff <- function(hab_resid, dev_resid){
  
  resid_diff <- hab_resid %>% 
  group_by(subject) %>% 
    filter(trial_number == max(trial_number)) %>% 
    rename(last_hab_resid = resid_hab) %>% 
    select(subject, last_hab_resid) %>% 
    left_join(
      dev_resid %>% select(subject, resid_dev), 
      by = c("subject")
    ) %>% 
    mutate(resid_diff = resid_dev - last_hab_resid, 
           resid_ratio = resid_dev / last_hab_resid)
  
  return(resid_diff)
}


