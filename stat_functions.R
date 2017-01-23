# percent max heart rate range required by job: 
# 100*(avg hr on job - resting HR) / (predicted HR max - resting HR)

perc_max <- function(avg, rest, pred) {
  
  perc <- 100*((avg - rest) / (pred - rest))
  return(perc)

}

# average should be calculated from data 

# basal metabolic rate
# men: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) + 5
# women: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) - 161

basal_men <- function(weight, height, years) {
  
  rate <- 10*10*(weight*kg) + 6.25*(height*cm) - 5*(age*years) + 5
  return(rate)
  
}

basal_women <- funtion(weight, height, years) {
  
  rate <- 10*(weight*kg) + 6.25*(height*cm) - 5*(age*years) - 161
  return(rate)
  
}

# step counts, heart rate for devices: two sample t-test 

stepcount_ttest <- function(id) {
  
  df <- stepcount_df(id)
  hex <- filter(df, Device == "hexoskin") %>%
    collect %>% .[["steps"]]
  fb <- filter(df, Device == "fitbit") %>%
    collect %>% .[["steps"]]
  
  test <- t.test(hex, fb)
  
  return(test)
  
}


# mean percent heart rate increase: two sample t-test
# maybe 



# reliability between fitbit and hexoskin for steps: intra-class correlation 
# coefficients (ICC) 

library(ICC)

icc <- function(id, alpha = 0.05) {
  
  df <- heartrate_df(id)
  df <- df %>% select(-date_time)
  
  coef <- ICC::ICCest(x = Device, y = heartrate, data = df, alpha = alpha)
  
}




