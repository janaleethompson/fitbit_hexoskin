# percent max heart rate range required by job: 
# 100*(avg hr on job - resting HR) / (predicted HR max - resting HR)

# want to calculate lowest on a per minute basis (?)

resting <- function(id) {
  
  df <- heartrate_df(id, fb_hr_breaks = "1 sec", h_hr_breaks = "1 sec", 
                     filter = FALSE)
  
#  h <- filter(df, Device == "hexoskin")
#  fb <- filter(df, Device == "fitbit")
  
  df_h <- filter(df, Device == "hexoskin" & !is.na(heartrate))
#  df_fb <- filter(df, Device == "fitbit" & !is.na(heartrate))
  
#  perc_missing_h <- (nrow(h) - nrow(df_h)) / nrow(h)
#  perc_missing_fb <- (nrow(fb) - nrow(df_fb)) / nrow(fb)
  
  for (i in 1:nrow(df_h)) {
    min_h <- df_h[i:60, 3]
    avg_h <- mean(min_h$heartrate)
    
    if (i == 1) {
      out_h <- avg_h
    } else {
      out_h <- c(out_h, avg_h)
    }
  }
  
  rest_h <- min(out_h)
  
  # for (i in 1:nrow(df_fb)) {
  #   min_fb <- df_fb[i:60, 3]
  #   avg_fb <- mean(min_fb$heartrate)
  #   
  #   if (i == 1) {
  #     out_fb <- avg_fb
  #   } else {
  #     out_fb <- c(out_fb, avg_fb)
  #   }
  # }
  
#  rest_fb <- min(out_fb)
  
  out_df <- data.frame(ID = id, resting_hr = rest_h)
  
  return(out_df)
  
}

#setwd('data')
#ids <- unique(substr(list.files(), 1, 4))
#setwd('..')

for (i in 1:length(ids)) {
  
  possibleError <- tryCatch({
    
    df <- resting(ids[i])
    if(i == 1){
      out <- df
    } else {
      out <- rbind(out, df)
    }
    
  }, error = function(e) {
    e
    message(paste0("problem with ID ", ids[i]))
  })
  
  if(inherits(possibleError, "error")) next
  
}

#saveRDS(out, "data/resting_hr.rds")
rest <- readRDS("data/resting_hr.rds")

# probably better to rely on hexoskin ? 
# missing data for 202n (missing fitbit hr data) and 210n (missing hexoskin data) 

perc_max <- function(avg, rest, pred) {
  
  perc <- 100*((avg - rest) / (pred - rest))
  return(perc)

}

# average should be calculated from data 

# basal metabolic rate
# men: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) + 5
# women: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) - 161

basal_men <- function(weight, height, years) {
  
  rate <- 10*weight*kg + 6.25*height*cm - 5*age*years + 5
  return(rate)
  
}

basal_women <- funtion(weight, height, years) {
  
  rate <- 10*weight*kg + 6.25*height*cm - 5*age*years - 161
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


# resting heart rate estimation 
# make note of problem IDs

