# percent max heart rate range required by job: 
# 100*(avg hr on job - resting HR) / (predicted HR max - resting HR)

# want to calculate lowest on a per minute basis (?)

# block is the unit of seconds that you would like to calculate averages for

hr_work <- function(id, block = 15) {
  
  df <- heartrate_df(id, fb_hr_breaks = "1 sec", h_hr_breaks = "1 sec", 
                     filter = FALSE)
  
  df <- filter(df, Device == "hexoskin" & !is.na(heartrate))
  
  loop_n <- (block - 1)
  loop_index <- nrow(df) - loop_n
  
  for (i in 1:loop_index) {
    
    sec <- df[i:(i + loop_n), 3]
    avg <- mean(sec$heartrate)
    
    if (i == 1) {
      out <- avg
    } else {
      out <- c(out, avg)
    }
    
  }
  
  work_rest <- min(out)
  
  out_df <- data.frame(ID = id, resting_at_work = work_rest)
  
  return(out_df)
  
}

#setwd('data')
#ids <- unique(substr(list.files(), 1, 4))
#setwd('..')

for (i in 1:length(ids)) {
  
  possibleError <- tryCatch({
    
    df <- hr_work(ids[i])
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

#saveRDS(out, "data/resting_work_15.rds")
rest <- readRDS("data/resting_work_15.rds")


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



