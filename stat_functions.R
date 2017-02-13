ids <- readRDS("data/unique_ids.rds")

# percent max heart rate range required by job: 
# 100*(avg hr on job - resting HR) / (predicted HR max - resting HR)

perc_max <- function(id) {
  
  stats <- readRDS("data/id_stats.rds")
  stats <- filter(stats, id == id)
  avg <- stats$average_hr
  max <- stats$pred_max_hr
  
  resting <- readRDS("data/resting_work_5.rds")
  resting <- filter(resting, ID == id)
  resting <- resting$resting_at_work
  
  perc_max <- ((avg-resting)/(max-resting))*100
  return(perc_max)
  
}

percmax_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- perc_max(ids[i])
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
  
  return(out)
  
}

# resting heartrate at work 
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


resthr_loop <- function(ids, block) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- hr_work(ids[i], block = block)
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
  
  return(out)
  
}

#saveRDS(out, "data/resting_work_15.rds")
#rest <- readRDS("data/resting_work_15.rds")

#saveRDS(out, "data/resting_work_5.rds")

# basal metabolic rate
# men: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) + 5
# women: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) - 161

bmr <- function(ID) {
  
  stats <- readRDS("data/id_stats.rds")
  stats <- filter(stats, id == ID)
  weight <- stats$weight_kg
  age <- stats$age
  height <- stats$height_cm
  
  if (gender == "M") {
    
    bmr <- 10*weight + 6.25*height - 5*age + 5
    
  } else {
    
    bmr <- 10*weight + 6.25*height - 5*age - 161
    
  }
  
  return(bmr)
  
}

bmr_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- bmr(ids[i])
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
  
  return(out)
  
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



