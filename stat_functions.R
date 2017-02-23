ids <- readRDS("data/unique_ids.rds")

# resting heartrate at work 
# block is the unit that you would like to calculate averages over

hr_work <- function(id, average, block) {
  
  df <- heartrate_df(id, fb_hr_breaks = avg, h_hr_breaks = avg, 
                     filter = TRUE)
  
  df <- filter(df, Device == "hexoskin" & !is.na(heartrate))
  
  time <- select(df, date_time) %>% collect %>% .[["date_time"]]
  end <- time[length(time)]
  
  stop <- end - lubridate::minutes(5)
  
  df <- filter(df, date_time <= stop)
  
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


resthr_loop <- function(ids, average, block) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- hr_work(ids[i], average = average, block = block)
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

#out_15 <- resthr_loop(ids, "1 sec", 30)

#saveRDS(out, "data/resting_work_15.rds")
#rest <- readRDS("data/resting_work_15.rds")

#saveRDS(out, "data/resting_work_5.rds")

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

# basal metabolic rate
# men: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) + 5
# women: 10*(weight(kg)) + 6.25*(height(cm)) - 5*(age(years)) - 161

# already calculated 

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

stepcountttest_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      p_val <- stepcount_ttest(ids[i])$p.value
      df <- data.frame(id = ids[i], p_value = p_val)
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

# 506n, 701n
#ids2 <- ids[! ids %in% c('506n', '701n', '122w', '123w', '200n', '202n', '913n')]

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


# looking into resting heartrate problem: getting results that are too low:

exp_plots <- function(ids, dir, averaging) {
  
  dir <- paste0("~/Desktop/", dir)
  
  if(!dir.exists(dir)) {
    dir.create(dir)
  }
  
  
  for (i in 1:length(ids)) { 
    
    file_name <- paste0(ids[i], ".png")
    grDevices::png(filename = paste0(dir, "/", file_name))
    
    avg <- paste0(averaging, " sec")
    
    to_plot <- heartrate_df(ids[i], fb_hr_breaks = avg, h_hr_breaks = avg, 
                            filter = TRUE)
    to_plot <- filter(to_plot, !is.na(heartrate)) %>%
      filter(Device == "hexoskin") 
    
    time <- select(to_plot, date_time) %>% collect %>% .[["date_time"]]
    end <- time[length(time)]
    
    stop <- end - lubridate::minutes(5)
    
    to_plot <- filter(to_plot, date_time <= stop)
    
    plot(to_plot$date_time, to_plot$heartrate, type = "l", col = "red", main = ids[i], 
         xlab = "time", ylab = "heartrate", ylim = c(0, 180))
    abline(h = 30, v = 1)
    abline(h = 60, v = 1)
    
    grDevices::dev.off()
    
  }
  
}


exp_plots(ids, "rest_hr", 1)



