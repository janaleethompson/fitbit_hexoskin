ids <- readRDS("data/unique_ids.rds")

# resting heartrate at work 
# block is the unit that you would like to calculate averages over
# for example, average = "1 sec", and block = 15 would take the minimum of moving 15 sec averages 

hr_work <- function(id, average, block) {
  
  df <- heartrate_df(id, fb_hr_breaks = avg, h_hr_breaks = avg, 
                     filter = TRUE)
  
  df <- filter(df, Device == "hexoskin" & !is.na(heartrate))
  
  # cutting off last 5 minutes? 
  # time <- select(df, date_time) %>% collect %>% .[["date_time"]]
  # end <- time[length(time)]
  # 
  # stop <- end - lubridate::minutes(5)
  # 
  # df <- filter(df, date_time <= stop)
  
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
#saveRDS(out_15, "data/resting_work.rds")

# percent max heart rate range required by job: 
# 100*(avg hr on job - resting HR) / (predicted HR max - resting HR)

perc_max <- function(id) {
  
  part <- id
  
  stats <- readRDS("data/id_stats.rds")
  stats <- filter(stats, id == part)
  avg <- stats$average_hr
  max <- stats$pred_max_hr
  
  resting <- readRDS("data/resting_work.rds")
  resting <- filter(resting, ID == id)
  resting <- resting$resting_at_work
  
  perc_max <- ((avg-resting)/(max-resting))*100
  perc_max <- data.frame(ID = id, percent_max = perc_max)
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

#out <- percmax_loop(ids)
#saveRDS(out, file = "data/percent_max.rds")
# missing stat info for 910n

# H1 step counts, heart rate for devices: two sample t-test 

stepcount_ttest <- function(id) {
  
  df <- stepcount_df(id)
  hex <- filter(df, Device == "hexoskin") %>% collect %>% .[["steps"]]
  fb <- filter(df, Device == "fitbit") %>% collect %>% .[["steps"]]
  
  variance <- var.test(hex, fb)$p.value
  
  if (variance > 0.05) {
    test <- t.test(hex, fb, var.equal = TRUE, paired = TRUE, alternative = "less")
  } else {
    test <- t.test(hex, fb, var.equal = FALSE, paired = TRUE, alternative = "less")
  }
  
  pval <- test$p.value
  
  out <- data.frame(ID = id, p_value = pval)
  
  return(out)
  
}

# Ha: true difference in means is less than 0 (hexoskin < fitbit)

stepcountttest_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- stepcount_ttest(ids[i])
      
      if (i == 1) {
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

# 200n 202n 301n 122w, 123w, 913n
# ids <- ids[!ids %in% c("200n", "202n", "301n", "122w", "123w", "913n")]

# out <- stepcountttest_loop(ids)
# out <- mutate(out, sig = if_else(p_value < 0.05, TRUE, FALSE))
# sig == TRUE indicates that hourly averages of fitbit stepcounts are greater than hexoskin 

# saveRDS(out, file = "data/stepcount_ttest.rds")



# H3 mean percent heart rate increase: two sample t-test
# maybe 



# H2 reliability between fitbit and hexoskin for steps: intra-class correlation 
# coefficients (ICC) 

library(ICC)

icc_hr <- function(id, alpha = 0.05) {
  
  df <- heartrate_df(id)
  df <- df %>% select(-date_time) %>%
    filter(!is.na(heartrate))
  
  coef <- ICC::ICCest(x = Device, y = heartrate, data = df, alpha = alpha)
  icc <- round(coef$ICC, 4)
  
  lci <- round(coef$LowerCI, 4)
  uci <- round(coef$UpperCI, 4)

  ci <- paste0("(", lci, ", ", uci, ")")
  
  out <- data.frame(ID = id, ICC = icc, CI = ci)
  return(out)
  
}

icchr_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- icc_hr(ids[i])
      
      if (i == 1) {
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

# H4 
# out <- icchr_loop(ids)
# saveRDS(out, "data/icc_heartrate.rds")


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


# exp_plots(ids, "rest_hr", 1)

# H5: active workers will take significantly more steps throughout their work-shift
# than sedentary workers 

stats <- readRDS("data/id_stats.rds")
active <- filter(stats, dept != "Office")$steps
sedentary <- filter(stats, dept == "Office")$steps
  
variance <- var.test(active, sedentary)$p.value
  
if (variance > 0.05) {
  test <- t.test(active, sedentary, var.equal = TRUE, paired = FALSE, alternative = "greater")
} else {
  test <- t.test(active, sedentary, var.equal = FALSE, paired = FALSE, alternative = "greater")
}

saveRDS(test, file = "data/active_sed_steps.rds")




# H6: active workers will have a greater mean percent heart rate increase 

stats <- readRDS("data/id_stats.rds")
active <- filter(stats, dept != "Office")$average_hr
sedentary <- filter(stats, dept == "Office")$average_hr

variance <- var.test(active, sedentary)$p.value

if (variance > 0.05) {
  test <- t.test(active, sedentary, var.equal = TRUE, paired = FALSE, alternative = "greater")
} else {
  test <- t.test(active, sedentary, var.equal = FALSE, paired = FALSE, alternative = "greater")
}

saveRDS(test, "data/meanhr_ttest.rds")


# H3 fitbit measures of heartrate > hexoskin 

heartrate_ttest <- function(id) {
  
  df <- heartrate_df(id)
  hex <- filter(df, Device == "hexoskin") %>% collect %>% .[["heartrate"]]
  fb <- filter(df, Device == "fitbit") %>% collect %>% .[["heartrate"]]
  
  variance <- var.test(hex, fb)$p.value
  
  if (variance > 0.05) {
    test <- t.test(hex, fb, var.equal = TRUE, paired = TRUE, alternative = "less")
  } else {
    test <- t.test(hex, fb, var.equal = FALSE, paired = TRUE, alternative = "less")
  }
  
  pval <- test$p.value
  
  out <- data.frame(ID = id, p_value = pval)
  
  return(out)
  
}

heartratettest_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- heartrate_ttest(ids[i])
      
      if (i == 1) {
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

# out <- heartratettest_loop(ids)
# problem w/ 122w, 123w, 913n
# out <- mutate(out, sig = if_else(p_value < 0.05, TRUE, FALSE))
# saveRDS(out, file = "data/heartrate_ttest.rds")

# H7 

stats <- readRDS("data/id_stats.rds")
percmax <- readRDS("data/percent_max.rds")
percmax <- rename(percmax, id = ID)
stats <- full_join(percmax, stats, by = "id")

active <- filter(stats, dept != "Office")$max
sedentary <- filter(stats, dept == "Office")$max

variance <- var.test(active, sedentary)$p.value

if (variance > 0.05) {
  test <- t.test(active, sedentary, var.equal = TRUE, paired = FALSE, alternative = "greater")
} else {
  test <- t.test(active, sedentary, var.equal = FALSE, paired = FALSE, alternative = "greater")
}

saveRDS(test, file = "data/active_sed_percmax.rds")


# H8 

mets_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- mets(ids[i], hourly = FALSE)
      
      if (i == 1) {
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

out <- mets_loop(ids)
out <- rename(out, id = ID)
stats <- readRDS("data/id_stats.rds")
df <- full_join(out, stats, by = "id")

active <- filter(df, dept != "Office")$mean_mets
sedentary <- filter(df, dept == "Office")$mean_mets

variance <- var.test(active, sedentary)$p.value

if (variance > 0.05) {
  test <- t.test(active, sedentary, var.equal = TRUE, paired = FALSE, alternative = "greater")
} else {
  test <- t.test(active, sedentary, var.equal = FALSE, paired = FALSE, alternative = "greater")
}

saveRDS(test, file = "data/active_sed_mets.rds")



# H2

icc_steps <- function(id, alpha = 0.05) {
  
  df <- stepcount_df(id)
  df <- df %>% select(-date_time) %>%
    filter(!is.na(steps))
  
  coef <- ICC::ICCest(x = Device, y = steps, data = df, alpha = alpha)
  icc <- round(coef$ICC, 4)
  
  lci <- round(coef$LowerCI, 4)
  uci <- round(coef$UpperCI, 4)
  
  ci <- paste0("(", lci, ", ", uci, ")")
  
  out <- data.frame(ID = id, ICC = icc, CI = ci)
  return(out)
  
}

iccsteps_loop <- function(ids) {
  
  for (i in 1:length(ids)) {
    
    possibleError <- tryCatch({
      
      df <- icc_steps(ids[i])
      
      if (i == 1) {
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

# out <- iccsteps_loop(ids)
# problem with 122w, 123w, 200n, 202n, 301n, 913n
# saveRDS(out, "data/icc_steps.rds")



# H1: Fitbit step counts > Hexoskin 
# one-sided t test, paired
h1 <- readRDS("data/stepcount_ttest.rds")
# significant = reject H0: no difference vs. Ha: fitbit > hexoskin

# H2: Fitbit and Hexoskin will have good inter method reliability for step counts 
# ICC
h2 <- readRDS("data/icc_steps.rds")

# H3: Hexoskin and Fitbit heart rate measures will be significantly different - Fitbit will overestimate heart rate
# one-sided t test, paried 
h3 <- readRDS("data/heartrate_ttest.rds")
# significant = reject H0: no difference vs. Ha: fitbit > hexoskin

# H4: Fitbit and Hexoskin will have poor inter-method reliability for heart rate measures 
# ICC
h3 <- readRDS("data/icc_heartrate.rds")

# H5: Active workers will take significantly more steps than sedentary workers
# one-sided t test, not paired 
h5 <- readRDS("data/active_sed_steps.rds")
# significant p-value: average active > sedentary steps 

# H6: Active workers will have a greater mean percent heart rate increase than sedentary workers 
# one-sided t test, not paired 
h6 <- readRDS("data/meanhr_ttest.rds")
# significant p-value: average active > sedentary heart rate 

# H7: Active workers will have a greater percent maximum heart rate range than sedentary workers 
# one-sided t test, not paired 
h7 <- readRDS("data/active_sed_percmax.rds")
# significant p-value: average active > sedentary % hr range 

# H8: Active workers will have increased energy expenditure than sedentary workers 
# one-sided t test, not paired 
h8 <- readRDS("data/active_sed_mets.rds")
# significant p-value: average active > sedentary METs 
