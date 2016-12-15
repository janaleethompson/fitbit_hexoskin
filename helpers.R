library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(stringr)

## Exploratory plots for fitbit and hexoskin data 

### Fitbit 

fitbit <- function(id, fb_hr_breaks = "1 min", 
                   download_range = "20161105_20161205") {

  steps_file <- paste0(id, "_minuteStepsNarrow_", download_range)
  fb_steps <- readRDS(paste0("data/", steps_file, ".rds"))
  
  fb_steps <- fb_steps %>%
    mutate(date_time = as.POSIXct(ActivityMinute, 
                                  format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "UTC")) %>%
    select(date_time, Steps) %>%
    rename(steps = Steps) %>%
    group_by(date_time = cut(date_time, breaks = "60 min")) %>%
    summarize(steps = sum(steps, na.rm= TRUE)) %>%
    ungroup(date_time) %>%
    mutate(steps = round(steps, 0), 
           date_time = lubridate::ymd_hms(date_time))
    
  hr_file <- paste0(id, "_heartrate_seconds_", download_range)
  fb_hr <- readRDS(paste0("data/", hr_file, ".rds"))
  
  fb_hr <- fb_hr %>%
    mutate(date_time = as.POSIXct(Time, 
                                  format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "UTC")) %>%
    select(date_time, Value) %>%
    rename(heart_rate = Value) %>%
    group_by(date_time = cut(date_time, breaks = fb_breaks)) %>%
    summarize(heart_rate = mean(heart_rate, na.rm = TRUE)) %>%
    ungroup(date_time) %>%
    mutate(heart_rate = round(heart_rate, 0), 
           date_time = lubridate::ymd_hms(date_time))
    
  fb <- list("steps" = fb_steps, 
             "hr" = fb_hr)
  
  return(fb)
  
}

### Hexoskin

hexoskin <- function(id) {
  
  df <- data.frame("hexo" = c("record-111413", "record-111955", 
                            "record-111555", "record-111902", 
                            "record-112875"), 
                 "fb" = c("300n", "308n", "501n", "601n", "203q"))
  
  loc <- which(df$fb == id)
  file <- as.character(df[loc,1])

  hexo <- readRDS(paste0("data/", file, ".rds"))
  
  hexo_df <- hexo %>%
    rename(time = time..s.256., 
           breathing_rate = breathing_rate..rpm...api.datatype.33..,
           minute_vent = minute_ventilation..mL.min...api.datatype.36..,
           sleep = sleep_position..NA...api.datatype.270..,
           activity = activity..g...api.datatype.49..,
           heart_rate = heart_rate..bpm...api.datatype.19..,
           cadence = cadence..spm...api.datatype.53..) %>%
    mutate(date_time = (ymd("19700101") + dseconds(time/256 - 21600)) - 3600) %>%
    select(-time, -minute_vent, -sleep, -activity)
  
  hexo <- hexo_df[,c(4, 2, 1, 3)]
  
  hexo_hr <- select(hexo, date_time, heart_rate) %>%
    mutate(date_time = force_tz(date_time, tzone = "UTC")) %>%
    mutate(sec = as.integer(lubridate::second(date_time)))

  hexo_breathing <- select(hexo, date_time, breathing_rate) %>% 
    group_by(date_time = cut(date_time, breaks = "1 min")) %>%
    summarize(breathing_rate = mean(breathing_rate, na.rm = TRUE)) %>%
    ungroup(date_time) %>%
    mutate(breathing_rate = round(breathing_rate, 0), 
           date_time = lubridate::ymd_hms(date_time)) %>%
    unique() 
  
  hexo_steps <- select(hexo, date_time, cadence) %>% 
    rename(steps = cadence) %>%
    select(date_time, steps) %>% 
    mutate(steps = steps/60) %>%
    group_by(date_time = cut(date_time, breaks = "60 min")) %>%
    summarize(steps = sum(steps, na.rm = TRUE)) %>%
    ungroup(date_time) %>%
    
    mutate(date_time = lubridate::ymd_hms(date_time), 
           hour = lubridate::hour(date_time))
          
  list <- stringr::str_split(as.character(hexo_steps$date_time), " ")
  df <- lapply(list, tibble::as_tibble)
  
  for(i in 1:length(df)) {
    df[[i]] <- df[[i]][1,]
  }
  
  df <- bind_rows(df)
  
  hexo_steps <- bind_cols(hexo_steps, df) 
  
  hexo_steps <- hexo_steps %>%
    mutate(temp = paste(hexo_steps$value, paste0(hexo_steps$hour, ":00:00")), 
           temp = lubridate::ymd_hms(temp)) %>%
    select(temp, steps) %>%
    rename(date_time = temp)
    
  
  hexo <- list("hr" = hexo_hr, 
               "breathing" = hexo_breathing, 
               "steps" = hexo_steps)
  
  return(hexo)
  
}

heartrate_df <- function(id, fb_hr_breaks = "1 min", 
                         h_hr_breaks = "1 min", filter = TRUE,
                         download_range = "20161105_20161205") {
  
  fb <- fitbit(id, fb_hr_breaks, download_range)
  fb_hr <- fb$hr
  
  hexo <- hexoskin(id)$hr
  
  if (filter == TRUE) {
    
    if (min(hexo$date_time) < min(fb_hr$date_time)) { 
      hexo <- filter(hexo, date_time >= min(fb_hr$date_time))
    } else {
      fb_hr <- filter(fb_hr, date_time >= min(hexo$date_time))
    }
    
    if (max(hexo$date_time) < max(fb_hr$date_time)) {
      fb_hr <- filter(fb_hr, date_time <= max(hexo$date_time))
    } else {
      hexo <- filter(hexo, date_time <= max(fb_hr$date_time))
    }
    
    hexo <- hexo %>%
      filter(date_time >= min(fb_hr$date_time) & date_time <= max(fb_hr$date_time))
    
  }
  
  hexo <- hexo %>%
    group_by(date_time = cut(date_time, breaks = h_hr_breaks)) %>%
    summarize(heart_rate = mean(heart_rate, na.rm = TRUE)) %>%
    ungroup(date_time) %>%
    mutate(heart_rate = round(heart_rate, 0), 
           date_time = lubridate::ymd_hms(date_time))
  
  hexo <- unique(hexo)
  fb_hr <- unique(fb_hr)
  
  hr <- full_join(hexo, fb_hr, by = "date_time") %>%
    rename(hexoskin = heart_rate.x,
           fitbit = heart_rate.y) %>%
    gather("Device", "heartrate", 2:3)
  
  return(hr)

}


hr_plot <- function(id, fb_hr_breaks = "1 min", h_hr_breaks = "1 min", 
                    filter = TRUE, download_range = "20161105_20161205", 
                    date_break = "30 min") {
  
  to_plot <- heartrate_df(id, fb_hr_breaks, h_hr_breaks, filter, download_range)
  
  plot <- ggplot(to_plot, aes(x = date_time, y = heartrate, color = Device)) + 
    geom_line(alpha = 0.75) + 
    scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                     date_labels = "%I:%M %p") +
    theme_few() + 
    ylab("Heart Rate") + 
    scale_colour_brewer(palette = "Set1") 
  
  return(plot)
  
}

### Hexoskin breathing rate plots 

br_plot <- function(id, date_break = "30 min") {
  

  df <- hexoskin(id)$breathing


  plot <- ggplot(df, aes(x = date_time, y = breathing_rate)) + 
    geom_line() + 
    theme_few() + 
    ylab("Breathing Rate (1 min. intervals)") + 
    scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
    date_labels = "%I:%M %p")


  return(plot)

}


stepcount_df <- function(id, filter = TRUE,
                         download_range = "20161105_20161205") {

  fb <- fitbit(id, download_range)
  fb_steps <- fb$steps
  
  fb_steps <- filter(fb_steps, steps != 0)

  hexo <- hexoskin(id)$steps

  if (filter == TRUE) {

  if (min(hexo$date_time) < min(fb_steps$date_time)) { 
    hexo <- filter(hexo, date_time >= min(fb_steps$date_time))
  } else {
    fb_steps <- filter(fb_steps, date_time >= min(hexo$date_time))
  }

  if (max(hexo$date_time) < max(fb_steps$date_time)) {
    fb_steps <- filter(fb_steps, date_time <= max(hexo$date_time))
  } else {
    hexo <- filter(hexo, date_time <= max(fb_steps$date_time))
  }

  hexo <- hexo %>%
  filter(date_time >= min(fb_steps$date_time) & date_time <= max(fb_steps$date_time))

  }

  steps <- full_join(hexo, fb_steps, by = "date_time") %>%
    rename(hexoskin = steps.x,
    fitbit = steps.y) %>%
    gather("Device", "steps", 2:3)
  
  return(steps)

}

stepcount_plot <- function(id, filter = TRUE, 
                           download_range = "20161105_20161205") {

  to_plot <- stepcount_df(id, filter, download_range)
  
  plot <- ggplot(to_plot, aes(x = date_time, y = steps, fill = Device)) + 
    geom_bar(stat = "identity", position = "dodge", alpha = 0.75) +
    scale_x_datetime(name = NULL, breaks = scales::date_breaks("1 hour"), 
    date_labels = "%I:%M %p") +
    theme_few() + 
    ylab("Step Count per hour") + 
    scale_fill_brewer(palette = "Set1") 

  return(plot)

}





# aggregated data - want to be able to aggreagate across several workers. 



