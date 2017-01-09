library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(stringr)

## Exploratory plots for fitbit and hexoskin data 

### Fitbit 

fitbit <- function(id, fb_hr_breaks = "1 min") {

  steps_file <- paste0(id, "_fb_steps")
  steps <- readRDS(paste0("data/", steps_file, ".rds"))
  
  fb_steps <- steps %>%
    mutate(date_time = as.POSIXct(ActivityMinute, 
                                  format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "UTC")) %>%
 #   mutate(date_time = force_tz(date_time, tzone = "MST")) %>%
    select(date_time, Steps) %>%
    rename(steps = Steps) %>%
    group_by(date_time = cut(date_time, breaks = "60 min")) %>%
    summarize(steps = sum(steps, na.rm= TRUE)) %>%
    ungroup(date_time) %>%
    mutate(steps = round(steps, 0), 
           date_time = lubridate::ymd_hms(date_time))
    
  hr_file <- paste0(id, "_fb_hr")
  fb_hr <- readRDS(paste0("data/", hr_file, ".rds"))
  
  fb_hr <- fb_hr %>%
    mutate(date_time = as.POSIXct(Time, 
                                  format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "UTC")) %>%
    select(date_time, Value) %>%
    rename(heart_rate = Value) %>%
    group_by(date_time = cut(date_time, breaks = fb_hr_breaks)) %>%
    summarize(heart_rate = mean(heart_rate, na.rm = TRUE)) %>%
    ungroup(date_time) %>%
    mutate(heart_rate = round(heart_rate, 0), 
           date_time = lubridate::ymd_hms(date_time))
    
  fb <- list("steps" = fb_steps, 
             "hr" = fb_hr)
  
  return(fb)
  
}

### Hexoskin

hexoskin <- function(id, br_breaks = "1 min") {
  
  hexo <- readRDS(paste0("data/", id, ".rds"))
  
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
  #  mutate(date_time = force_tz(date_time, tzone = "MST")) %>%
    mutate(date_time = force_tz(date_time, tzone = "UTC")) %>%
    mutate(sec = as.integer(lubridate::second(date_time)))

  hexo_breathing <- select(hexo, date_time, breathing_rate) %>% 
    group_by(date_time = cut(date_time, breaks = br_breaks)) %>%
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

heartrate_df <- function(id, br_breaks = "1 min", fb_hr_breaks = "1 min", 
                         h_hr_breaks = "1 min", filter = TRUE) {
  
  fb <- fitbit(id, fb_hr_breaks)
  fb_hr <- fb$hr
  
  hexo <- hexoskin(id, br_breaks)$hr
  
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


hr_plot <- function(id, br_breaks = "1 min", fb_hr_breaks = "1 min",
                    h_hr_breaks = "1 min", filter = TRUE, 
                    date_break = "30 min", fb = TRUE, h = TRUE) {
  
  to_plot <- heartrate_df(id, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
  
  if (fb == FALSE) {
    to_plot <- filter(to_plot, Device != "fitbit")
    
    plot <- ggplot(to_plot, aes(x = date_time, y = heartrate, color = Device)) + 
      geom_line(alpha = 0.75) + 
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Heart Rate") + 
      scale_color_manual(values = "#377EB8")
  }
  
  else if (h == FALSE) {
    to_plot <- filter(to_plot, Device != "hexoskin")
    
    plot <- ggplot(to_plot, aes(x = date_time, y = heartrate, color = Device)) + 
      geom_line(alpha = 0.75) + 
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Heart Rate") + 
      scale_color_manual(values = "#E41A1C")
  }
  
  else {
    
    plot <- ggplot(to_plot, aes(x = date_time, y = heartrate, color = Device)) + 
      geom_line(alpha = 0.75) + 
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Heart Rate") + 
      scale_colour_brewer(palette = "Set1") 
    
  }

  return(plot)
  
}

### Hexoskin breathing rate plots 

br_plot <- function(id, br_breaks = "1 min", date_break = "30 min") {
  
  df <- hexoskin(id, br_breaks)$breathing

  plot <- ggplot(df, aes(x = date_time, y = breathing_rate)) + 
    geom_line() + 
    theme_few() + 
    ylab("Breathing Rate") + 
    scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
    date_labels = "%I:%M %p")


  return(plot)

}


stepcount_df <- function(id, filter = TRUE) {

  fb_steps <- fitbit(id)$steps

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

stepcount_plot <- function(id, filter = TRUE) {

  to_plot <- stepcount_df(id, filter = filter)
  
  plot <- ggplot(to_plot, aes(x = date_time, y = steps, fill = Device)) + 
    geom_bar(stat = "identity", position = "dodge", alpha = 0.75) +
    scale_x_datetime(name = NULL, breaks = scales::date_breaks("60 min"), 
    date_labels = "%I:%M %p") +
    theme_few() + 
    ylab("Step Count per hour") + 
    xlab("") + 
    scale_fill_brewer(palette = "Set1") 

  return(plot)

}

# aggregating over multiple ids 

average_hr <- function(ids, br_breaks = "1 min", fb_hr_breaks = "1 min",
                       h_hr_breaks = "1 min", filter = TRUE, date_break = "30 min") {
  
  list <- vector("list", length(ids))
  
  for (i in 1:length(ids)) { 
    
    list[[i]] <- heartrate_df(ids[i], br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
  }
  
  for (i in 1:length(ids)) { 
    
    list[[i]] <- spread(list[[i]], Device, heartrate)
    
  }
  
  for (i in 1:length(list)) { 
    
    if (i == 1) { 
      df <- inner_join(list[[1]], list[[2]], by = "date_time")
    } else if (i != 2) {
      df <- inner_join(df, list[[i]], by = "date_time")
    }
    
  }
  
  fb <- subset(df, select = grep("fitbit|date_time", names(df)))
  
  hex <- subset(df, select = grep("hexoskin|date_time", names(df)))
  
  fb <- mutate(fb, fitbit = rowMeans(fb[,2:ncol(fb)], na.rm = TRUE)) %>%
    select(date_time, fitbit)
  
  hex <- mutate(hex, hexoskin = rowMeans(hex[,2:ncol(hex)], na.rm = TRUE)) %>%
    select(date_time, hexoskin)
  
  df <- full_join(fb, hex, by = "date_time") %>%
    gather("Device", "average_heartrate", 2:3)
  
  return(df)
  
}

hr_plot_avg <- function(ids, br_breaks = "1 min", fb_hr_breaks = "1 min",
                        h_hr_breaks = "1 min", filter = TRUE, date_break = "30 min", 
                        fb = TRUE, h = TRUE) {
  
  if (length(ids) == 1) { 
    
    plot <- hr_plot(ids, br_breaks, fb_hr_breaks, h_hr_breaks, filter, date_break, fb, h)
    
  } else {
      
    to_plot <- average_hr(ids, br_breaks, fb_hr_breaks, h_hr_breaks, filter, date_break)
    
    if (fb == FALSE) {
      to_plot <- filter(to_plot, Device != "fitbit")
      
      plot <- ggplot(to_plot, aes(x = date_time, y = average_heartrate, color = Device)) + 
        geom_line(alpha = 0.75) + 
        scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                         date_labels = "%I:%M %p") +
        theme_few() + 
        ylab("Average Heart Rate") + 
        scale_color_manual(values = "#377EB8")
    }
    
    else if (h == FALSE) {
      to_plot <- filter(to_plot, Device != "hexoskin")
      
      plot <- ggplot(to_plot, aes(x = date_time, y = average_heartrate, color = Device)) + 
        geom_line(alpha = 0.75) + 
        scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                         date_labels = "%I:%M %p") +
        theme_few() + 
        ylab("Average Heart Rate") + 
        scale_color_manual(values = "#E41A1C")
    }
    
    else {
      
      plot <- ggplot(to_plot, aes(x = date_time, y = average_heartrate, color = Device)) + 
        geom_line(alpha = 0.75) + 
        scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                         date_labels = "%I:%M %p") +
        theme_few() + 
        ylab("Average Heart Rate") + 
        scale_colour_brewer(palette = "Set1") 
      
    }
  }
  
  return(plot)
  
}


br_avg <- function(ids, br_breaks = "1 min", date_break = "30 min") { 
  
  list <- vector("list", length(ids))
  
  for (i in 1:length(ids)) { 
    
    list[[i]] <- hexoskin(ids[i], br_breaks)$breathing
    
  }
  
  for (i in 1:length(list)) {
    if(i == 1) {
      df <- inner_join(list[[1]], list[[2]], by = "date_time")
    } else if (i != 2) {
      df <- inner_join(df, list[[i]], by = "date_time")
    }
  }
  
  df <- mutate(df, average_breathing_rate = rowMeans(df[,2:ncol(df)], 
                                                     na.rm = TRUE)) %>%
    select(date_time, average_breathing_rate)
  
  return(df) 
  
}

br_plot_avg <- function(ids, br_breaks = "1 min", date_break = "30 min") {
  
  if (length(ids) == 1) { 
    
    plot <- br_plot(ids, br_breaks, date_break)
    
  } else {
      
    df <- br_avg(ids, br_breaks, date_break)
    plot <- ggplot(df, aes(x = date_time, y = average_breathing_rate)) + 
      geom_line() + 
      theme_few() + 
      ylab("Average Breathing Rate") + 
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p")
    
  }
  
  return(plot)
  
}

steps_avg <- function(ids, filter = TRUE) { 
  
  list <- vector("list", length(ids))
  
  for (i in 1:length(ids)) { 
    list[[i]] <- stepcount_df(ids[i], filter = filter)
  }
  
  for (i in 1:length(ids)) { 
    list[[i]] <- spread(list[[i]], Device, steps)
  }
  
  for (i in 1:length(list)) {
    if (i == 1) { 
      df <- inner_join(list[[1]], list[[2]], by = "date_time")
    } else if (i != 2) {
        df <- inner_join(df, list[[i]], by = "date_time")
    }
    
  }
  
  fb <- subset(df, select = grep("fitbit|date_time", names(df)))
  
  hex <- subset(df, select = grep("hexoskin|date_time", names(df)))
  
  fb <- mutate(fb, fitbit = rowMeans(fb[,2:ncol(fb)], na.rm = TRUE)) %>%
    select(date_time, fitbit)
  
  hex <- mutate(hex, hexoskin = rowMeans(hex[,2:ncol(hex)], na.rm = TRUE)) %>%
    select(date_time, hexoskin)
  
  df <- full_join(fb, hex, by = "date_time") %>%
    gather("Device", "average_stepcount", 2:3)
  
  return(df)
  
}

stepcount_plot_average <- function(ids, filter = TRUE) {
  
  if (length(ids) == 1) {
    plot <- stepcount_plot(ids, filter)
  } else {
    df <- steps_avg(ids, filter)
    plot <- ggplot(df, aes(x = date_time, y = average_stepcount, fill = Device)) + 
      geom_bar(stat = "identity", position = "dodge", alpha = 0.75) +
      scale_x_datetime(name = NULL, breaks = scales::date_breaks("60 min"), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Average Step Count per hour") + 
      xlab("") + 
      scale_fill_brewer(palette = "Set1")
  }
  
}


# want to have plots working 
# ask janalee about shifts 
# default plot when nothing is selected 
