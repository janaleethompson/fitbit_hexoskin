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
    mutate(date_time = force_tz(date_time, tzone = "UTC")) # %>%
  #  mutate(sec = as.integer(lubridate::second(date_time)))

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


hr_plot_mult <- function(ids, br_breaks = "1 min", fb_hr_breaks = "1 min",
                         h_hr_breaks = "1 min", filter = TRUE, 
                         date_break = "30 min", fb = TRUE, h = TRUE, 
                         min = NULL, max = NULL, alpha = 1) {
  
  if (length(ids == 1)) {
    id <- ids[1]
    id2 <- NULL
    id3 <- NULL
    id4 <- NULL
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 2)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- NULL
    id4 <- NULL
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 3)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- NULL
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 4)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 5)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 6)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 7)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 8)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- ids[8]
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 9)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- ids[8]
    id9 <- ids[9]
    id10 <- NULL
  }
  
  if (length(ids == 10)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- ids[8]
    id9 <- ids[9]
    id10 <- ids[10]
  }

  to_plot <- heartrate_df(id, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
  
  if (!is.null(min) & !is.null(max)) {
    
    late <- c('307n', '308n', '505n', '602n', '600n', '601n')
    tail <- dim(to_plot)[1]
    
    date_min <- format(to_plot$date_time[1], "%Y-%m-%d")
    date_max <- format(to_plot$date_time[1], "%Y-%m-%d")
    
    if(any(ids %in% late)) {
      if(grep("am", min) == 1) {
        date_min <- format(df$date_time[tail], "%Y-%m-%d")
      }
      if(grep("am", max) == 1) {
        date_max <- format(df$date_time[tail], "%Y-%m-%d")
      }
    }
    
    time_min <- substr(min, 1, nchar(min)-3)
    ampm_min <- substr(min, nchar(min)-1, nchar(min))
    
    time_max <- substr(max, 1, nchar(max)-3)
    ampm_max <- substr(max, nchar(max)-1, nchar(max))
    
    min_time <- lubridate::ymd_hms(paste0(date_min, " ", time_min, ":00", " ", 
                                          ampm_min))
    max_time <- lubridate::ymd_hms(paste0(date_max, " ", time_max, ":00", " ", 
                                          ampm_max))
    
    to_plot <- filter(to_plot, date_time >= min_time & date_time <= max_time)
    
  }
  
  to_plot_fb <- filter(to_plot, Device != "hexoskin")
  to_plot_hex <- filter(to_plot, Device != "fitbit")
  
  if (!is.na(id2)) {
    to_plot2 <- heartrate_df(id2, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot2 <- filter(to_plot2, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb2 <- filter(to_plot2, Device != "hexoskin")
    to_plot_hex2 <- filter(to_plot2, Device != "fitbit")
  }
  
  if (!is.na(id3)) {
    to_plot3 <- heartrate_df(id3, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot3 <- filter(to_plot3, date_time >= min_time & date_time <= max_time)
    }
  
    to_plot_fb3 <- filter(to_plot3, Device != "hexoskin")
    to_plot_hex3 <- filter(to_plot3, Device != "fitbit")
  }
  
  if (!is.na(id4)) {
    to_plot4 <- heartrate_df(id4, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot4 <- filter(to_plot4, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb4 <- filter(to_plot4, Device != "hexoskin")
    to_plot_hex4 <- filter(to_plot4, Device != "fitbit")
  }
  
  if (!is.na(id5)) {
    to_plot5 <- heartrate_df(id5, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot5 <- filter(to_plot5, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb5 <- filter(to_plot5, Device != "hexoskin")
    to_plot_hex5 <- filter(to_plot5, Device != "fitbit")
  }
  
  if (!is.na(id6)) {
    to_plot6 <- heartrate_df(id6, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot6 <- filter(to_plot6, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb6 <- filter(to_plot6, Device != "hexoskin")
    to_plot_hex6 <- filter(to_plot6, Device != "fitbit")
  }
  
  if (!is.na(id7)) {
    to_plot7 <- heartrate_df(id7, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot7 <- filter(to_plot7, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb7 <- filter(to_plot7, Device != "hexoskin")
    to_plot_hex7 <- filter(to_plot7, Device != "fitbit")
  }
  
  if (!is.na(id8)) {
    to_plot8 <- heartrate_df(id8, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot8 <- filter(to_plot8, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb8 <- filter(to_plot8, Device != "hexoskin")
    to_plot_hex8 <- filter(to_plot8, Device != "fitbit")
  }
  
  if (!is.na(id9)) {
    to_plot9 <- heartrate_df(id9, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot9 <- filter(to_plot9, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb9 <- filter(to_plot9, Device != "hexoskin")
    to_plot_hex9 <- filter(to_plot9, Device != "fitbit")
  }
  
  if (!is.na(id10)) {
    to_plot10 <- heartrate_df(id2, br_breaks, fb_hr_breaks, h_hr_breaks, filter)
    
    if (!is.null(min) & !is.null(max)) {
      to_plot10 <- filter(to_plot10, date_time >= min_time & date_time <= max_time)
    }
    
    to_plot_fb10 <- filter(to_plot10, Device != "hexoskin")
    to_plot_hex10 <- filter(to_plot10, Device != "fitbit")
  }
  
  if (fb == FALSE) {

    plot <- ggplot(to_plot_hex, aes(x = date_time, y = heartrate)) + 
     # geom_line(data = to_plot_fb, alpha = alpha, aes(color = "A")) + 
      geom_line(alpha = alpha, aes(color = paste0(eval(id), "_hex"))) +
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Heart Rate") +
      scale_color_manual("IDs", values = c("#1f78b4", "#33a02c", "#e31a1c", 
                                           "#ff7f00", "#6a3d9a", "#ffffb3", 
                                           "#fb8072", "#fdb462", "#fccde5", 
                                           "#bc80bd"))  
    
    if (!is.na(id2)) {
      
     plot <- plot + geom_line(data = to_plot_hex2,alpha = alpha, 
                              aes(color = paste0(eval(id2), "_hex")))
     
    }
    
    if (!is.na(id3)) {
      
      plot <- plot + geom_line(data = to_plot_hex3,alpha = alpha, 
                               aes(color = paste0(eval(id3), "_hex")))
      
    }
    
    if (!is.na(id4)) {
      
      plot <- plot + geom_line(data = to_plot_hex4,alpha = alpha, 
                               aes(color = paste0(eval(id4), "_hex")))
      
    }
    
    if (!is.na(id5)) {
      
      plot <- plot + geom_line(data = to_plot_hex5,alpha = alpha, 
                               aes(color = paste0(eval(id5), "_hex")))
      
    }
    
    if (!is.na(id6)) {
      
      plot <- plot + geom_line(data = to_plot_hex6,alpha = alpha, 
                               aes(color = paste0(eval(id6), "_hex")))
      
    }
    
    if (!is.na(id7)) {
      
      plot <- plot + geom_line(data = to_plot_hex7,alpha = alpha, 
                               aes(color = paste0(eval(id7), "_hex")))
      
    }
    
    if (!is.na(id8)) {
      
      plot <- plot + geom_line(data = to_plot_hex8,alpha = alpha, 
                               aes(color = paste0(eval(id8), "_hex")))
      
    }
    
    if (!is.na(id9)) {
      
      plot <- plot + geom_line(data = to_plot_hex9,alpha = alpha, 
                               aes(color = paste0(eval(id9), "_hex")))
      
    }
    
    if (!is.na(id10)) {
      
      plot <- plot + geom_line(data = to_plot_hex10,alpha = alpha, 
                               aes(color = paste0(eval(id10), "_hex")))
      
    }
    
  }
  
  else if (h == FALSE) {

    plot <- ggplot(to_plot_fb, aes(x = date_time, y = heartrate)) + 
      geom_line(alpha = alpha, aes(color = paste0(eval(id), "_fb"))) + 
   #   geom_line(data = to_plot_hex, alpha = alpha, aes(color = "B")) +
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Heart Rate") +
      scale_color_manual("IDs", values = c("#a6cee3", "#b2df8a", "#fb9a99", 
                                           "#fdbf6f", "#cab2d6", "#8dd3c7", 
                                           "#bebada", "#80b1d3", "#b3de69", 
                                           "#d9d9d9")) 
    
    if (!is.na(id2)) {
      plot <- plot + geom_line(data = to_plot_fb2, alpha = alpha, 
                               aes(color = paste0(eval(id2), "_fb")))
    }
    
    if (!is.na(id3)) {
      plot <- plot + geom_line(data = to_plot_fb3, alpha = alpha, 
                               aes(color = paste0(eval(id3), "_fb")))
    }
    
    if (!is.na(id4)) {
      plot <- plot + geom_line(data = to_plot_fb4, alpha = alpha, 
                               aes(color = paste0(eval(id4), "_fb")))
    }
    
    if (!is.na(id5)) {
      plot <- plot + geom_line(data = to_plot_fb5, alpha = alpha, 
                               aes(color = paste0(eval(id5), "_fb")))
    }
    
    if (!is.na(id6)) {
      plot <- plot + geom_line(data = to_plot_fb6, alpha = alpha, 
                               aes(color = paste0(eval(id6), "_fb")))
    }
    
    if (!is.na(id7)) {
      plot <- plot + geom_line(data = to_plot_fb7, alpha = alpha, 
                               aes(color = paste0(eval(id7), "_fb")))
    }
    
    if (!is.na(id8)) {
      plot <- plot + geom_line(data = to_plot_fb8, alpha = alpha, 
                               aes(color = paste0(eval(id8), "_fb")))
    }
    
    if (!is.na(id9)) {
      plot <- plot + geom_line(data = to_plot_fb9, alpha = alpha, 
                               aes(color = paste0(eval(id9), "_fb")))
    }
    
    if (!is.na(id10)) {
      plot <- plot + geom_line(data = to_plot_fb10, alpha = alpha, 
                               aes(color = paste0(eval(id10), "_fb")))
    }
    
  }
  
   else {
    
    plot <- ggplot(to_plot, aes(x = date_time, y = heartrate)) + 
      geom_line(data = to_plot_fb, alpha = alpha, aes(color = paste0(eval(id), "_fb"))) + 
      geom_line(data = to_plot_hex, alpha = alpha, aes(color = paste0(eval(id), "_hex"))) +
      scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                       date_labels = "%I:%M %p") +
      theme_few() + 
      ylab("Heart Rate") +
      scale_color_manual("IDs", values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                                           "#33a02c", "#fb9a99", "#e31a1c", 
                                           "#fdbf6f", "#ff7f00", "#cab2d6", 
                                           "#6a3d9a", "#8dd3c7", "#ffffb3", 
                                           "#bebada", "#fb8072", "#80b1d3", 
                                           "#fdb462", "#b3de69", "#fccde5", 
                                           "#d9d9d9", "#bc80bd")) 
    
    if(!is.na(id2)) {
      
      plot <- plot + geom_line(data = to_plot_fb2, alpha = alpha, 
                               aes(color = paste0(eval(id2), "_fb"))) + 
        geom_line(data = to_plot_hex2, alpha = alpha, aes(color = paste0(eval(id2), "_hex")))
    }
    
    if(!is.na(id3)) {
      
      plot <- plot + geom_line(data = to_plot_fb3, alpha = alpha, 
                               aes(color = paste0(eval(id3), "_fb"))) + 
        geom_line(data = to_plot_hex3, alpha = alpha, aes(color = paste0(eval(id3), "_hex")))
    }
    
    if(!is.na(id4)) {
      
      plot <- plot + geom_line(data = to_plot_fb4, alpha = alpha, 
                               aes(color = paste0(eval(id4), "_fb"))) + 
        geom_line(data = to_plot_hex4, alpha = alpha, aes(color = paste0(eval(id4), "_hex")))
    }
    
    if(!is.na(id5)) {
      
      plot <- plot + geom_line(data = to_plot_fb5, alpha = alpha, 
                               aes(color = paste0(eval(id5), "_fb"))) + 
        geom_line(data = to_plot_hex5, alpha = alpha, aes(color = paste0(eval(id5), "_hex")))
    }
    
    if(!is.na(id6)) {
      
      plot <- plot + geom_line(data = to_plot_fb6, alpha = alpha, 
                               aes(color = paste0(eval(id6), "_fb"))) + 
        geom_line(data = to_plot_hex6, alpha = alpha, aes(color = paste0(eval(id6), "_hex")))
    }
    
    if(!is.na(id7)) {
      
      plot <- plot + geom_line(data = to_plot_fb7, alpha = alpha, 
                               aes(color = paste0(eval(id7), "_fb"))) + 
        geom_line(data = to_plot_hex7, alpha = alpha, aes(color = paste0(eval(id7), "_hex")))
    }
    
    if(!is.na(id8)) {
      
      plot <- plot + geom_line(data = to_plot_fb8, alpha = alpha, 
                               aes(color = paste0(eval(id8), "_fb"))) + 
        geom_line(data = to_plot_hex8, alpha = alpha, aes(color = paste0(eval(id8), "_hex")))
    }
    
    if(!is.na(id9)) {
      
      plot <- plot + geom_line(data = to_plot_fb9, alpha = alpha, 
                               aes(color = paste0(eval(id9), "_fb"))) + 
        geom_line(data = to_plot_hex9, alpha = alpha, aes(color = paste0(eval(id9), "_hex")))
    }
    
    if(!is.na(id10)) {
      
      plot <- plot + geom_line(data = to_plot_fb10, alpha = alpha, 
                               aes(color = paste0(eval(id10), "_fb"))) + 
        geom_line(data = to_plot_hex10, alpha = alpha, aes(color = paste0(eval(id10), "_hex")))
    }
    
  }
  
    
  
  return(plot)
    
}
  

br_plot_mult <- function(ids, br_breaks = "1 min", date_break = "30 min", 
                         min = NULL, max = NULL, alpha = 1) {
  
  if (length(ids == 1)) {
    id <- ids[1]
    id2 <- NULL
    id3 <- NULL
    id4 <- NULL
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 2)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- NULL
    id4 <- NULL
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 3)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- NULL
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 4)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- NULL
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 5)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- NULL
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 6)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- NULL
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 7)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- NULL
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 8)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- ids[8]
    id9 <- NULL
    id10 <- NULL
  }
  
  if (length(ids == 9)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- ids[8]
    id9 <- ids[9]
    id10 <- NULL
  }
  
  if (length(ids == 10)) {
    id <- ids[1]
    id2 <- ids[2]
    id3 <- ids[3]
    id4 <- ids[4]
    id5 <- ids[5]
    id6 <- ids[6]
    id7 <- ids[7]
    id8 <- ids[8]
    id9 <- ids[9]
    id10 <- ids[10]
  }
  
  df <- hexoskin(id, br_breaks)$breathing 
  
  if (!is.null(min) & !is.null(max)) {
    
    late <- c('307n', '308n', '505n', '602n', '600n', '601n')
    tail <- dim(df)[1]
    
    date_min <- format(df$date_time[1], "%Y-%m-%d")
    date_max <- format(df$date_time[1], "%Y-%m-%d")
    
    if(any(ids %in% late)) {
      if(grep("am", min) == 1) {
        date_min <- format(df$date_time[tail], "%Y-%m-%d")
      }
      if(grep("am", max) == 1) {
        date_max <- format(df$date_time[tail], "%Y-%m-%d")
      }
    }
    
    time_min <- substr(min, 1, nchar(min)-3)
    ampm_min <- substr(min, nchar(min)-1, nchar(min))
    
    time_max <- substr(max, 1, nchar(max)-3)
    ampm_max <- substr(max, nchar(max)-1, nchar(max))
    
    min_time <- lubridate::ymd_hms(paste0(date_min, " ", time_min, ":00", " ", 
                                          ampm_min))
    max_time <- lubridate::ymd_hms(paste0(date_max, " ", time_max, ":00", " ", 
                                          ampm_max))
    
    df <- filter(df, date_time >= min_time & date_time <= max_time)
    
  }
  
  plot <- ggplot(df, aes(x = date_time, y = breathing_rate)) + 
    geom_line(aes(color = eval(id))) + 
    theme_few() + 
    ylab("Breathing Rate") + 
    scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                     date_labels = "%I:%M %p") + 
    scale_color_manual("IDs", values = c("#1f78b4", "#33a02c", "#e31a1c", 
                                         "#ff7f00", "#6a3d9a", "#ffffb3", 
                                         "#fb8072", "#fdb462", "#fccde5", 
                                         "#bc80bd"))
  
  if (!is.na(id2)) {
    df2 <- hexoskin(id2, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df2 <- filter(df2, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df2, aes(color = eval(id2)))
    
  }
  
  if (!is.na(id3)) {
    df3 <- hexoskin(id3, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df3 <- filter(df3, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df3, aes(color = eval(id3)))
    
  }
  
  if (!is.na(id4)) {
    df4 <- hexoskin(id4, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df4 <- filter(df4, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df4, aes(color = eval(id4)))
    
  }
  
  if (!is.na(id5)) {
    df5 <- hexoskin(id5, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df5 <- filter(df5, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df5, aes(color = eval(id5)))
    
  }
  
  if (!is.na(id6)) {
    df6 <- hexoskin(id6, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df6 <- filter(df6, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df6, aes(color = eval(id6)))
    
  }
  
  if (!is.na(id7)) {
    df7 <- hexoskin(id7, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df7 <- filter(df7, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df7, aes(color = eval(id7)))
    
  }
  
  if (!is.na(id8)) {
    df8 <- hexoskin(id8, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df8 <- filter(df8, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df8, aes(color = eval(id8)))
    
  }
  
  if (!is.na(id9)) {
    df9 <- hexoskin(id9, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df9 <- filter(df9, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df9, aes(color = eval(id9)))
    
  }
  
  if (!is.na(id10)) {
    df10 <- hexoskin(id10, br_breaks)$breathing
    
    if(!is.null(min) & !is.null(max)) {
      df10 <- filter(df10, date_time >= min_time & date_time <= max_time)
    }
    
    plot <- plot + geom_line(data = df10, aes(color = eval(id10)))
    
  }
 
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


