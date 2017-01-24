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
        geom_line(alpha = alpha) + 
        scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                         date_labels = "%I:%M %p") +
        theme_few() + 
        ylab("Average Heart Rate") + 
        scale_color_manual(values = "#377EB8")
    }
    
    else if (h == FALSE) {
      to_plot <- filter(to_plot, Device != "hexoskin")
      
      plot <- ggplot(to_plot, aes(x = date_time, y = average_heartrate, color = Device)) + 
        geom_line(alpha = alpha) + 
        scale_x_datetime(name = NULL, breaks = scales::date_breaks(date_break), 
                         date_labels = "%I:%M %p") +
        theme_few() + 
        ylab("Average Heart Rate") + 
        scale_color_manual(values = "#E41A1C")
    }
    
    else {
      
      plot <- ggplot(to_plot, aes(x = date_time, y = average_heartrate, color = Device)) + 
        geom_line(alpha = alpha) + 
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
