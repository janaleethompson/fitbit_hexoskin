dir_files <- "~/Desktop/new"
setwd(dir_files)

files <- list.files()

names <- substr(files, 1, nchar(files)-4)

for (i in 1:length(files)) { 
  
  data <- read.csv(files[i])
  saveRDS(data, paste0(names[i], ".rds"))
  
}


move <- grep('.rds', list.files(), value = TRUE)
dir <- "~/Documents/gs/R/fitbit_hexoskin/data"
file.copy(move, dir)

# for fitbit files: 

setwd(dir)

hr <- grep("heartrate", list.files(), value = TRUE)
hr_new <- paste0(substr(hr, 1, 4), "_fb_hr.rds")
file.rename(hr, hr_new)

step <- grep("steps", list.files(), value = TRUE)
step_new <- paste0(substr(step, 1, 4), "_fb_steps.rds")
file.rename(step, step_new)


# missing data for 209n 


setwd('~/Documents/gs/R/fitbit_hexoskin')

