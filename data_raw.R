dir_files <- "~/Desktop/newnew"
setwd(dir_files)

files <- list.files()

names <- substr(files, 1, nchar(files)-4)

for (i in 1:length(files)) { 
  
  data <- read.csv(files[i])
  saveRDS(data, paste0(names[i], ".rds"))
  
}


fb <- grep('heartrate|Steps', list.files(), value = TRUE)
file.remove(fb)

hex <- list.files()
hex_new <- paste0(substr(hex, 1, 4), "_hexoskin.rds")
file.rename(hex, hex_new)


rds <- list.files()
ids <- unique(substr(rds, 1, 4))
new_names <- paste0(ids, "_hexoskin.rds")

hex_move <- grep('.rds', list.files(), value = TRUE)
file.copy(hex_move, dir)


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


setwd('~/Documents/gs/R/fitbit_hexoskin/data')
hex_move <- grep("hexoskin", list.files(), value = TRUE)
new_dir <- "~/Documents/gs/R/fitbit_hexoskin/old_hexoskin"
file.copy(hex_move, new_dir)


