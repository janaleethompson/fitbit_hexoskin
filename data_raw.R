dir_files <- "~/Desktop"
setwd(dir_files)

files <- list.files()[2:4]

names <- substr(files, 1, nchar(files)-4)

for (i in 1:length(files)) { 
  
  data <- read.csv(files[i])
  saveRDS(data, paste0(names[i], ".rds"))
  
}


move <- grep('.rds', list.files(), value = TRUE)
dir <- "~/Documents/gs/R/fitbit_hexoskin/data"
file.copy(move, dir)

# for fitbit files: 

setwd("~/Desktop/new")

hr <- grep("heartrate", list.files(), value = TRUE)
hr_new <- paste0(substr(hr, 1, 4), "_fb_hr.rds")
file.rename(hr, hr_new)

step <- grep("Steps", list.files(), value = TRUE)
step_new <- paste0(substr(step, 1, 4), "_fb_steps.rds")
file.rename(step, step_new)


# missing data for 209n 

# cleaning stats file 

df <- readRDS('data/id_stats.rds')
colnames(df) <- c('id', 'dob', 'gender', 'age', 'pred_max_hr', 'dept', 'steps', 
                  'distance_miles', 'notes', 'average_hr', 'perc_of_max', 
                  'calories', 'weight_kg', 'height_cm', 'bmr')


setwd('~/Documents/gs/R/fitbit_hexoskin')

# METs 

mets <- list.files('~/Documents/gs/R/JT/METs')
names <- paste0(substr(mets, 1, 4), "_mets")

for (i in 1:length(mets)) {
  data <- read.csv(paste0("~/Documents/gs/R/JT/METs/", mets[i]))
  saveRDS(data, file = paste0("~/Documents/gs/R/fitbit_hexoskin/data/", names[i], ".rds"))
}

for (i in ids) {
  
  possibleError <- tryCatch({
    
    df <- mets(i)
    saveRDS(df, file = paste0("data/", i, "_mets_clean.rds"))
    
  }
  , 
  error = function(e) {
    e
    message(paste0("Error with ID ", i, "."))
  }
  )
  if (inherits(possibleError, "error")) next 
  
}
