setwd("~/Documents/gs/fitbit_hexoskin/data") # directory where Fitabase csv files are saved
files <- list.files()
names <- substr(files, 1, nchar(files)-4)

for (i in 1:length(files)) { 
  
  data <- read.csv(files[i])
  saveRDS(data, paste0(names[i], ".rds"))
  
}

