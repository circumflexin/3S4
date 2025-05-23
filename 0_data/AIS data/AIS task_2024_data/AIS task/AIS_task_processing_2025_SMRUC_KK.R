### 3S4 project - AIS data processing
## SMRU Consulting
## Katarina Klementisova

library(readr)
library(dplyr)
library(tools)

###################################################################################################################################
############################################### Vessels with IMO - Oct01 - Noc30 2024 #############################################
###################################################################################################################################

# dataset with validated vessel types
vessels_IMO <- read_csv("outputs/3S_uniqueVessels_IMO_validated_categories_consolidated.csv")

setwd("Y:/752 3S/Vessel type validation 2025/csvs")
dirx <- dir("Y:/752 3S/Vessel type validation 2025/csvs")
mylist <- file_path_sans_ext(dirx) 

all_data <- data.frame()
all_data <- do.call(rbind,lapply(dirx,read.csv))

setwd("Y:/752 3S/Vessel type validation 2025/Individual vessel AIS tracks_Oct01 - Nov30 2024_vessels with IMO")

for (i in 1:nrow(vessels_IMO)) {
  print(paste("Exporting IMO", vessels_IMO$IMO[i], "loop", i, "out of", nrow(vessels_IMO), sep = " "))
  tmp <- all_data %>%
    filter(imo == vessels_IMO$IMO[i]) %>%
    # keep both original and validated vessel types
    mutate(orig_shiptype = vessels_IMO$original_shiptype[i],
           validated_shiptype = vessels_IMO$ClassType[i])
  
  if (nrow(tmp) > 0) { # only export data for vessels that have min 1 data row
    
    tablename <- paste("AIS track_vessel Oct01 - Nov30 2024 IMO ", vessels_IMO$IMO[i], "_MMSI ", vessels_IMO$MMSI[i], ".csv", sep = "")
    write.csv(tmp, tablename, row.names = FALSE)
    
  } else next
  
}

