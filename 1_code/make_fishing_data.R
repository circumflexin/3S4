
#housekeeping ---- 
library(momentuHMM)
library(dplyr)
library(lubridate)


ups_dir = ".\\0_data\\"
out_dir = ".\\2_pipeline\\HMMs\\"
res = 1.5 
deploys = c("oo23_292b_pt", "oo23_295a_pt", "oo23_295b_pt", "oo23_295b_pt", "oo23_299a_pt","oo23_299b_pt", "oo23_301a_pt", "oo23_302a_pt")
catch = read.csv("D:\\Analysis\\3S4\\0_data\\Catch_data\\3S4-Herring-Oct23.csv")
catch$startPX = as.POSIXct(paste(catch$Start.date,catch$Start.clock),"%d.%m.%Y %H:%M", tz = "UTC")
catch$stopPX = as.POSIXct(paste(catch$Stop.date,catch$Stop.clock),"%d.%m.%Y %H:%M", tz = "UTC")
catch$interv = interval(catch$startPX, catch$stopPX)


# join fishing data
deploy = "oo23_302a_pt"
track = R.matlab::readMat(paste(ups_dir, "Pseuedotracks\\",deploy,".mat", sep = ""))
rel = read.csv(paste(".\\2_pipeline\\make_dsfb\\",deploy,"_relAIS.csv", sep = ""),header = FALSE )
for(i in seq_along(rel[,1])){
  boat = read.csv(paste(ups_dir, "AIS data\\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO\\", rel[i,], sep = ""))
  boat$datetime_PX = as.POSIXct(boat$date_time_utc, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  catch_boat = catch[catch$ID.boat == boat$name_ais[1],]
  boat$isFISH = sapply(boat$datetime_PX, FUN =  function(x) any(x > catch_boat$startPX & x < catch_boat$stopPX))
  if(i == 1){mas = boat}
  else{mas = rbind(mas,boat)}
}



