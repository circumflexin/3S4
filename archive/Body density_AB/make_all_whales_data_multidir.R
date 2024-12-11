dataDir1 <- "D:\\CAS 2019\\Data\\D3\\metadata\\Glide_detection\\Glide_summaries\\"
dataDir2 <- "D:\\3S 2016-17\\Data\\Dtag\\Metadata\\Glides\\Summaries\\"
modelDir <- "D:\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\SW_Models\\"
newdataDIR ="D:\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"

setwd(modelDir)

whales1 <- c("SW19_241a", "SW19_241b", "SW19_245a","SW19_248a")  
whales2 <- c("SW17_184a","SW17_186a","SW17_186b","SW17_188a","SW17_191a")  

prevMax <- 0

for(w in 1:length(whales1)) {
  
  tab <- read.csv(paste(dataDir1, whales1[w], ".csv",sep=""))  
  
  names(tab) <- c(
    
    "start.pt",               # Start point of the glide  
    "end.pt",                 # End point of the glide     
    "duration",               # duration (s) of glide (i.e. all sub-glide has duration of 5s)     
    "mean.depth",             # Mean depth (m) of the glide     
    "delta.depth",            # Total depth change durin a 5-s glide
    "mean.speed",             # Mean swim speed during the glide (m/s)    
    "mean.pitch",             # Mean pitch angle (deg) during the glide (calculated using general statisitics)     
    "sin.pitch",              # Sine of mean pitch      
    "sd.pitch",               # SD of pitch angle (calculated using general statistics)     
    "mean.temp",              # Mean ambient temperature during the glide     
    "DswGG",                  # density of seawater at the glide depth   
    "acceleration",           # Acceleration during the glide (i.e. slope of swim speed vs time)     
    "R",                      # R values for the regression swim speed vs time     
    "se.accel",               # SE of the gradient (acceleration)
    "phase",                  # 0 for bottom phase, -1 for descent phase, 1 for asscent phase     
    "dive",                   # Number of dive in which the glide occurred     
    "dive.max.depth",         # Maximum dive depth (m) of the dive     
    "dive.duration",          # Dive duration (s)     
    "mean.pitch.c",           # Mean pitch angle (deg) during the glide (calculated using circular statistics)     
    "r.pitch",                # Measure of concentration (r) of  pitch angles in 5s     
    "mean.roll",              # Mean roll angle (deg) during the glide (calculated using circular statistics)     
    "r.roll",                 # Measure of concentration (r) of  roll angles in 5s     
    "mean.heading",           # Mean heading angle (deg) during the glide (calculated using circular statistics)     
    "r.heading"              # Measure of concentration (r) of  heading angles in 5s
  )
  
  tab <- tab[is.na(tab$duration)==F,]
  tab$whale.id <- whales1[w]       # whale.id
  tab$dive.all <- as.numeric(as.factor(tab$dive)) + prevMax # index number for all dives (required for jags model)
  
  if(w>1) {
    tab_new <- rbind(tab_new, tab)
  } else {
    tab_new <- tab
  }
  prevMax <- max(tab_new$dive.all)
  
}
tab1 <- tab_new

for(w in 1:length(whales2)) {
  
  tab <- read.csv(paste(dataDir2, whales2[w], ".csv",sep=""))  
  
  names(tab) <- c(
    
    "start.pt",               # Start point of the glide  
    "end.pt",                 # End point of the glide     
    "duration",               # duration (s) of glide (i.e. all sub-glide has duration of 5s)     
    "mean.depth",             # Mean depth (m) of the glide     
    "delta.depth",            # Total depth change durin a 5-s glide
    "mean.speed",             # Mean swim speed during the glide (m/s)    
    "mean.pitch",             # Mean pitch angle (deg) during the glide (calculated using general statisitics)     
    "sin.pitch",              # Sine of mean pitch      
    "sd.pitch",               # SD of pitch angle (calculated using general statistics)     
    "mean.temp",              # Mean ambient temperature during the glide     
    "DswGG",                  # density of seawater at the glide depth   
    "acceleration",           # Acceleration during the glide (i.e. slope of swim speed vs time)     
    "R",                      # R values for the regression swim speed vs time     
    "se.accel",               # SE of the gradient (acceleration)
    "phase",                  # 0 for bottom phase, -1 for descent phase, 1 for asscent phase     
    "dive",                   # Number of dive in which the glide occurred     
    "dive.max.depth",         # Maximum dive depth (m) of the dive     
    "dive.duration",          # Dive duration (s)     
    "mean.pitch.c",           # Mean pitch angle (deg) during the glide (calculated using circular statistics)     
    "r.pitch",                # Measure of concentration (r) of  pitch angles in 5s     
    "mean.roll",              # Mean roll angle (deg) during the glide (calculated using circular statistics)     
    "r.roll",                 # Measure of concentration (r) of  roll angles in 5s     
    "mean.heading",           # Mean heading angle (deg) during the glide (calculated using circular statistics)     
    "r.heading"              # Measure of concentration (r) of  heading angles in 5s
  )
  
  tab <- tab[is.na(tab$duration)==F,]
  tab$whale.id <- whales2[w]       # whale.id
  tab$dive.all <- as.numeric(as.factor(tab$dive)) + prevMax # index number for all dives (required for jags model)
  
  if(w>1) {
    tab_new <- rbind(tab_new, tab)
  } else {
    tab_new <- tab
  }
  prevMax <- max(tab_new$dive.all)
  
}

tab2 <- tab_new

tab = rbind(tab1,tab2)

save(tab, file=paste(newdataDIR, "all_whales.Rd", sep=""))


