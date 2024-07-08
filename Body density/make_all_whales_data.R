#setwd("/Users/georgesato/R/BD_estimation/oo23/")
dataDir<-".//oo_glides//"
modelDir<-".//oo_models//"
whales <- c("oo23_295b", "oo23_297b", "oo23_299a", "oo23_299b", "oo23_301a", "oo23_302a")  

prevMax <- 0

for(w in 1:length(whales)) {
  
  tab <- read.csv(paste(dataDir, whales[w], ".csv",sep=""))  
  
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
    "r.heading"               # Measure of concentration (r) of  heading angles in 5s
  )
  
  tab <- tab[is.na(tab$duration)==F,]
  tab$whale.id <- whales[w]       # whale.id
  tab$whale.num <- w              # whale.id in numeric
  
  tab$dive.num <- as.numeric(as.factor(tab$dive))   
  tab$dive.all <- tab$dive.num + prevMax
  
  if(w>1) {
    tab_new <- rbind(tab_new, tab)
  } else {
    tab_new <- tab
  }
  prevMax <- max(tab_new$dive.all)
  
}

tab <- tab_new

plot(tab$dive.num, col=tab$whale.num) # what is this for?
plot(tab$dive.all, col=tab$whale.num)
plot(tab$acceleration, col=tab$whale.num)
plot(tab$mean.depth,tab$acceleration)

save(tab, file=paste(".\\Data\\", "all_whales.Rd", sep=""))


