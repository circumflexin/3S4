#to do:
# see if a solution for lag() exists in base R

dataDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"
modelDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\SW_Models\\"

setwd(modelDir)

library(R2jags)
require(dplyr)
set.seed(1990)

## load & filter data 

# -> defines "fitName" that is used to store model output in working directory
# store filtered data & the names of whales and dives that are included

load(paste(dataDir,"all_whales.Rd",sep=""))

filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>100 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
  is.na(tab$mean.speed)==F & tab$r.roll>0.9  & is.na(tab$DswGG)==F & tab$r.pitch>0.999
#& tab$mean.depth>200 & tab$sonar==0 & tab$vertglide==1
tab <- tab[filterBool & !is.na(filterBool),]   

# #AB- look for and thin consecutive subglides
lag_end = lag(tab$end.pt,1,FALSE)
is_sub = lag_end == tab$start.pt # where start time is equal to end time for glide(n-1)
sub_index = ave(is_sub, cumsum(!is_sub), FUN = cumsum)+1 #subglide counter
tab$sg.index = sub_index # replace subglide index
tab <- tab[tab$sg.index%%2!=0,] #drop even numbers of subglides 


#AB - drop whales where the thinning takes n below 20
keep <- tapply(tab$whale.id,tab$whale.id,length)
keepw <- names(keep[keep>20])
dropw <- names(keep[keep<20])
print("following whales dropped after thinning:")
for (x in dropw) {
  print(x)
}

tab <- tab[tab$whale.id %in% keepw,]

fitName <- "_f_pitch30_depth100_rpitch999_thinned"

# Which numeric identifier corresponds to which whale/dive
fit.whales <- tapply(tab$whale.id, as.numeric(as.factor(tab$whale.id)), unique)
fit.dives <- tapply(tab$dive.all, as.numeric(as.factor(tab$dive.all)), unique)