#to do:
# see if a solution for lag() exists in base R

dataDir <- "D:\\Analysis\\3S4\\Body density_AB\\Data\\"
modelDir <- "D:\\Analysis\\3S4\\Body density_AB\\oo_models_GS\\"
downstreamDir = "D:\\Analysis\\3S4\\Body density_AB\\Data\\"

setwd(modelDir)

library(R2jags)
require(dplyr)
set.seed(1990)

## load & filter data 

# -> defines "fitName" that is used to store model output in working directory
# store filtered data & the names of whales and dives that are included

load(paste(dataDir,"all_whales.Rd",sep=""))

filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>60 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
  is.na(tab$mean.speed)==F & tab$r.roll>0.9 & tab$r.pitch>0.9 & tab$r.heading>0.9  & is.na(tab$DswGG)==F
tab <- tab[filterBool & !is.na(filterBool),]   


# #AB- look for and thin consecutive subglides
lag_end = lag(tab$end.pt,1,FALSE)
is_sub = lag_end == tab$start.pt # where start time is equal to end time for glide(n-1)
sub_index = ave(is_sub, cumsum(!is_sub), FUN = cumsum)+1 #subglide counter
tab$sg.index = sub_index # replace subglide index
tab <- tab[tab$sg.index%%2!=0,] #drop even numbers of subglides 


#AB - drop whales where the thinning takes n below 20
test <- tapply(tab$whale.id,tab$whale.id,length)
keepw <- names(test[test>20])
dropw <- names(test[test<20])
print("following whales dropped after thinning:")
for (x in dropw) {
  print(x)
}
tab <- tab[tab$whale.id %in% keepw,]

fitName <- "_f_pitch30_depth100_rpitch999_thinned"

# Which numeric identifier corresponds to which whale/dive
fit.whales <- tapply(tab$whale.id, as.numeric(as.factor(tab$whale.id)), unique)
fit.dives <- tapply(tab$dive.all, as.numeric(as.factor(tab$dive.all)), unique)
  
#save(tab, filterBool, fit.whales, fit.dives, file=paste("data", fitName, ".Rd", sep=""))
save(tab, filterBool, fit.whales, fit.dives, file=paste(downstreamDir,"data", fitName, ".Rd", sep=""))


  
# Summary of model structures

# Type of variability in parameter:
  # D = Dive-by-dive variability
  # I = Individual variability
  # G = Parameter pooled across individuals and dives

  modTab <- data.frame(
    modNum    = c(1,  2,  3,  4,  5,  6,  7,  8,  9,  10,  11,  12),
    Vair.type = c("D","D","D","G","G","G","I","I","I","G","I","D"),  # Vair term
    CdAM.type = c( "G","I","G","G","I","G","G","I","G","I", "I", "I"), # drag term
    BD.type   = c( "I","G","G","I","G","G","I","G","G","I", "I", "I")) # body density

  modelName <- paste("model(oo", modTab$modNum, ")", sep="")

## Loop over all 12 models

for(m in 1:length(modelName)) {

      ## Re-organise data for jags
      
        sp.data <- list(N=length(tab$duration))
        
        sp.data$a <- tab$acceleration
        sp.data$DswGG <- tab$DswGG
        sp.data$mean.speed <- tab$mean.speed
        sp.data$sin.pitch <- tab$sin.pitch
        sp.data$depth <- tab$mean.depth
        sp.data$tau <- (1/(tab$se.accel+0.001))^2   ## use measured SE for tau  
        
        # Indexing for individual variability
        if(modTab$Vair.type[m]=="I" | modTab$CdAM.type[m]=="I" | modTab$BD.type[m]=="I") {
          sp.data$NW <- length(unique(as.numeric(as.factor(tab$whale.id))))
          sp.data$whale.id <- as.numeric(as.factor(tab$whale.id))
        }
          
        # Indexing for dive-by-dive variability
        if(modTab$Vair.type[m]=="D") {
          sp.data$ND <- length(unique(as.numeric(as.factor(tab$dive.all))))
          sp.data$dive.id <- as.numeric(as.factor(tab$dive.all))
        }
      
      # Monitored parameters
      
        # Parameters in all models
        sp.params <- c("CdAM.g", "body.density.g", "Vair",
                       "compr", "a.mu", "body.density.t") # when tau is not estimated
        
        # individual CdAM
        if(modTab$CdAM.type[m]=="I") {sp.params <- c(sp.params, c("CdAM", "CdAM.var"))} 
        
        # individual body.density
        if(modTab$BD.type[m]=="I") {sp.params <- c(sp.params, c("body.density", "body.var"))}
  
        # individual or dive-by-dive Vair
        if(modTab$Vair.type[m]=="I" | modTab$Vair.type[m]=="D") {
          sp.params <- c(sp.params, c("Vair.var", "Vair.d"))}

      
      ## Load initial values
        setwd('..')
        source("make_inits.R")
        setwd(modelDir)
        
        # Number of initial values for each parameter
        n.CdAM <- 1
        n.body.density <- 1
        n.body.density.g <- 1
        n.Vair <- 1
        n.Vair.var <- 1
        n.compr <- 1
        n.loc <-1
        
        if(modTab$CdAM.type[m]=="I") {n.CdAM <- sp.data$NW}
        if(modTab$BD.type[m]=="I") {n.body.density <- sp.data$NW}
        if(modTab$Vair.type[m]=="I") {n.Vair <- sp.data$NW}
        if(modTab$Vair.type[m]=="D") {n.Vair <- sp.data$ND}
      
      ## Check data and monitored parameters
      
        print(str(sp.data))
        print(sp.params)
        
      ## Fit model
      
      fit = jags(data=sp.data, inits=sp.inits, parameters.to.save=sp.params, 
                 model.file=paste(modelName[m], ".txt",sep=""),
                 n.chains=3, n.iter=24000, n.burnin=12000, 
                 n.thin=max(1, floor(3 * (24000-12000) /1000)))
      
      
      ## Save model inputs and outputs
      
        #save(sp.data, sp.params, fit, filterBool, 
         #    file=paste(modelName[m], fitName, ".Rd", sep=""))
        
        save(sp.data, sp.params, fit, filterBool, 
             file=paste(downstreamDir,modelName[m], fitName, ".Rd", sep=""))

} # loop for different models


