#setwd("/Users/georgesato/R/BD_estimation/oo23/")
dataDir<-".//Data//"
modelDir<-".//oo_models_GS//"

library(R2jags)
set.seed(0)

# load & filter data -> fitName
load(paste(dataDir,"all_whales.Rd", sep = ""))
filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>60 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
  is.na(tab$mean.speed)==F & tab$r.roll>0.9 & tab$r.pitch>0.9 & tab$r.heading>0.9  & is.na(tab$DswGG)==F
              #& tab$mean.depth>200 & tab$sonar==0 & tab$vertglide==1
tab <- tab[filterBool,]    
tab <- tab[rowSums(is.na(tab)) != ncol(tab), ] # AB - Added this to drop robs where all columns were NAs as this was breaking JAGS

save(tab, file="raw_glides_filtered.Rd")

fitName <- "_f_pitch30_depth100"


#modTab <- read.csv("list_of_models.csv") 

modelName <- paste("model(", 1:12, ")", sep="") # third batch of models#1:12
whales <- unique(tab$whale.id)

modNum<-c(1:12)#1,2,3,4,5,6,7,8,9,10,11,12
modelName <- paste("model(oo", 1:12, ")", sep="") # third batch of models
whales <- unique(tab$whale.id)

for (j in 1:length(modNum)){
  
  m<-modNum[j]

#for(m in 1:12) {

      # create data for jags
      
      sp.data <- list(N=length(tab$duration))
      
      sp.data$a <- tab$acceleration
      sp.data$DswGG <- tab$DswGG
      sp.data$mean.speed <- tab$mean.speed
      sp.data$sin.pitch <- tab$sin.pitch
      sp.data$depth <- tab$mean.depth
      
      sp.data$NW <- length(unique(tab$whale.num))
      sp.data$whale.id <- as.numeric(as.factor(tab$whale.num))
             
      sp.data$tau <- (1/(tab$se.accel+0.001))^2         ## use measured SE for tau  
      
      # whale-by-whale variability in Vair
      if(is.na(modTab$Vair.d[m]) & is.na(modTab$Vair.i[m])) {
        n.Vair <- sp.data$NW
        } else {
          # Dive-by-dive variability in Vair
          if(is.na(modTab$Vair.d[m])) {
            sp.data$ND <- length(unique(tab$dive.all))
            sp.data$dive.id <- as.numeric(as.factor(tab$dive.all))
            n.Vair <- sp.data$ND
          } else {
            # global Vair only
            n.Vair <- 1
          }
        }
      
      # all models
      sp.params <- c("compr", "Vair", "a.mu", "body.density.t") #"tau", is removed
      
      # CdAM.g
      if(is.na(modTab$CdAM.g[m])) {
        sp.params <- c(sp.params, c("CdAM.g", "CdAM.var"))
      } 
      # CdAM
      if(is.na(modTab$CdAM[m])) {
        sp.params <- c(sp.params, c("CdAM"))
      } 
      
      # body.density.g
      if(is.na(modTab$body.density.g[m])) {
        sp.params <- c(sp.params, c("body.density.g", "body.var"))
      } 
      # body.density
      if(is.na(modTab$body.density[m])) {
        sp.params <- c(sp.params, c("body.density"))
      }

      # Vair
      if(is.na(modTab$Vair.d[m])) {
        sp.params <- c(sp.params, c("Vair.var", "Vair.d"))
      }

      
      ## load initial values
      
      source("make_inits.R")
      
      n.CdAM <- sp.data$NW
      n.body.density <- sp.data$NW
      n.body.density.g <- 1
      n.compr <- 1
      n.Vair.var <- 1
      n.loc<-1
    
      # Vair.d, CdAM and body.density only exist when they dive and individual-specific, respectively
      # but tau can change size:
  #    if(is.na(modTab$tau[m])) {n.tau <- sp.data$NW}         #removed because now we use measered tau
    
      print(str(sp.data))
      print(sp.params)
      
      # fit model
      fit = jags(data=sp.data, inits=sp.inits, parameters.to.save=sp.params, 
                 model.file=paste(modelDir,modelName[m], ".txt",sep=""),
                 n.chains=3, n.iter=30000, n.burnin=12000, 
                 n.thin=max(1, floor(3 * (30000-12000) /1000)))
      
      
      # save model
      save(sp.data, sp.params, fit, file=paste(modelName[m], fitName, ".Rd", sep=""))

} # loop for different models


