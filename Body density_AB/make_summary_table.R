######## Make summary table for estimates of the selected model

modelDir <- "D:\\Analysis\\SW sonar chapter\\statistical_analysis\\data\\body_density\\"
downstreamDir = "D:\\Data\\Sonar_response\\"


setwd(modelDir)

library(R2jags)

# Select model

fitName <- "_f_pitch30_depth100_rpitch999_thinned"      # name of the fit, based on selection of "good glides"
modelName <- "model(SW12)"              # insert best model name here

# Load modelled data and model results

# tab = filtered data frame of glides
# filterBool = true/false vector that identifies included data from original data (all_whales.Rd)
# fit.whales = list of included whales
# fit.dives = list of included dives

load(paste("data", fitName, ".Rd", sep=""))
load(paste(modelName, fitName, ".Rd", sep=""))


### make summary table for GLOBAL mean
  
  summary.g <- data.frame(fit$BUGSoutput$summary[c("body.density.g","CdAM.g","Vair","compr"),c("mean", "2.5%", "97.5%")])
  names(summary.g)[2:3] <- c("L95", "U95")
  summary.g$range95 <- summary.g$U95-summary.g$L95
  summary.g <- signif(summary.g, digits=3)
  
  write.csv(summary.g, file=paste(downstreamDir, modelName, fitName, "_Estimates_global.csv", sep=""))

### make summary table for WHALE-BY-WHALE mean
  
  summary.indiv <- data.frame(ID=fit.whales)
  summary.indiv$numGliges <- NA
  for(j in 1:length(fit.whales)){
    summary.indiv[j,2]<-sum(tab$whale.id==fit.whales[j])    #Number of glides used for the model 
  }
  
  # when the selected model include indiv-specific body density
  if(ncol(fit$BUGSoutput$sims.list$body.density)==length(fit.whales)){  
    
    tempresult <- data.frame(fit$BUGSoutput$summary[paste("body.density[",1:length(fit.whales),"]", sep=""),
                                         c("mean", "2.5%", "97.5%")])
    colnames(tempresult)[1:3] <- c("BD.mean", "BD.L95", "BD.U95")
    tempresult$BD.range95 <- signif(tempresult[,3]-tempresult[,2],digits=3)
    tempresult[,1:3] <- signif(tempresult[,1:3], digits=7)
    summary.indiv <- cbind(summary.indiv, tempresult)         
  }           
  
  # when the selected model include indiv-specific CdAM
  if(ncol(fit$BUGSoutput$sims.list$CdAM)==length(fit.whales)){ 

      tempresult <- data.frame(fit$BUGSoutput$summary[paste("CdAM[",1:length(fit.whales),"]", sep=""),
                                                      c("mean", "2.5%", "97.5%")])
      colnames(tempresult)[1:3] <- c("CdAM.mean", "CdAM.L95", "CdAM.U95")
      tempresult$CdAM.range95 <- round(tempresult[,3]-tempresult[,2],3)
      tempresult[,1:3] <- round(tempresult[,1:3], 3)
      summary.indiv <- cbind(summary.indiv, tempresult)  
    }
    
  # when the selected model include indiv-specific Vair
  if(ncol(fit$BUGSoutput$sims.list$Vair.d)==length(fit.whales)){  
    
    tempresult <- data.frame(fit$BUGSoutput$summary[paste("Vair.d[",1:length(fit.whales),"]", sep=""),
                                                    c("mean", "2.5%", "97.5%")])
    colnames(tempresult)[1:3] <- c("Vair.mean", "Vair.L95", "Vair.U95")
    tempresult$Vair.range95 <- round(tempresult[,3]-tempresult[,2],3)
    tempresult[,1:3] <- round(tempresult[,1:3], digits=3)
    summary.indiv <- cbind(summary.indiv, tempresult)  
  }
    
write.csv(summary.indiv, file=paste(downstreamDir, modelName, fitName, "_Estimates_indiv.csv", sep=""))


### make summary table for DIVE-BY-DIVE mean

  # only when the selected model include dive-by-dive Vair
  if(length(fit$BUGSoutput$mean$Vair.d)==length(fit.dives)){             
    
    summary.d <- data.frame(dive.ID=fit.dives)
    
    for(j in 1:length(fit.dives)){
      dBool <- tab$dive.all==fit.dives[j] # Select glides that the dive index refers to
      summary.d[j,"ID"] <- unique(tab$whale.id[dBool]) 
      summary.d[j,"numGlides"] <- sum(dBool)       
      summary.d[j,"dive.number"] <- unique(tab$dive[dBool])
      summary.d[j,"dive.all"] <- unique(tab$dive.all[dBool])
      summary.d[j,"dive.max.depth"] <- unique(tab$dive.max.depth[dBool])
      summary.d[j,"dive.duration"] <- unique(tab$dive.duration[dBool])
      }
    
    tempresult <- data.frame(fit$BUGSoutput$summary[paste("Vair.d[",1:length(fit.dives),"]", sep=""),
                                                    c("mean", "2.5%", "97.5%")])
    colnames(tempresult)[1:3] <- c("Vair.mean", "Vair.L95", "Vair.U95")
    tempresult$Vair.range95 <- round(tempresult[,3]-tempresult[,2],3)
    tempresult[,1:3] <- round(tempresult[,1:3], digits=3)
    summary.d <- cbind(summary.d, tempresult)  
    
    write.csv(summary.d, file=paste(downstreamDir, modelName, fitName, "_Estimates_dive.csv", sep=""))    
    
    }

dens_sum = list("global" = summary.g, "indiv" = summary.indiv, "dive" = summary.d)
save(dens_sum, file = paste(downstreamDir, "dens_data.rd",sep = ""))  



