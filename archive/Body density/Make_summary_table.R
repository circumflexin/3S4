######## Make summary table for estimates of the selected model

setwd("D:\\Analysis\\3S4\\Body density\\")
dataDir<-"D:\\Analysis\\3S4\\Body density\\Data\\"
modelDir<-"D:\\Analysis\\3S4\\Body density\\oo_models_GS\\"

### SELECT THE MODEL
modelName<-c("model(oo12)")   ############ set the selected model

# load & filter data -> fitName
load(paste(dataDir,"all_whales.Rd", sep = ""))
filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>60 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
  is.na(tab$mean.speed)==F & tab$r.roll>0.9 & tab$r.pitch>0.9 & tab$r.heading>0.9  & is.na(tab$DswGG)==F
tab <- tab[filterBool & !is.na(filterBool),]   

fitName <- "_f_pitch30_depth100_rpitch999_thinned"

whales <- unique(tab$whale.id)

# load the selected model
m<-1
load(paste(modelDir,modelName[m], fitName, ".Rd", sep=""))


### make summary table for GLOBAL mean
summary.g<-matrix(data=NA, nrow = 1, ncol = 16)
colnames(summary.g)<-c("meanBD.g", "L95BD.g", "U95BD.g","95rangeBD.g", "meanCdAM.g", "L95CdAM.g", "U95CdAM.g", "95rangeCdAM.g",
                       "meanVair", "L95Vair", "U95Vair", "95rangeVair.g", "meanCompr", "L95Compr", "U95Compr", "95rangeCompr")

  summary.g[1]<-signif(mean(fit$BUGSoutput$sims.list$body.density.g), digits = 5)                 # mean global body density
  summary.g[2]<-signif(quantile(fit$BUGSoutput$sims.list$body.density.g, prob=0.025), digits=5)  #lower 95% CRI global body density
  summary.g[3]<- signif(quantile(fit$BUGSoutput$sims.list$body.density.g, prob=0.975), digits=5)  #upper 95% CRI global body density
  summary.g[4]<- signif((quantile(fit$BUGSoutput$sims.list$body.density.g, prob=0.975) 
                  - quantile(fit$BUGSoutput$sims.list$body.density.g, prob=0.025)), digits=5)     # 95%CRI range for global body density
  summary.g[5]<-signif(mean(fit$BUGSoutput$sims.list$CdAM.g), digits = 3)              # mean global CdAM
  summary.g[6]<-signif(quantile(fit$BUGSoutput$sims.list$CdAM.g, prob=0.025), digits=3)  #lower 95% CRI global CdAM
  summary.g[7]<- signif(quantile(fit$BUGSoutput$sims.list$CdAM.g, prob=0.975), digits=3)  #upper 95% CRI global CdAM
  summary.g[8]<- signif((quantile(fit$BUGSoutput$sims.list$CdAM.g, prob=0.975)
                  -quantile(fit$BUGSoutput$sims.list$CdAM.g, prob=0.025)), digits=3)    # 95%CRI range for global CdAM
  summary.g[9]<-signif(mean(fit$BUGSoutput$sims.list$Vair), digits = 6)                # mean global global Vair
  summary.g[10]<-signif(quantile(fit$BUGSoutput$sims.list$Vair, prob=0.025), digits=3)  #lower 95% CRI global Vair
  summary.g[11]<- signif(quantile(fit$BUGSoutput$sims.list$Vair, prob=0.975), digits=3)  #upper 95% CRI global Vair
  summary.g[12]<- signif((quantile(fit$BUGSoutput$sims.list$Vair, prob=0.975)
                  - quantile(fit$BUGSoutput$sims.list$Vair, prob=0.025)), digits=3)      # 95%CRI range for global Vair
  summary.g[13]<-signif(mean(fit$BUGSoutput$sims.list$compr), digits = 3)                # mean compressibility
  summary.g[14]<-signif(quantile(fit$BUGSoutput$sims.list$compr, prob=0.025), digits=3)  #lower 95% CRI compressibility
  summary.g[15]<- signif(quantile(fit$BUGSoutput$sims.list$compr, prob=0.975), digits=3)  #upper 95% CRI compressibility
  summary.g[16]<- signif((quantile(fit$BUGSoutput$sims.list$compr, prob=0.975) 
                          -quantile(fit$BUGSoutput$sims.list$compr, prob=0.025)), digits=3)    # 95%CRI range for compressibility
  
write.csv(summary.g, file=paste(modelDir, modelName[m], fitName, "_Estimates_global.csv", sep=""))

### make summary table for WHALE-BY-WHALE mean
if(length(fit$BUGSoutput$mean$body.density)==length(whales) ||length(fit$BUGSoutput$mean$CdAM)==length(whales)||
     length(fit$BUGSoutput$mean$Vair.d)==length(whales)){
      summary.indiv<-matrix(data=NA, nrow=length(whales), ncol=14)
      colnames(summary.indiv)<-c("ID", "numGlides", "meanBD", "L95BD", "U95BD", "95rangeBD", "meanCdAM", "L95CdAM", 
                                 "U95CdAM", "95rangeCdAM", "meanVair", "L95Vair", "U95Vair",  "95rangeVair")      
}
    summary.indiv[,1]<-whales
    for(j in 1:length(whales)){
      filterbool2<-tab$whale.num==j
      tab2<-tab[filterbool2,]
      summary.indiv[j,2]<-nrow(tab2)    #Number of glides used for the model 
    }
  
  # when the selected model include indiv-specific Vair  
  if(ncol(fit$BUGSoutput$sims.list$body.density)==length(whales)){  
    for(j in 1:length(whales)){
      summary.indiv[j,3]<-signif(mean(fit$BUGSoutput$sims.list$body.density[,j]), digits=6)    #mean whale-by-whale body denity
      summary.indiv[j,4]<-signif(quantile(fit$BUGSoutput$sims.list$body.density[,j], prob=0.025), digits=5)  #L95% CRI mean whale-by-whale body denity
      summary.indiv[j,5]<- signif(quantile(fit$BUGSoutput$sims.list$body.density[,j], prob=0.975), digits=5)  #U95% CRI mean whale-by-whale body denity
      summary.indiv[j,6]<-signif((quantile(fit$BUGSoutput$sims.list$body.density[,j], prob=0.975)
                            -quantile(fit$BUGSoutput$sims.list$body.density[,j], prob=0.025)), digits=5)      #U95% CRI range whale-by-whale body density
    }          
  }           
  # when the selected model include indiv-specific CdAM
  if(ncol(fit$BUGSoutput$sims.list$CdAM)==length(whales)){ 
    for(j in 1:length(whales)){
      summary.indiv[j,7]<-signif(mean(fit$BUGSoutput$sims.list$CdAM[,j]), digits=3)   #mean whale-by-whale CdAM
      summary.indiv[j,8]<-signif(quantile(fit$BUGSoutput$sims.list$CdAM[,j], prob=0.025), digits=3)  #L95% CRI whale-by-whale CdAM
      summary.indiv[j,9]<-signif(quantile(fit$BUGSoutput$sims.list$CdAM[,j], prob=0.975), digits=3)  #U95% CRI whale-by-whale CdAM
      summary.indiv[j,10]<-signif((quantile(fit$BUGSoutput$sims.list$CdAM[,j], prob=0.975)
                            -quantile(fit$BUGSoutput$sims.list$CdAM[,j], prob=0.025)), digits=3)    #U95% CRI whale-by-whale CdAM
    }
  }
  # when the selected model include indiv-specific Vair
  if(ncol(fit$BUGSoutput$sims.list$Vair.d)==length(whales)){  
    for(j in 1:length(whales)){
      summary.indiv[j,11]<-signif(mean(fit$BUGSoutput$sims.list$Vair.d[,j]), digits=3) #mean whale-by-whale Vair
      summary.indiv[j,12]<-signif(quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.025), digits=3)  #L95% CRI whale-by-whale Vair
      summary.indiv[j,13]<-signif(quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.975), digits=3)  #U95% CRI whale-by-whale Vair
      summary.indiv[j,14]<-signif((quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.975)
                            -quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.025)), digits=3)   #U95% CRI range whale-by-whale Vair
    }
  }
write.csv(summary.indiv, file=paste(modelDir, modelName[m], fitName, "_Estimates_indiv.csv", sep=""))


### make summary table for DIVE-BY-DIVE mean
  # only when the selected model include dive-by-dive Vair
  test<-dim(fit$BUGSoutput$sims.list$Vair.d)
  numVair<-length(tapply(sp.data$dive.id, sp.data$dive.id, max))
  if(test[2]==numVair){             
    summary.d<-matrix(data=NA, nrow=numVair, ncol=8)
    colnames(summary.d)<-c("ID", "dive.number", "max.dive.depth", "dive.duration",
                               "meanVair.d", "L95Vair.d", "U95Vair.d", "95rangeVair.d")
      summary.d[,1] <- tapply(tab$whale.id, sp.data$dive.id, unique)    # whale ID
      summary.d[,2] <- tapply(tab$dive, sp.data$dive.id, max)           # dive number   
      summary.d[,3] <- tapply(tab$dive.max.depth, sp.data$dive.id, max) # max dive depth
      summary.d[,4] <- tapply(tab$dive.duration, sp.data$dive.id, max)  # dive duration
    for(j in 1:numVair){
      summary.d[j,5]<-signif(mean(fit$BUGSoutput$sims.list$Vair.d[,j]), digits=3) #mean dive-by-dive Vair
      summary.d[j,6]<-signif(quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.025), digits=3)  #L95% CRI dive-by-dive Vair
      summary.d[j,7]<-signif(quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.975), digits=3)  #U95% CRI dive-by-dive Vair
      summary.d[j,8]<-signif((quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.975)
                               -quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.025)), digits=3)   #U95% CRI range dive-by-dive Vair
    }
  write.csv(summary.d, file=paste(modelDir, modelName[m], fitName, "_Estimates_dive.csv", sep=""))    
}


  



