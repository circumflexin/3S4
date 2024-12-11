### plot results of the selected model
modelDir <- "D:\\Analysis\\SW sonar chapter\\statistical_analysis\\data\\body_density\\"
downstreamDir = modelDir

setwd(modelDir)

library(lattice)
library(R2jags)
set.seed(1990)

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


############################### COLOURS

whaleCol = seq(1:length(fit.whales))

rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))

pitchBreaks <- seq(-90, 90, 10)
mycols <- rbPal(length(pitchBreaks))[as.numeric(cut(tab$mean.pitch,breaks = pitchBreaks))]

#############################

pdf(paste(downstreamDir,"plot_best_", modelName, ".pdf", sep=""), width=6,height=5)
options(graphics.record=TRUE)

minBD <- min(fit$BUGSoutput$sims.list$body.density)
maxBD <- max(fit$BUGSoutput$sims.list$body.density)

## body density

if(length(fit$BUGSoutput$mean$body.density)==length(fit.whales)){
  
  tempI <-  which.min(fit$BUGSoutput$sd$body.density) # set y-axis maximum to most narrow posterior density
  test<-density(fit$BUGSoutput$sims.list$body.density[,tempI])
  plot(1,1,xlim=c(minBD, maxBD+(maxBD-minBD)/3),
       ylim=c(0,max(test$y)), col=NA,
       xlab="Body density (kg/m^3)", ylab="Posterior probability")
  grid()
  for(w in 1:length(fit.whales)) {
    lines(density(fit$BUGSoutput$sims.list$body.density[,w]),col=whaleCol[w])
  }
  numG<-c(1)
  for(w in 1:length(fit.whales)) {
    filterW<-tab$whale.id==fit.whales[w]
    numG[w]<-sum(filterW)
  }
  legend(x=maxBD, y=max(test$y), 
         legend=paste(fit.whales[1:length(fit.whales)], " [", numG[1:length(numG)], "]", sep=""),
         lty=1, col=whaleCol, cex=0.5)
  
  if(length(fit$BUGSoutput$sims.list$body.density.g)>0){
    lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000050"), lwd=7)
    legend(x=maxBD, y=max(test$y)*0.1, "global mean", lty=1, lwd=7, col=c("#00000050"), cex=0.6)
  }
  
} else {
  minBD <- min(fit$BUGSoutput$sims.list$body.density.g)
  maxBD <- max(fit$BUGSoutput$sims.list$body.density.g)
  test<-density(fit$BUGSoutput$sims.list$body.density.g)
  plot(1,1,xlim=c(minBD,maxBD+(maxBD-minBD)/3), ylim=c(0,max(test$y)), col=NA,
       xlab="body.density.g", ylab="posterior density",
       main=paste("mean global body.density.g = ",
                  signif(mean(fit$BUGSoutput$sims.list$body.density.g), digits=3), " (sd = ",
                  signif(sd(fit$BUGSoutput$sims.list$body.density.g), digits=3), ")", sep=""))
  lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000040"), lwd=5)
  legend(x=maxBD, y=max(test$y), "body.density.g", lty=1, col=c("#00000040"), lwd=5, cex=0.7)
}


# Vair.d (for indiv-specific)

if(length(fit$BUGSoutput$mean$Vair.d)==length(fit.whales)){
  minVair <- min(fit$BUGSoutput$sims.list$Vair.d)
  maxVair <- max(fit$BUGSoutput$sims.list$Vair.d)
  tempI <-  which.min(fit$BUGSoutput$sd$Vair.d)
  test<-density(fit$BUGSoutput$sims.list$Vair.d[,tempI])
  plot(1,1,xlim=c(minVair, maxVair+(maxVair-minVair)/3), ylim=c(0,max(test$y)*2), col=NA,
       xlab="Vair", ylab="posterior density")
  grid()
  for(w in 1:length(fit.whales)) {
    lines(density(fit$BUGSoutput$sims.list$Vair.d[,w]), col=whaleCol[w])
  }
  lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#80808030"), lwd=3)
  legend(x=maxVair, y=max(test$y)*2, legend=fit.whales, lty=1, col=w, cex=0.7)
  legend(x=maxVair, y=max(test$y)*0.8, "global", lty=1, col=c("#80808030"), lwd=3, cex=0.7)
}

# Vair.d (for dive.specific)

if(length(fit$BUGSoutput$mean$Vair.d)==length(unique(sp.data$dive.id))){
  minVair <- min(fit$BUGSoutput$sims.list$Vair.d)
  maxVair <- max(fit$BUGSoutput$sims.list$Vair.d)
  tempI <-  which.min(fit$BUGSoutput$sd$Vair.d) 
  test<-density(fit$BUGSoutput$sims.list$Vair.d[,tempI])
  plot(1,1,xlim=c(minVair, maxVair), ylim=c(0,max(test$y)), col=NA,
       xlab="Vair", ylab="posterior density")
  grid()
  for(w in 1:length(unique(sp.data$dive.id))) {
    lines(density(fit$BUGSoutput$sims.list$Vair.d[,w]), col="black")
  }
  lines(density(fit$BUGSoutput$sims.list$Vair), col=c("grey"), lwd=3)
  legend(x=maxVair*0.8, y=max(test$y)*0.8, "global", lty=1, col=c("grey"), lwd=3, cex=0.7)
}

# Vair global

minVair <- min(fit$BUGSoutput$sims.list$Vair)
maxVair <- max(fit$BUGSoutput$sims.list$Vair)
test<-density(fit$BUGSoutput$sims.list$Vair)
plot(1,1,xlim=c(minVair,maxVair+(maxVair-minVair)/3), ylim=c(0,max(test$y)), col=NA,
     xlab="Vair", ylab="posterior density",
     main=paste("mean global Vair = ",
                signif(mean(fit$BUGSoutput$sims.list$Vair), digits=3), " (sd = ",
                signif(sd(fit$BUGSoutput$sims.list$Vair), digits=3), ")", sep=""))
lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#00000040"), lwd=5)
legend(x=maxVair, y=max(test$y), "global Vair", lty=1, col=c("#00000040"), lwd=5, cex=0.7)


# CdAM

if(length(fit$BUGSoutput$mean$CdAM)==length(fit.whales)){
  minCdAM <- min(fit$BUGSoutput$sims.list$CdAM)
  maxCdAM <- max(fit$BUGSoutput$sims.list$CdAM)
  tempI <-  which.min(fit$BUGSoutput$sd$CdAM)
  test<-density(fit$BUGSoutput$sims.list$CdAM[,tempI])
  plot(1,1,xlim=c(minCdAM, maxCdAM+(maxCdAM-minCdAM)/3), ylim=c(0,max(test$y)), col=NA,
       xlab="CdAM", ylab="posterior density")
  grid()
  for(w in 1:length(fit.whales)) {
    lines(density(fit$BUGSoutput$sims.list$CdAM[,w]),
          col=whaleCol[w])
  }
  legend(x=maxCdAM, y=max(test$y), legend=fit.whales, lty= 1, col=whaleCol, cex=0.5)
  if(length(fit$BUGSoutput$sims.list$CdAM.g)>0){
    lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
    legend(x=maxCdAM, y=max(test$y)*0.1, "global mean", lty=1, lwd=5, col=c("#00000040"), cex=0.5)
  }
} else {
  minCdAM <- min(fit$BUGSoutput$sims.list$CdAM.g)
  maxCdAM <- max(fit$BUGSoutput$sims.list$CdAM.g)
  test<-density(fit$BUGSoutput$sims.list$CdAM.g)
  plot(1,1,xlim=c(minCdAM,maxCdAM+(maxCdAM-minCdAM)/3), ylim=c(0,max(test$y)), col=NA,
       xlab="CdAM.g", ylab="posterior density",
       main=paste("mean global CdAM.g = ",
                  signif(mean(fit$BUGSoutput$sims.list$CdAM.g), digits=3), " (sd = ",
                  signif(sd(fit$BUGSoutput$sims.list$CdAM.g), digits=3), ")", sep=""))
  lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
  legend(x=maxCdAM, y=max(test$y), "CdAM.g", lty=1, col=c("#00000040"), lwd=5, cex=0.7)
}

# compressibility

if(length(fit$BUGSoutput$mean$compr)>0) {
  minCompr <- min(fit$BUGSoutput$sims.list$compr)
  maxCompr <- max(fit$BUGSoutput$sims.list$compr)
  plot(density(fit$BUGSoutput$sims.list$compr), col="lightgrey", lwd=5,
       xlab="Compressibility", ylab="posterior density", 
       xlim=c(minCompr, maxCompr), # <<<< ADJUST X-AXIS RANGE HERE
       main=paste("mean compr = ",
                  signif(mean(fit$BUGSoutput$sims.list$compr), digits=3), " (sd = ",
                  signif(sd(fit$BUGSoutput$sims.list$compr), digits=3), ")", sep=""))
}

# body density.g with uniform prior

if(length(fit$BUGSoutput$mean$body.density.g)>0){
  test<-density(fit$BUGSoutput$sims.list$body.density.g)
  plot(1,1,
       xlim=c(800, 1400), # <<<< ADJUST X-AXIS RANGE HERE
       ylim=c(0,max(test$y)), col=NA,
       xlab="body density", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$body.density.g, digit=5), sep=""))
  lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000040"), lwd=3)
  legend(x=1200, y=max(test$y)*1, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=1200, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  
  # PRIOR - INSERT CORRECT VALUES HERE
  segments(800, 0, 1400, 0, col=c("#00008B50"), lwd=10)
}


# CdAM.g with uniform prior

if(length(fit$BUGSoutput$mean$CdAM.g)>0){
  test<-density(fit$BUGSoutput$sims.list$CdAM.g)
  plot(1,1,
       xlim=c(0, 30), # <<<< ADJUST X-AXIS RANGE HERE
       ylim=c(0,max(test$y)*1.2), col=NA,
       xlab="CdAM (E-06)", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$CdAM.g, digit=3), sep=""))
  lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
  legend(x=30, y=max(test$y)*1.2, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=30, y=max(test$y)*0.2, "prior distribution", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  
  # PRIOR - INSERT CORRECT VALUES HERE
  segments(1e-6,0,30,0, col=c("#00008B50"), lwd=10)
}


# gloal Vair with uniform prior

if(length(fit$BUGSoutput$mean$Vair)>0){
  test<-density(fit$BUGSoutput$sims.list$Vair)
  plot(1,1,
       xlim=c(2, 50), # <<<< ADJUST X-AXIS RANGE HERE
       ylim=c(0,max(test$y)*1.5), col=NA,
       xlab="Vair", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$Vair, digit=3), sep=""))
  lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#00000040"), lwd=5)
  legend(x=35, y=max(test$y)*1, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=35, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  
  # PRIOR - INSERT CORRECT VALUES HERE
  segments(5, 0, 50, 0, col=c("#00008B50"), lwd=10)
}


# compr with uniform prior

if(length(fit$BUGSoutput$mean$compr)>0){
  test<-density(fit$BUGSoutput$sims.list$compr)
  plot(density(fit$BUGSoutput$sims.list$compr), col=c("#00000040"), lwd=5,
       xlim=c(0, 0.8), # <<<< ADJUST X-AXIS RANGE HERE
       ylim=c(0, max(test$y)), 
       xlab="Compressibility", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$compr, digit=3), sep=""))
  legend(x=0.6, y=max(test$y)*1, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=0.6, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  
  # PRIOR - INSERT CORRECT VALUES HERE
  segments(0.1, 0, 0.7, 0, col=c("#00008B50"), lwd=10)
}


par(mfrow=c(1,1))
# measured tau
tempw <- sp.data$whale.id[which.min(tapply(sp.data$tau, sp.data$whale.id, sd))]
test<-density(sp.data$tau[sp.data$whale.id==tempw])
plot(1,1,xlim=c(min(sp.data$tau),max(sp.data$tau)), ylim=c(0,max(test$y)), col=NA,
     xlab="TAU used in the model", ylab="density")
grid()
for(w in 1:length(fit.whales)) {  
  wBool <- sp.data$whale.id==w  
  lines(density(sp.data$tau[wBool]), col=whaleCol[w], lty=1)
}
legend(max(sp.data$tau)*0.7, max(test$y)*0.7, 
       legend=fit.whales, lty=1, col=whaleCol, cex=0.5)



#plot(sp.data$a, fit$BUGSoutput$mean$a.mu, ylab="", xlab="", col=whaleCol[sp.data$whale.id], cex=0.8)
#mtext("observed acceleration", side=1, line=3, cex=1.2)
#mtext("posterior mean", side=2, line=3, cex=1.2)
#abline(0,1)
#grid()

#plot(sp.data$mean.speed, sp.data$a,
#     ylab="", xlab="", 
#     main="")
#points(sp.data$mean.speed, fit$BUGSoutput$mean$a.mu, col=whaleCol[sp.data$whale.id], cex=0.8)
#grid()
#mtext("speed (m/s)", side=1, line=3, cex=1.2)
#mtext("acceleration", side=2, line=3, cex=1.2)
#mtext("observed and fitted (in colour)", side=3, line=1, cex=1.2, font=2)




## observed acceleration vs glide depth
# colour coded by speed
rbPal2<-colorRampPalette(c("darkblue", "blue", "deepskyblue", "cyan", "lawngreen",
                           "yellow", "orange", "darkred"))

speedBreaks<-seq(0, 4, 0.01)      # speed is color coded by 0.01
spBreakLegend<-seq(0, 4, 0.5)     # data to make legend
SPcols <- rbPal2(length(speedBreaks))[as.numeric(cut(sp.data$mean.speed,breaks = speedBreaks))]

plot(sp.data$depth, sp.data$a, ylab="", xlab="",main="", col=SPcols, cex=0.5, pch=1)
grid()
mtext("depth (m)", side=1, line=3, cex=1.2)
mtext("obs. acceleration", side=2, line=3, cex=1.2)
mtext("obs. acceleration vs depth (coloured by speed)", side=3, line=1, cex=1.2, font=2)
legend(max(sp.data$depth)*0.9,max(sp.data$a), legend=spBreakLegend[1:length(spBreakLegend)], 
       fill=rbPal2(length(spBreakLegend))[1:length(spBreakLegend)][1:length(spBreakLegend)], cex=0.6)


#plot(sp.data$depth, sp.data$a, ylab="", xlab="",main="", col=pitchCols1, cex=0.8, pch=1)
#plot(sp.data$depth, sp.data$a, ylab="", xlab="",main="", col="grey", cex=0.5, pch=1)
#grid()
#points(sp.data$depth, fit$BUGSoutput$mean$a.mu, col=SPcols, pch=3, cex=0.5)
#mtext("depth (m)", side=1, line=3, cex=1.2)
#mtext("acceleration", side=2, line=3, cex=1.2)
#mtext("obs + fitted acceleration vs depth", side=3, line=1, cex=1.2, font=2)
#legend(max(sp.data$depth)*0.9,max(sp.data$a), legend=spBreakLegend[1:length(spBreakLegend)], 
#       fill=rbPal2(length(spBreakLegend))[1:length(spBreakLegend)][1:length(spBreakLegend)], cex=0.6)



## observed data alon, color coded by pitch
#rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
#                          "yellow", "orange", "orangered", "darkred"))
#pitchBreaks <- seq(-90, 90, 10)
#sp.data$mean.pitch<-asin(sp.data$sin.pitch)*180/pi      # add mean.pitch to sp.data
#mycols <- rbPal(length(pitchBreaks))[as.numeric(cut(sp.data$mean.pitch,breaks = pitchBreaks))]

#plot(sp.data$mean.speed, sp.data$a, ylab="", xlab="",main="", col=mycols, cex=0.5, pch=1, 
#     xlim=c(0, max(sp.data$mean.speed)*1.2))
#grid()
#mtext("speed (m/s)", side=1, line=3, cex=1.2)
#mtext("obs. acceleration", side=2, line=3, cex=1.2)
#mtext("obs a vs speed", side=3, line=1, cex=1.2, font=2)
#legend(max(sp.data$mean.speed)*1.1,max(sp.data$a), legend=pitchBreaks[-(8:12)], 
#       fill=rbPal(length(pitchBreaks))[(1:20)[-(8:12)]], cex=0.6)


rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))
pitchBreaks <- seq(-90, 90, 10)
sp.data$mean.pitch<-asin(sp.data$sin.pitch)*180/pi      # add mean.pitch to sp.data
mycols <- rbPal(length(pitchBreaks))[as.numeric(cut(sp.data$mean.pitch,breaks = pitchBreaks))]

## observed data & estimated a
plot(sp.data$mean.speed, sp.data$a, ylab="", xlab="",main="", 
     col="grey", cex=0.5, pch=1, xlim=c(0, max(sp.data$mean.speed)*1.2))
points(sp.data$mean.speed, fit$BUGSoutput$mean$a.mu, col=mycols, pch=3, cex=0.5)
grid()
mtext("speed (m/s)", side=1, line=3, cex=1.2)
mtext("acceleration", side=2, line=3, cex=1.2)
mtext("obs (grey) + fitted (colored by pitch) acc. vs speed", side=3, line=1, cex=1.2, font=2)
legend(max(sp.data$mean.speed)*1.1,max(sp.data$a), legend=pitchBreaks[-(8:12)], 
       fill=rbPal(length(pitchBreaks))[(1:20)[-(8:12)]], cex=0.6)

#for(w in 1:length(fit.whales)) {

#  wBool <- sp.data$whale.id==w

## observed data alon
#  plot(sp.data$mean.speed[wBool], sp.data$a[wBool], ylab="", xlab="",main="", 
#       col="grey", cex=0.5, pch=1)
#  points(sp.data$mean.speed[wBool], fit$BUGSoutput$mean$a.mu[wBool], col=mycols, pch=3, cex=0.5)
#  grid()
#  mtext("speed (m/s)", side=1, line=3, cex=1.2)
#  mtext("acceleration", side=2, line=3, cex=1.2)
#  mtext(fit.whales[w], side=3, line=1, cex=1.2, font=2)

#}



# plot observed A and estimated A with speed
rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))
pitchBreaks<-seq(-90, 90, 10)

sp.data$mean.pitch<-asin(sp.data$sin.pitch)*180/pi      # add mean.pitch to sp.data

for(w in 1:length(fit.whales)) {
  
  wBool <- sp.data$whale.id==w
  
  mycols2<-rbPal(length(pitchBreaks))[as.numeric(cut(sp.data$mean.pitch[wBool],breaks = pitchBreaks))]   # new mycolor for filtered data (for each whale)
  
  ## observed data alon
  plot(sp.data$mean.speed[wBool], sp.data$a[wBool], ylab="", xlab="",main="", 
       col="darkgrey", cex=0.8, pch=19, xlim=c(min(sp.data$mean.speed), max(sp.data$mean.speed)+1), 
       ylim=c(min(sp.data$a), max(sp.data$a)))
  points(sp.data$mean.speed[wBool], fit$BUGSoutput$mean$a.mu[wBool], col="black", bg=mycols2, pch=21, cex=0.8)
  grid()
  mtext("speed (m/s)", side=1, line=3, cex=1.2)
  mtext("acceleration", side=2, line=3, cex=1.2)
  mtext(paste(fit.whales[w], " [obs(grey), fitted(coloured by pitch)]", sep=""), side=3, line=1, cex=1.2, font=2)
  legend(max(sp.data$mean.speed),max(sp.data$a), legend=pitchBreaks[-(8:12)], 
         fill=rbPal(length(pitchBreaks))[(1:20)[-(8:12)]], cex=0.7)
}


options(graphics.record=FALSE)      
dev.off() 



