############ Make plots to compare 12 models###############
######### and, make a plot of model type ##########

# dataDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"
modelDir <- "D:\\Analysis\\SW sonar chapter\\statistical_analysis\\data\\body_density\\"
downstreamDir = modelDir
setwd(modelDir)

library(R2jags)
set.seed(0)

# Select fitted models

  fitName <- "_f_pitch30_depth100_rpitch999_thinned"                     # name of the fit, based on selection of "good glides"
  modNum <- 1:12                                       # model numbers 
  modelName <- c(paste("model(SW", modNum, ")", sep="")) # model names used to store model output

# Load modelled data

  # tab = filtered data frame of glides
  # filterBool = true/false vector that identifies included data from original data (all_whales.Rd)
  # fit.whales = list of included whales
  # fit.dives = list of included dives

  load(paste("data", fitName, ".Rd", sep=""))
  
  
# Initialise results table
  
# Data frame to store parameter estimates from all models
  
  rtab <- data.frame(
    model     = modelName,
    modNum    = c(1,  2,  3,  4,  5,  6,  7,  8,  9,  10,  11,  12),
    Vair.type = c("D","D","D","G","G","G","I","I","I","G","I","D"),  # variation in Vair term
    CdAM.type = c( "G","I","G","G","I","G","G","I","G","I", "I", "I"), # individual drag term
    BD.type   = c( "I","G","G","I","G","G","I","G","G","I", "I", "I")) # individual body density

# store all posterior means, lower and upper 95% CRI in a separate matrix

  body.mat <- matrix(NA, length(modelName), length(fit.whales))
  body.mat.l <- body.mat
  body.mat.u <- body.mat
  
  CdAM.mat <- body.mat
  CdAM.mat.l <- body.mat
  CdAM.mat.u <- body.mat
  
  Vair.mat <- body.mat
  Vair.mat.l <- body.mat
  Vair.mat.u <- body.mat

# Loop over models to load results
  
for(m in 1:length(modelName)) {
  
  load(paste(modelName[m], fitName, ".Rd", sep=""))
  
  # Deviance posterior mean and lower and upper 95%
  rtab[m, "deviance"] <-   fit$BUGSoutput$mean$deviance
  rtab[m, "deviance.l"] <- fit$BUGSoutput$summary["deviance",3]
  rtab[m, "deviance.u"] <- fit$BUGSoutput$summary["deviance",7]    
  
  # Deviance information criterion, estimated number of parameters and R-squared between obs and fitted
  rtab[m, "DIC"] <- fit$BUGSoutput$DIC
  rtab[m, "pD"] <- fit$BUGSoutput$pD
  rtab[m, "R"] <- sum((sp.data$a-fit$BUGSoutput$mean$a.mu)^2)
  
  # body density global posterior mean and lower and upper 95% CRI
  if (length(fit$BUGSoutput$mean$body.density.g)==1) {# when there is a global body density 
    rtab[m, "body.density.g"] <-  fit$BUGSoutput$mean$body.density.g
    rtab[m, "body.density.g.l"] <-  fit$BUGSoutput$summary["body.density.g",3]
    rtab[m, "body.density.g.u"] <-  fit$BUGSoutput$summary["body.density.g",7]}
  
  # across-individual variance in body density (global mean, L95% and U95%)
  if (length(fit$BUGSoutput$mean$body.var)==1) {# when there is variation from global body density   
    rtab[m, "body.var"] <-  fit$BUGSoutput$mean$body.var # when global
    rtab[m, "body.var.l"] <- fit$BUGSoutput$summary["body.var",3]
    rtab[m, "body.var.u"] <- fit$BUGSoutput$summary["body.var",7]}
  
  # individual estimates => store in separate matrices for mean L95% and U95%
  if (length(fit$BUGSoutput$mean$body.density)==length(fit.whales)) {# when there is a individual body density 
    rtab[m, "body.density.mean"] <- mean(fit$BUGSoutput$mean$body.density)
    body.mat[m,] <- fit$BUGSoutput$mean$body.density
    body.mat.l[m,] <- fit$BUGSoutput$summary[paste("body.density[",1:length(fit.whales),"]",sep=""),3]
    body.mat.u[m,] <- fit$BUGSoutput$summary[paste("body.density[",1:length(fit.whales),"]",sep=""),7]
  }    
  
  
  # CdAM global posterior mean and lower and upper 95% CRI
  if (length(fit$BUGSoutput$mean$CdAM.g)==1) {# when there is a global CdAM
    rtab[m, "CdAM.g"] <-  fit$BUGSoutput$mean$CdAM.g
    rtab[m, "CdAM.g.l"] <-  fit$BUGSoutput$summary["CdAM.g",3]     
    rtab[m, "CdAM.g.u"] <-  fit$BUGSoutput$summary["CdAM.g",7]}
  
  # across-individual variance in CdAM (global mean, L95% and U95%)
  if (length(fit$BUGSoutput$mean$CdAM.var)==1) {# when there is variation from CdAM
    rtab[m, "CdAM.var"] <-  fit$BUGSoutput$mean$CdAM.var # when global
    rtab[m, "CdAM.var.l"] <- fit$BUGSoutput$summary["CdAM.var",3]
    rtab[m, "CdAM.var.u"] <- fit$BUGSoutput$summary["CdAM.var",7]}
  
  # individual estimates => store in separate matrices for mean L95% and U95%
  if (length(fit$BUGSoutput$mean$CdAM)==length(fit.whales)) {# when there is a individual CdAM
    rtab[m, "CdAM.mean"] <- mean(fit$BUGSoutput$mean$CdAM)
    CdAM.mat[m,] <- fit$BUGSoutput$mean$CdAM
    CdAM.mat.l[m,] <- fit$BUGSoutput$summary[paste("CdAM[",1:length(fit.whales),"]",sep=""),3]
    CdAM.mat.u[m,] <- fit$BUGSoutput$summary[paste("CdAM[",1:length(fit.whales),"]",sep=""),7]
  }    
  
  # Vair global posterior mean and lower and upper 95% CRI
  if (length(fit$BUGSoutput$mean$Vair)==1) {# 
    rtab[m, "Vair.g"] <-  fit$BUGSoutput$mean$Vair # when global
    rtab[m, "Vair.g.l"] <- fit$BUGSoutput$summary["Vair",3]
    rtab[m, "Vair.g.u"] <- fit$BUGSoutput$summary["Vair",7]}
  
  # across-individual variance in Vair (global mean, L95% and U95%)
  if (length(fit$BUGSoutput$mean$Vair.var)==1) {#    
    rtab[m, "Vair.var"] <- fit$BUGSoutput$mean$Vair.var # when variation from global
    rtab[m, "Vair.var.l"] <- fit$BUGSoutput$summary["Vair.var",3]
    rtab[m, "Vair.var.u"] <- fit$BUGSoutput$summary["Vair.var",7]}
  
  # calculate average over dive-by-dive estimates of Vair
  if(length(fit$BUGSoutput$mean$Vair.d)>length(fit.whales)) {
    rtab[m, "Vair.d.mean"] <- mean(fit$BUGSoutput$mean$Vair.d)} # average of dive-by-dive Vair
  
  # individual estimates => store in separate matrices for mean L95% and U95%
  if(length(fit$BUGSoutput$mean$Vair.d)==length(fit.whales)) {
    rtab[m, "Vair.w.mean"] <- mean(fit$BUGSoutput$mean$Vair.d)
    Vair.mat[m,] <- fit$BUGSoutput$mean$Vair.d
    Vair.mat.l[m,] <- fit$BUGSoutput$summary[paste("Vair.d[",1:length(fit.whales),"]",sep=""),3]
    Vair.mat.u[m,] <- fit$BUGSoutput$summary[paste("Vair.d[",1:length(fit.whales),"]",sep=""),7]
  } # average of individual Vair
  
  # Compressibility global posterior mean and lower and upper 95% CRI
  rtab[m, "compr"] <- fit$BUGSoutput$mean$compr
  rtab[m, "compr.l"] <- fit$BUGSoutput$summary["compr",3]
  rtab[m, "compr.u"] <- fit$BUGSoutput$summary["compr",7]
  
}

  
  
# Saving results
  
# write.csv(rtab, file=paste("all_model_estimates.csv", sep=""))

# save(rtab, 
#     body.mat, body.mat.l, body.mat.u,
#     Vair.mat, Vair.mat.l, Vair.mat.u,
#     CdAM.mat, CdAM.mat.l, CdAM.mat.u,
#     file=paste("all_model_estimates.Rd", sep=""))


##### Make plots to compare 12 models
  
pdf(paste(downstreamDir,"compare_models.pdf",sep=""), width=4.5,height=6.5)        
options(graphics.record=TRUE)


modNames<-rtab$model

par(mfrow=c(1,1), mar=c(5, 5, 2, 2))

  ### deviance
  ord <- order(rtab$deviance)
  plot(rtab$deviance[ord], 1:length(modNames), yaxt="n", xlab="deviance", ylab="", pch=16,
       xlim=c(min(rtab$deviance.l), max(rtab$deviance.u)), cex=0.8)
  segments(x0=rtab$deviance.l[ord], x1=rtab$deviance.u[ord], 
           y0=1:length(modNames), y1=1:length(modNames))
  axis(2, at=1:length(modNames), modNames[ord], las=2)
  grid(lwd=1.5, col="gray")
  
  
  ### DIC
  ord <- order(rtab$DIC)
  plot(rtab$DIC[ord], 1:length(modNames), yaxt="n", xlab="DIC", ylab="", pch=16, main="DIC")
  axis(2, at=1:length(modNames), modNames[ord], las=2)
  grid(lwd=1.5, col="gray")
  
  
  ## body density in each model by individual
  ord <- order(rtab$DIC)
  minBD <- min(body.mat, na.rm=T)
  maxBD <- max(body.mat, na.rm=T)
  plot(body.mat[ord,1], 1:length(modNames), col=NA,
       yaxt="n", xlab="body density", ylab="", xlim=c(minBD, maxBD), 
       ylim=c(-1, length(modNames)))
  grid(lwd=1.5, col="gray")
  for(w in 1:length(fit.whales)) {
    tempI <- !is.na(body.mat[ord,w])
    lines(body.mat[ord[tempI],w], c(1:length(modNames))[tempI], col="blue")           
    abline(h=c(1:length(modNames))[!tempI], col="white", lwd=15)
    text(body.mat[ord[1],w], -0.3, fit.whales[w], srt=90, cex=0.75)
  }
  points(rtab$body.density.g[ord], 1:length(modNames), col="orange", lwd=2)
  axis(2, at=1:length(modNames), modNames[ord], las=2)
  title ("orange-global mean, blue-individual")
  
  ## CdAM
  ord <- order(rtab$DIC)
  minBD <- min(CdAM.mat, na.rm=T)
  maxBD <- max(CdAM.mat, na.rm=T)
  plot(CdAM.mat[ord,1], 1:length(modNames), col=NA,
       yaxt="n", xlab="CdAM", ylab="", xlim=c(minBD, maxBD), ylim=c(-1, length(modNames)))
  grid(lwd=1.5, col="gray")
  for(w in 1:length(fit.whales)) {
    tempI <- !is.na(CdAM.mat[ord,w])
    lines(CdAM.mat[ord[tempI],w], c(1:length(modNames))[tempI], col="blue")          
    abline(h=c(1:length(modNames))[!tempI], col="white", lwd=15)
    text(CdAM.mat[ord[1],w], -0.3, fit.whales[w], srt=90, cex=0.75)
  }
  points(rtab$CdAM.g[ord], 1:length(modNames), col="orange", pch=1, lwd=2)
  axis(2, at=1:length(modNames), modNames[ord], las=2)
  title ("orange-global mean, blue-individual")
  
  
  ## Vair
  ord <- order(rtab$DIC)
  minBD <- min(Vair.mat, na.rm=T)
  maxBD <- max(Vair.mat, na.rm=T)
  plot(Vair.mat[ord,1], 1:length(modNames), col=NA,
       yaxt="n", xlab="Vair", ylab="", xlim=c(minBD, maxBD), ylim=c(-1, length(modNames)))
  grid(lwd=1.5, col="gray")
  for(w in 1:length(fit.whales)) {
    tempI <- !is.na(Vair.mat[ord,w])
    lines(Vair.mat[ord[tempI],w], c(1:length(modNames))[tempI], col="blue")          #col=(4+c(w<14)*5))
    abline(h=c(1:length(modNames))[!tempI], col="white", lwd=15)
    text(Vair.mat[ord[1],w], -0.3, fit.whales[w], srt=90, cex=0.75)
  }
  points(rtab$Vair.g[ord], 1:length(modNames), col="orange", pch=1, lwd=2)
  axis(2, at=1:length(modNames), modNames[ord], las=2, cex=1)
  title ("orange-global mean, blue-individual")
  
  # compressibility
  ord <- order(rtab$DIC)
  minBD <- min(rtab$compr, na.rm=T)
  maxBD <- max(rtab$compr, na.rm=T)
  plot(rtab$compr[ord], 1:length(modNames), col=NA,
       yaxt="n", xlab="Compressibility", ylab="", xlim=c(minBD-0.1, maxBD+0.1))
  grid(lwd=1.5, col="gray")
  points(rtab$compr[ord], 1:length(modNames), col="orange", pch=1, lwd=2) 
  axis(2, at=1:length(modNames), modNames[ord], las=2, cex=1)
  title ("orange-global mean")


options(graphics.record=FALSE)      
dev.off() 



########### Make a plot of model type ###################

pdf(paste(downstreamDir,"model_parameters.pdf",sep=""), width=4.5,height=6.5)
options(graphics.record=TRUE)

par(mfrow=c(1,1), mar=c(5, 5, 2, 2))

ord<- order(rtab$DIC)
xx<-c("BD", "CdAM", "Vair")
xBD<-rep(1, length(rtab$DIC))
xCd<-rep(2, length(rtab$DIC))
xVair<-rep(3, length(rtab$DIC))
leg<-c("global only", "global+indiv", "global+dive")

rtab$BD.pch <- 1*(rtab$BD.type=="G")+15*(rtab$BD.type=="I")
rtab$CdAM.pch <- 1*(rtab$CdAM.type=="G")+15*(rtab$CdAM.type=="I")
rtab$Vair.pch <- 1*(rtab$Vair.type=="G")+15*(rtab$Vair.type=="I")+17*(rtab$Vair.type=="D")

plot(xBD, 1:length(rtab$DIC), pch=rtab$BD.pch[ord], 
     xlab="", ylab="", xlim=c(0.5, 4.7), yaxt="n", xaxt="n", main="parameters")
grid(lwd=1.5, col="gray")
points(xCd, 1:length(rtab$DIC), pch=rtab$CdAM.pch[ord])
points(xVair, 1:length(rtab$DIC), pch=rtab$Vair.pch[ord])
axis(2, at=1:length(rtab$DIC), rtab$model[ord], las=2)
axis(1, at=1:3, xx, las=2)
legend(3.3, length(rtab$DIC), leg[1:3], pch=c(1, 15, 17), cex=0.8)


options(graphics.record=FALSE)      
dev.off() 