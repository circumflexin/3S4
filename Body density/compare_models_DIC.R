############ Make plots to compare 12 models###############
######### and, make a plot of model type ##########

setwd("/Users/georgesato/R/BD_estimation/oo23/")
dataDir<-"/Users/georgesato/R/BD_estimation/oo23/oo_glides/"
modelDir<-"/Users/georgesato/R/BD_estimation/oo23/oo_models/"

library(R2jags)
set.seed(0)

# load & filter data -> fitName
load("all_whales.Rd")
fitName <- "_f_pitch30_depth100"
filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>100 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
  is.na(tab$mean.speed)==F & tab$r.roll>0.9  & is.na(tab$DswGG)==F
#& tab$mean.depth>200 & tab$sonar==0 & tab$vertglide==1
tab <- tab[filterBool,]      
tab <- tab[rowSums(is.na(tab)) != ncol(tab), ]

whales <- unique(tab$whale.id)

modNum<-c(1:12)                       

modelName <- c(paste("model(oo", modNum[1:length(modNum)], ")", sep=""))

rtab <- data.frame(model=modelName)

rtab$deviance<-NA 
rtab$deviance.l<-NA 
rtab$deviance.u<-NA                 
rtab$DIC <- NA
rtab$pD <- NA
rtab$R <- NA

rtab$body.density.mean <-NA # average of individual body densities
rtab$body.density.g <-NA # when global
rtab$body.density.g.l <-NA
rtab$body.density.g.u <-NA 

rtab$body.var <-NA # when global
rtab$body.var.l <-NA
rtab$body.var.u <-NA  

rtab$CdAM.mean<-NA # average of individual CdAM
rtab$CdAM.g<-NA# when global
rtab$CdAM.g.l<-NA
rtab$CdAM.g.u<-NA 

rtab$CdAM.var<-NA # when global             
rtab$CdAM.var.l<-NA
rtab$CdAM.var.u<-NA 

rtab$Vair.d.mean<-NA # average of dive-by-dive Vair
rtab$Vair.w.mean<-NA # average of individual Vair
rtab$Vair.g<-NA # when global
rtab$Vair.g.l<-NA
rtab$Vair.g.u<-NA 

rtab$Vair.var<-NA # when global
rtab$Vair.var.l<-NA
rtab$Vair.var.u<-NA 

rtab$compr<-NA
rtab$compr.l<-NA
rtab$compr.u<-NA


# store all posterior means, lower and upper

body.mat <- matrix(NA, length(modelName), length(whales))
body.mat.l <- body.mat
body.mat.u <- body.mat

CdAM.mat <- body.mat
CdAM.mat.l <- body.mat
CdAM.mat.u <- body.mat

Vair.mat <- body.mat
Vair.mat.l <- body.mat
Vair.mat.u <- body.mat


for(m in 1:length(modelName)) {
  
  load(paste(modelName[m], fitName, ".Rd", sep=""))
  
  rtab$deviance[m] <-   fit$BUGSoutput$mean$deviance
  rtab$deviance.l[m] <- fit$BUGSoutput$summary["deviance",3]
  rtab$deviance.u[m] <- fit$BUGSoutput$summary["deviance",7]                
  rtab$DIC[m] <- fit$BUGSoutput$DIC
  rtab$pD[m] <- fit$BUGSoutput$pD
  rtab$R[m] <- sum((sp.data$a-fit$BUGSoutput$mean$a.mu)^2)
  
  # body density
  if (length(fit$BUGSoutput$mean$body.density.g)==1) {# when there is a global body density 
    rtab$body.density.g[m] <-  fit$BUGSoutput$mean$body.density.g
    rtab$body.density.g.l[m] <-  fit$BUGSoutput$summary["body.density.g",3]
    rtab$body.density.g.u[m] <-  fit$BUGSoutput$summary["body.density.g",7]}
  
  if (length(fit$BUGSoutput$mean$body.var)==1) {# when there is variation from global body density   
    rtab$body.var[m] <-  fit$BUGSoutput$mean$body.var # when global
    rtab$body.var.l[m] <- fit$BUGSoutput$summary["body.var",3]
    rtab$body.var.u[m] <- fit$BUGSoutput$summary["body.var",7]}
  
  if (length(fit$BUGSoutput$mean$body.density)==length(whales)) {# when there is a individual body density 
    rtab$body.density.mean[m] <- mean(fit$BUGSoutput$mean$body.density)
    body.mat[m,] <- fit$BUGSoutput$mean$body.density
    body.mat.l[m,] <- fit$BUGSoutput$summary[paste("body.density[",1:length(whales),"]",sep=""),3] # Does not work with only 1 individual
    body.mat.u[m,] <- fit$BUGSoutput$summary[paste("body.density[",1:length(whales),"]",sep=""),7] # Does not work with only 1 individual
  }    
  
  
  # CdAM
  if (length(fit$BUGSoutput$mean$CdAM.g)==1) {# when there is a global CdAM
    rtab$CdAM.g[m] <-  fit$BUGSoutput$mean$CdAM.g
    rtab$CdAM.g.l[m] <-  fit$BUGSoutput$summary["CdAM.g",3]     
    rtab$CdAM.g.u[m] <-  fit$BUGSoutput$summary["CdAM.g",7]}
  
  if (length(fit$BUGSoutput$mean$CdAM.var)==1) {# when there is variation from CdAM
    rtab$CdAM.var[m] <-  fit$BUGSoutput$mean$CdAM.var # when global
    rtab$CdAM.var.l[m] <- fit$BUGSoutput$summary["CdAM.var",3]
    rtab$CdAM.var.u[m] <- fit$BUGSoutput$summary["CdAM.var",7]}
  
  if (length(fit$BUGSoutput$mean$CdAM)==length(whales)) {# when there is a individual CdAM
    rtab$CdAM.mean[m] <- mean(fit$BUGSoutput$mean$CdAM)
    CdAM.mat[m,] <- fit$BUGSoutput$mean$CdAM
    CdAM.mat.l[m,] <- fit$BUGSoutput$summary[paste("CdAM[",1:length(whales),"]",sep=""),3] # Does not work with only 1 individual
    CdAM.mat.u[m,] <- fit$BUGSoutput$summary[paste("CdAM[",1:length(whales),"]",sep=""),7] # Does not work with only 1 individual
  }    
  
  # Vair
  if (length(fit$BUGSoutput$mean$Vair)==1) {# 
    rtab$Vair.g[m] <-  fit$BUGSoutput$mean$Vair # when global
    rtab$Vair.g.l[m] <- fit$BUGSoutput$summary["Vair",3]
    rtab$Vair.g.u[m] <- fit$BUGSoutput$summary["Vair",7]}
  
  if (length(fit$BUGSoutput$mean$Vair.var)==1) {#    
    rtab$Vair.var[m] <- fit$BUGSoutput$mean$Vair.var # when variation from global
    rtab$Vair.var.l[m] <- fit$BUGSoutput$summary["Vair.var",3]
    rtab$Vair.var.u[m] <- fit$BUGSoutput$summary["Vair.var",7]}
  
  if(length(fit$BUGSoutput$mean$Vair.d)>length(whales)) {
    rtab$Vair.d.mean[m] <- mean(fit$BUGSoutput$mean$Vair.d)} # average of dive-by-dive Vair
  if(length(fit$BUGSoutput$mean$Vair.d)==length(whales)) {
    rtab$Vair.w.mean[m] <- mean(fit$BUGSoutput$mean$Vair.d)
    Vair.mat[m,] <- fit$BUGSoutput$mean$Vair.d
    Vair.mat.l[m,] <- fit$BUGSoutput$summary[paste("Vair.d[",1:length(whales),"]",sep=""),3] # Does not work with only 1 individual
    Vair.mat.u[m,] <- fit$BUGSoutput$summary[paste("Vair.d[",1:length(whales),"]",sep=""),7] # Does not work with only 1 individual
  } # average of individual Vair
  
  rtab$compr[m] <- fit$BUGSoutput$mean$compr
  rtab$compr.l[m] <- fit$BUGSoutput$summary["compr",3]
  rtab$compr.u[m] <- fit$BUGSoutput$summary["compr",3]
  
}

write.csv(rtab, file=paste("all_model_estimates.csv", sep=""))

save(rtab, 
     body.mat, body.mat.l, body.mat.u,
     Vair.mat, Vair.mat.l, Vair.mat.u,
     CdAM.mat, CdAM.mat.l, CdAM.mat.u,
     file=paste("all_model_estimates.Rd", sep=""))


##### Make plots to compare 12 models
pdf("compare_models.pdf", width=4.5,height=6.5)        
options(graphics.record=TRUE)

#modNames <- 1:length(modelName)
modNames<-rtab$model
par(mfrow=c(1,1), mar=c(5, 5, 2, 2))

### deviance
ord <- order(rtab$deviance)
plot(rtab$deviance[ord], 1:length(modNames), yaxt="n", xlab="deviance", ylab="", pch=16,
     xlim=c(min(rtab$deviance.l), max(rtab$deviance.u)), cex=0.8)
segments(x0=rtab$deviance.l[ord], x1=rtab$deviance.u[ord], 
         y0=1:length(modNames), y1=1:length(modNames))
axis(2, at=1:length(modNames), modNames[ord], las=2)
grid(, lwd=1.5, col="gray")


### DIC
ord <- order(rtab$DIC)
plot(rtab$DIC[ord], 1:length(modNames), yaxt="n", xlab="DIC", ylab="", pch=16, main="DIC")
axis(2, at=1:length(modNames), modNames[ord], las=2)
grid(, lwd=1.5, col="gray")

### pD
#ord <- order(rtab$DIC)
#plot(rtab$pD[ord], 1:length(modNames), yaxt="n", xlab="pD", ylab="", pch=16)
#axis(2, at=1:length(modNames), modNames[ord], las=2)
#grid(, lwd=1.5, col="gray")

### R
#ord <- order(rtab$DIC)
#plot(rtab$R[ord]*1000, 1:length(modNames), 
#     yaxt="n", xlab="Rx1000", ylab="", pch=16,
#     col=(is.na(rtab$tau.g)+1))
#axis(2, at=1:length(modNames), modNames[ord], las=2)
#grid(, lwd=1.5, col="gray")

# deviance vs. R
#plot(rtab$deviance, rtab$R*1000, 
#     pch=16, ylab="Rx1000", xlab="deviance",
#     col=(is.na(rtab$tau.g)+1))
#grid(, lwd=1.5, col="gray")
#title("red: individual tau")

#plot(rtab$deviance, rtab$pD, pch=16, 
#     ylab="pD", xlab="deviance",
#     col=(is.na(rtab$tau.g)+1))
#grid(, lwd=1.5, col="gray")


## body density in each model by individual
ord <- order(rtab$DIC)
minBD <- min(body.mat, na.rm=T)
maxBD <- max(body.mat, na.rm=T)
plot(body.mat[ord,1], 1:length(modNames), col=NA,
     yaxt="n", xlab="body density", ylab="", xlim=c(minBD, maxBD))
for(w in 1:length(whales)) {
  tempI <- !is.na(body.mat[ord,w])
  lines(body.mat[ord[tempI],w], c(1:length(modNames))[tempI], col="blue")           
  abline(h=c(1:length(modNames))[!tempI], col="white", lwd=15)
}
points(rtab$body.density.g[ord], 1:length(modNames), col="orange", lwd=2)
axis(2, at=1:length(modNames), modNames[ord], las=2)
grid(, lwd=1.5, col="gray")
title ("orange-global mean, blue-individual")

## CdAM
ord <- order(rtab$DIC)
minBD <- min(CdAM.mat, na.rm=T)
maxBD <- max(CdAM.mat, na.rm=T)
plot(CdAM.mat[ord,1], 1:length(modNames), col=NA,
     yaxt="n", xlab="CdAM", ylab="", xlim=c(minBD, maxBD))
for(w in 1:length(whales)) {
  tempI <- !is.na(CdAM.mat[ord,w])
  lines(CdAM.mat[ord[tempI],w], c(1:length(modNames))[tempI], col="blue")          
  abline(h=c(1:length(modNames))[!tempI], col="white", lwd=15)
}
points(rtab$CdAM.g[ord], 1:length(modNames), col="orange", pch=1, lwd=2)
axis(2, at=1:length(modNames), modNames[ord], las=2)
grid(, lwd=1.5, col="gray")
title ("orange-global mean, blue-individual")


## Vair
ord <- order(rtab$DIC)
minBD <- min(Vair.mat, na.rm=T)
maxBD <- max(Vair.mat, na.rm=T)
plot(Vair.mat[ord,1], 1:length(modNames), col=NA,
     yaxt="n", xlab="Vair", ylab="", xlim=c(minBD, maxBD))
for(w in 1:length(whales)) {
  tempI <- !is.na(Vair.mat[ord,w])
  lines(Vair.mat[ord[tempI],w], c(1:length(modNames))[tempI], col="blue")          #col=(4+c(w<14)*5))
  abline(h=c(1:length(modNames))[!tempI], col="white", lwd=15)
}
points(rtab$Vair.g[ord], 1:length(modNames), col="orange", pch=1, lwd=2)
axis(2, at=1:length(modNames), modNames[ord], las=2, cex=1)
grid(, lwd=1.5, col="gray")
title ("orange-global mean, blue-individual")

# compressibility
ord <- order(rtab$DIC)
minBD <- min(rtab$compr, na.rm=T)
maxBD <- max(rtab$compr, na.rm=T)
plot(rtab$compr[ord], 1:length(modNames), col=NA,
     yaxt="n", xlab="Compressibility", ylab="", xlim=c(minBD-0.1, maxBD+0.1))
points(rtab$compr[ord], 1:length(modNames), col="orange", pch=1, lwd=2) 
axis(2, at=1:length(modNames), modNames[ord], las=2, cex=1)
grid(, lwd=1.5, col="gray")
title ("orange-global mean")

par(mfrow=c(1,1), mar=c(5, 5, 10, 2))

options(graphics.record=FALSE)      
dev.off() 


# ########### Make a plot of model type ###################
# d<-read.csv("", head=T) 
# 
# d$DIC<-rtab$DIC
# 
# pdf("model_parameters.pdf", width=4.5,height=6.5)
# options(graphics.record=TRUE)
# 
# par(mfrow=c(1,1), mar=c(5, 5, 2, 2))
# 
# ord<-order(d$DIC)
# xx<-c("BD", "CdAM", "Vair")
# xBD<-rep(1, length(d$DIC))
# xCd<-rep(2, length(d$DIC))
# xVair<-rep(3, length(d$DIC))
# leg<-c("global only", "global+indiv", "global+dive")
# 
# 
# plot(xBD, 1:length(d$DIC), pch=d$body.density.type[ord], 
#      xlab="", ylab="", xlim=c(0.5, 4.7), yaxt="n", xaxt="n", main="parameters")
# grid(,lwd=1.5, col="gray")
# points(xCd, 1:length(d$DIC), pch=d$CdAM.type[ord])
# points(xVair, 1:length(d$DIC), pch=d$Vair.type[ord])
# axis(2, at=1:length(d$DIC), d$Model[ord], las=2)
# axis(1, at=1:3, xx, las=2)
# legend(3.3, length(d$DIC), leg[1:3], pch=c(1, 15, 17), cex=0.8)
# 
# 
# options(graphics.record=FALSE)      
# dev.off() 







