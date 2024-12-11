library(R2jags)
library(RColorBrewer)
require(lattice)

# make trajectory plots for the selected model

setwd("D:\\Analysis\\3S4\\Body density")
dataDir<-".//Data//"
modelDir<-".\\oo_models_GS\\"

whales <- seq(1:5)
modelName<-c("model(oo12)")   ### insert the selected model
m<-1
fitName <- "_f_pitch30_depth100_rpitch999_thinned"



# load model & data & filter data
load(paste(modelDir,modelName[m], fitName, ".Rd", sep=""))

# check whales and numeric whale names match
tapply(tab$whale.id, sp.data$whale.id, unique)
location <- c(rep(1,11),rep(2,14))
data.frame(whale=tapply(tab$whale.id, sp.data$whale.id, unique),
           #location, 
           body.density=fit$BUGSoutput$mean$body.density)



myPlots <- function(fit, par.name, titles="", diag=T) {
  
  n <- length(fit$mean[names(fit$mean)==par.name][[1]])
  
  if(n>1) {
    
    tau.mcmc <- list()
    for(j in 1:n) {
      
      tau.mcmc[[j]] <- mcmc.list(mcmc(fit$sims.array[,1,paste(par.name, "[",j,"]",sep="")]),
                                 mcmc(fit$sims.array[,2,paste(par.name, "[",j,"]",sep="")]),
                                 mcmc(fit$sims.array[,3,paste(par.name, "[",j,"]",sep="")]))
      
      temp <- gelman.diag(tau.mcmc[[j]])
      par(mfrow=c(1,2))
      traceplot(tau.mcmc[[j]])
      title(par.name)
      gelman.plot(tau.mcmc[[j]], main=round(temp$psrf[[1]],3), auto.layout=F)
      
      par(mfrow=c(1,1))
      lattice.options()
      plotObj <- densityplot(tau.mcmc[[j]], auto.layout=F,
                             main=list(label=paste(titles[j], par.name, "mean",
                                                   signif(summary(tau.mcmc[[j]])$statistics["Mean"]))))
      
      print(plotObj)
      
      
    }
    
  } else {
    
    tau.mcmc <- mcmc.list(mcmc(fit$sims.array[,1,par.name]),
                          mcmc(fit$sims.array[,2,par.name]),
                          mcmc(fit$sims.array[,3,par.name]))
    
    temp <- gelman.diag(tau.mcmc)
    par(mfrow=c(1,2))
    traceplot(tau.mcmc)
    title(par.name)
    gelman.plot(tau.mcmc, main=round(temp$psrf[[1]],3), auto.layout=F)
    
    par(mfrow=c(1,1))
    lattice.options()
    plotObj <- densityplot(tau.mcmc, auto.layout=F,
                           main=list(label=paste(par.name, "mean",
                                                 signif(summary(tau.mcmc)$statistics["Mean"]))))
    
    print(plotObj)
    
  }
  
  return(tau.mcmc)
}



myPlots2 <- function(fit, par.name) {
  
  n <- length(fit$mean[names(fit$mean)==par.name][[1]])
  
  
  if(n==3) {
    
    tau.mcmc <- mcmc.list(mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,1]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,2]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,3]))
    
    plotObj <- densityplot(tau.mcmc, auto.layout=F, xlab=par.name,
                           auto.key=list(text=whales))
    print(plotObj)
  } 
  
  if(n==6) {
    
    tau.mcmc <- mcmc.list(mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,1]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,2]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,3]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,4]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,5]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,6]))
    
    plotObj <- densityplot(tau.mcmc, auto.layout=F, xlab=par.name,
                           auto.key=list(text=whales))
    print(plotObj)
  }
  
  if(n==7) {
    
    tau.mcmc <- mcmc.list(mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,1]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,2]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,3]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,4]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,5]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,6]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,7]))
    
    plotObj <- densityplot(tau.mcmc, auto.layout=F, xlab=par.name,
                           auto.key=list(text=whales))
    print(plotObj)
  }  
  
  if(n==12) {
    
    tau.mcmc <- mcmc.list(mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,1]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,2]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,3]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,4]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,5]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,6]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,7]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,8]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,9]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,10]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,11]),
                          mcmc(fit$sims.list[names(fit$sims.list)==par.name][[1]][,11]))
    
    plotObj <- densityplot(tau.mcmc, auto.layout=F, xlab=par.name,
                           auto.key=list(text=whales))
    print(plotObj)
  } 
}






pdf(paste("plot_best_", modelName[m], "_tracehistories.pdf", sep=""), width=5,height=5)
options(graphics.record=TRUE)


## trace history

myPlots(fit$BUGSoutput, "deviance")
myPlots(fit$BUGSoutput, "body.density")
myPlots(fit$BUGSoutput, "body.density.g")
myPlots(fit$BUGSoutput, "body.var")
myPlots(fit$BUGSoutput, "CdAM")
myPlots(fit$BUGSoutput, "Vair")
myPlots(fit$BUGSoutput, "Vair.var")
myPlots(fit$BUGSoutput, "compr")



options(graphics.record=FALSE)      
dev.off() 

############################### COLOURS



#whales <- c("1_H607", "2_H686")

whaleCol<-c("black", "blue", "magenta", "darkgreen", (brewer.pal(5,"Set1")), "darkgrey", "pink",
            "black", "blue", "magenta", "darkgreen", (brewer.pal(5,"Set1")), "darkgrey", "pink",
            "black", "blue", "magenta")
ltys<-c(rep(1, 11), rep(2, 11), rep(3, 3))

#whaleCol <- c("chocolate", "chocolate1","chocolate3","chocolate4",
#              "coral","aquamarine2","coral3","aquamarine4",
#              "cadetblue", "cadetblue1", "cadetblue2", "cadetblue3")

colVec2 <- rep("", length(tab$whale.id))
colVec2[tab$whale.id==whales[1]] <- "chocolate"
colVec2[tab$whale.id==whales[2]] <- "chocolate1"
colVec2[tab$whale.id==whales[3]] <- "chocolate3"
colVec2[tab$whale.id==whales[4]] <- "chocolate4"

colVec2[tab$whale.id==whales[5]] <- "coral"
colVec2[tab$whale.id==whales[6]] <- "aquamarine2"
colVec2[tab$whale.id==whales[7]] <- "coral3"
colVec2[tab$whale.id==whales[8]] <- "aquamarine4"

colVec2[tab$whale.id==whales[9]] <- "cadetblue"
colVec2[tab$whale.id==whales[10]] <- "cadetblue1"
colVec2[tab$whale.id==whales[11]] <- "cadetblue2"
colVec2[tab$whale.id==whales[12]] <- "cadetblue3"


pitchCols1 <- rep("", length(tab$mean.pitch))
pitchCols1[tab$mean.pitch<0] <- "grey"
pitchCols1[tab$mean.pitch>0] <- "lightblue"

pitchCols2 <- rep("", length(tab$mean.pitch))
pitchCols2[tab$mean.pitch<0] <- "black"
pitchCols2[tab$mean.pitch>0] <- "blue"

#rbPal <- colorRampPalette(c('blue','red'))
rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))

pitchBreaks <- seq(-90, 90, 10)
mycols <- rbPal(length(pitchBreaks))[as.numeric(cut(tab$mean.pitch,breaks = pitchBreaks))]


#############################





pdf(paste("plot_best_", modelName[m], ".pdf", sep=""), width=5,height=5)
options(graphics.record=TRUE)

#myPlots2(fit$BUGSoutput, "tau")
#myPlots2(fit$BUGSoutput, "body.density")
#myPlots2(fit$BUGSoutput, "CdAM")
#myPlots2(fit$BUGSoutput, "Vair")

# tau
#plot(1,1,xlim=c(0,16000), ylim=c(0,0.01), col=NA,
#     xlab="tau", ylab="posterior density")
#grid()
#for(w in 1:length(whales)) {
#  lines(density(fit$BUGSoutput$sims.list$tau[,w]),
#        col=whaleCol[w])
#}
#legend(x=10000, y=0.008, legend=whales, lty=1, col=whaleCol, cex=0.7)

# whales2 <- c("1_H607", "2_H686", "3_H761", "4_H731", "5_H698", "6_H584", 
#              "7_H707", "8_H755", "9_H607", "10_H002", "11_H405", 
#              "Mn09_127a", "Mn09_136", "Mn09_140", "Mn09_148", "Mn09_152", 
#              "Mn10_133a", "Mn10_139a", "Mn10_139b", "Mn10_143",
#              "Mn10_144","Mn10_146", "Mn10_151", "Mn10_155a", "Mn10_155b")
# 

# body density
test<-density(fit$BUGSoutput$sims.list$body.density[,1])
plot(1,1,xlim=c(1020, 1048),
     ylim=c(0,max(test$y)), col=NA,
     xlab="body density", ylab="posterior density")
grid()
for(w in 1:length(whales)) {
lines(density(fit$BUGSoutput$sims.list$body.density[,w]),
      col=whaleCol[w], lty=ltys[w])
}
numG<-c(1)
for(w in 1:length(whales)) {
  filterW<-tab$whale.num==w
  tabW<-tab[filterW,]
  numG[w]<-length(tabW$acceleration)
}
legend(x=1040, y=max(test$y), legend=paste(whales[1:length(whales)], "[", numG[1:length(numG)], "]", sep=""),
       lty=ltys, col=whaleCol, cex=0.5)
if(length(fit$BUGSoutput$sims.list$body.density.g)>0){
  lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000050"), lwd=7)
  legend(x=1040, y=max(test$y)*0.1, "global mean", lty=1, lwd=7, col=c("#00000050"), cex=0.6)
}



# body density (ENLARGED)
test<-2.5
plot(1,1,xlim=c(1020, 1048),
     ylim=c(0,max(test)), col=NA,
     xlab="body density", ylab="posterior density")
grid()
for(w in 1:length(whales)) {
  lines(density(fit$BUGSoutput$sims.list$body.density[,w]),
        col=whaleCol[w], lty=ltys[w], lwd = 2)
}
numG<-c(1)
for(w in 1:length(whales)) {
  filterW<-tab$whale.num==w
  tabW<-tab[filterW,]
  numG[w]<-length(tabW$acceleration)
}
legend(x=1045, y=max(test), legend=paste(whales[1:length(whales)], "[", numG[1:length(numG)], "]", sep=""),
       lty=ltys, col=whaleCol, cex=0.5)
if(length(fit$BUGSoutput$sims.list$body.density.g)>0){
  lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000050"), lwd=7)
  legend(x=1045, y=max(test)*0.1, "global mean", lty=1, lwd=7, col=c("#00000050"), cex=0.6)
}



# Vair.d (for indiv-specific)
if(ncol(fit$BUGSoutput$sims.list$Vair.d)==length(whales)){
test<-density(fit$BUGSoutput$sims.list$Vair.d[,6])
plot(1,1,xlim=c(2,50), ylim=c(0,max(test$y)*2), col=NA,
     xlab="Vair", ylab="posterior density")
grid()
for(w in 1:length(whales)) {
  lines(density(fit$BUGSoutput$sims.list$Vair.d[,w]),
        col=whaleCol[w], lty=ltys[w])
}
lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#80808030"), lwd=3)
legend(x=35, y=max(test$y)*2, legend=whales, lty=ltys, col=whaleCol, cex=0.7)
legend(x=35, y=max(test$y)*0.8, "global", lty=1, col=c("#80808030"), lwd=3, cex=0.7)
}

# Vair global
test<-density(fit$BUGSoutput$sims.list$Vair)
plot(1,1,xlim=c(2,50), ylim=c(0,max(test$y)*1.5), col=NA,
     xlab="Vair", ylab="posterior density",
     main=paste("mean global Vair = ",
                signif(mean(fit$BUGSoutput$sims.list$Vair), digits=3), " (sd = ",
                signif(sd(fit$BUGSoutput$sims.list$Vair), digits=3), ")", sep=""))
lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#00000040"), lwd=5)
legend(x=35, y=max(test$y), "global Vair", lty=1, col=c("#00000040"), lwd=5, cex=0.7)


# CdAM
test<-density(fit$BUGSoutput$sims.list$CdAM[,1])
plot(1,1,xlim=c(0,80), ylim=c(0,max(test$y)), col=NA,
     xlab="CdAM", ylab="posterior density")
grid()
for(w in 1:length(whales)) {
  lines(density(fit$BUGSoutput$sims.list$CdAM[,w]),
        col=whaleCol[w], lty=ltys[w])
}
legend(x=60, y=max(test$y), legend=whales, lty=ltys, col=whaleCol, cex=0.5)
if(length(fit$BUGSoutput$sims.list$CdAM.g)>0){
  lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
  legend(x=60, y=max(test$y)*0.1, "global mean", lty=1, lwd=5, col=c("#00000040"), cex=0.5)
}

# compressibility
plot(density(fit$BUGSoutput$sims.list$compr), col="lightgrey", lwd=5,
     xlab="Compressibility", ylab="posterior density", xlim=c(0.1, 0.8), ylim=c(0, 5),
     main=paste("mean compr = ",
                signif(mean(fit$BUGSoutput$sims.list$compr), digits=3), " (sd = ",
                signif(sd(fit$BUGSoutput$sims.list$compr), digits=3), ")", sep=""))



# body density.g with uniform prior
if(length(fit$BUGSoutput$sims.list$body.density.g)>0){
  test<-density(fit$BUGSoutput$sims.list$body.density.g)
  plot(1,1,xlim=c(800, 1400),
       ylim=c(0,max(test$y)), col=NA,
       xlab="body density", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$body.density.g, digit=5), sep=""))
  lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000040"), lwd=3)
  legend(x=1200, y=max(test$y)*1, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=1200, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  segments(800, 0, 1400, 0, col=c("#00008B50"), lwd=10)
}



# CdAM.g with uniform prior
if(length(fit$BUGSoutput$sims.list$CdAM.g)>0){
  test<-density(fit$BUGSoutput$sims.list$CdAM.g)
  plot(1,1,xlim=c(0, 80),
       ylim=c(0,max(test$y)*1.2), col=NA,
       xlab="CdAM (E-06)", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$CdAM.g, digit=3), sep=""))
  lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
  legend(x=30, y=max(test$y)*1.2, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=30, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  xx<-seq(5, 20, 0.1)
  lines(xx, dnorm(xx, 10, 2), col=c("#00008B50"), lwd=5)
}


# gloal Vair with uniform prior
if(length(fit$BUGSoutput$sims.list$Vair)>0){
  test<-density(fit$BUGSoutput$sims.list$Vair)
  plot(1,1,xlim=c(2, 50),
       ylim=c(0,max(test$y)*1.5), col=NA,
       xlab="Vair", ylab="posterior density",
       main=paste("mean =", signif(fit$BUGSoutput$mean$Vair, digit=3), sep=""))
  lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#00000040"), lwd=5)
  legend(x=35, y=max(test$y)*1, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=35, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  segments(5, 0, 50, 0, col=c("#00008B50"), lwd=10)
}


# compr with uniform prior
# compressibility
test<-density(fit$BUGSoutput$sims.list$compr)
plot(density(fit$BUGSoutput$sims.list$compr), col=c("#00000040"), lwd=5,
     xlim=c(0, 0.8), ylim=c(0, max(test$y)), 
     xlab="Compressibility", ylab="posterior density",
     main=paste("mean =", signif(fit$BUGSoutput$mean$compr, digit=3), sep=""))
  legend(x=0.6, y=max(test$y)*1, "global mean", lty=1, lwd=3, col=c("#00000040"), cex=0.6)
  legend(x=0.6, y=max(test$y)*0.2, "prior range", lty=1, lwd=5, col=c("#00008B50"), cex=0.6)
  segments(0.1, 0, 0.7, 0, col=c("#00008B50"), lwd=10)


# prior info
par(mfrow=c(2,1))

# gamma prior for indiv. body density
shape<-(fit$BUGSoutput$mean$body.density.g)^2/(fit$BUGSoutput$mean$body.var)
rate<-(fit$BUGSoutput$mean$body.density.g)/(fit$BUGSoutput$mean$body.var)
xx<-seq(1027, 1042, 0.1)
plot(xx, dgamma(xx, shape, rate),col=c("#00008B50"), lwd=5,type="l",
     xlim=c(min(xx), max(xx)), main="Gamma prior for indiv. body density",
     xlab="body density")

# gamma prior for indiv. CdAM
shape<-(fit$BUGSoutput$mean$CdAM.g)^2/(fit$BUGSoutput$mean$CdAM.var)
rate<-(fit$BUGSoutput$mean$CdAM.g)/(fit$BUGSoutput$mean$CdAM.var)
xx<-seq(0, 40, 0.1)
plot(xx, dgamma(xx, shape, rate),col=c("#00008B50"), lwd=5,type="l",
     main="Gamma prior for indiv. CdAM",
     xlab="CdAM (E-06)")

# gamma prior for Vair.d
shape<-(fit$BUGSoutput$mean$Vair)^2/(fit$BUGSoutput$mean$Vair.var)
rate<-(fit$BUGSoutput$mean$Vair)/(fit$BUGSoutput$mean$Vair.var)
xx<-seq(0, 100, 0.1)
plot(xx, dgamma(xx, shape, rate),col=c("#00008B50"), lwd=5,type="l",
     main="Gamma prior for Vair.d",
     xlab="Vair.d")

#plot(1,1,xlim=c(0,40), ylim=c(0,max(test$y)*2), col=NA)



par(mfrow=c(1,1))
# measured tau
test<-density(sp.data$tau)
plot(1,1,xlim=c(min(sp.data$tau),max(sp.data$tau)), ylim=c(0,max(test$y)*8), col=NA,
     xlab="TAU used in the model", ylab="density")
grid()
for(w in 1:length(whales)) {  
  wBool <- sp.data$whale.id==w  
  lines(density(sp.data$tau[wBool]), col=whaleCol[w], lty=ltys[w])
}
legend(max(sp.data$tau)*0.7, max(test$y)*8, legend=whales, lty=ltys, col=whaleCol, cex=0.5)





plot(sp.data$a, fit$BUGSoutput$mean$a.mu, ylab="", xlab="", col=whaleCol[sp.data$whale.id], cex=0.8)
mtext("observed acceleration", side=1, line=3, cex=1.2)
mtext("posterior mean", side=2, line=3, cex=1.2)
abline(0,1)
grid()


plot(sp.data$mean.speed, sp.data$a,
     ylab="", xlab="", 
     main="")
points(sp.data$mean.speed, fit$BUGSoutput$mean$a.mu, col=whaleCol[sp.data$whale.id], cex=0.8)
grid()
mtext("speed (m/s)", side=1, line=3, cex=1.2)
mtext("acceleration", side=2, line=3, cex=1.2)
mtext("observed and fitted (in colour)", side=3, line=1, cex=1.2, font=2)







## observed a alone vs glide depth
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
mtext("obs a vs depth (coloured by speed)", side=3, line=1, cex=1.2, font=2)
legend(max(sp.data$depth)*0.9,max(sp.data$a)-0.15, legend=spBreakLegend[1:length(spBreakLegend)], 
       fill=rbPal2(length(spBreakLegend))[1:length(spBreakLegend)][1:length(spBreakLegend)], cex=0.6)



#plot(sp.data$depth, sp.data$a, ylab="", xlab="",main="", col=pitchCols1, cex=0.8, pch=1)
plot(sp.data$depth, sp.data$a, ylab="", xlab="",main="", col="grey", cex=0.5, pch=1)
grid()
points(sp.data$depth, fit$BUGSoutput$mean$a.mu, col=SPcols, pch=3, cex=0.5)
mtext("depth (m)", side=1, line=3, cex=1.2)
mtext("acceleration", side=2, line=3, cex=1.2)
mtext("obs + fitted", side=3, line=1, cex=1.2, font=2)
legend(max(sp.data$depth)*0.9,max(sp.data$a)-0.15, legend=spBreakLegend[1:length(spBreakLegend)], 
       fill=rbPal2(length(spBreakLegend))[1:length(spBreakLegend)][1:length(spBreakLegend)], cex=0.6)



## observed data alon, color coded by pitch
rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))
pitchBreaks <- seq(-90, 90, 10)
sp.data$mean.pitch<-asin(sp.data$sin.pitch)*180/pi      # add mean.pitch to sp.data
mycols <- rbPal(length(pitchBreaks))[as.numeric(cut(sp.data$mean.pitch,breaks = pitchBreaks))]

plot(sp.data$mean.speed, sp.data$a, ylab="", xlab="",main="", col=mycols, cex=0.5, pch=1, 
     xlim=c(0, max(sp.data$mean.speed)*1.2))
grid()
mtext("speed (m/s)", side=1, line=3, cex=1.2)
mtext("obs. acceleration", side=2, line=3, cex=1.2)
mtext("obs a vs speed", side=3, line=1, cex=1.2, font=2)
legend(max(sp.data$mean.speed)*1.1,max(sp.data$a), legend=pitchBreaks[-(8:12)], 
       fill=rbPal(length(pitchBreaks))[(1:20)[-(8:12)]], cex=0.6)


## observed data & estimated a
plot(sp.data$mean.speed, sp.data$a, ylab="", xlab="",main="", 
     col="grey", cex=0.5, pch=1, xlim=c(0, max(sp.data$mean.speed)*1.2))
points(sp.data$mean.speed, fit$BUGSoutput$mean$a.mu, col=mycols, pch=3, cex=0.5)
grid()
mtext("speed (m/s)", side=1, line=3, cex=1.2)
mtext("acceleration", side=2, line=3, cex=1.2)
mtext("obs + fitted", side=3, line=1, cex=1.2, font=2)
legend(max(sp.data$mean.speed)*1.1,max(sp.data$a), legend=pitchBreaks[-(8:12)], 
       fill=rbPal(length(pitchBreaks))[(1:20)[-(8:12)]], cex=0.6)

#for(w in 1:length(whales)) {
  
#  wBool <- sp.data$whale.id==w
  
  ## observed data alon
#  plot(sp.data$mean.speed[wBool], sp.data$a[wBool], ylab="", xlab="",main="", 
#       col="grey", cex=0.5, pch=1)
#  points(sp.data$mean.speed[wBool], fit$BUGSoutput$mean$a.mu[wBool], col=mycols, pch=3, cex=0.5)
#  grid()
#  mtext("speed (m/s)", side=1, line=3, cex=1.2)
#  mtext("acceleration", side=2, line=3, cex=1.2)
#  mtext(whales[w], side=3, line=1, cex=1.2, font=2)
  
#}



# plot observed A and estimated A with speed
rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))
pitchBreaks<-seq(-90, 90, 10)

sp.data$mean.pitch<-asin(sp.data$sin.pitch)*180/pi      # add mean.pitch to sp.data

for(w in 1:length(whales)) {
  
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
  mtext(whales[w], side=3, line=1, cex=1.2, font=2)
  legend(max(sp.data$mean.speed),max(sp.data$a), legend=pitchBreaks[-(8:12)], 
         fill=rbPal(length(pitchBreaks))[(1:20)[-(8:12)]], cex=0.7)
}






#### BODY DENSITY VS tau
#########################################################
#colFun1 <- colorRampPalette(paste0("lightblue", 1:4))
#colFun2 <- colorRampPalette(paste0("orange", 1:4)) 
#w <- 1
#tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
#                       fit$BUGSoutput$sims.list$tau[,w])
#temp <- densCols(tempData, colramp=colFun1)
#plot(tempData[,1], tempData[,2],
#     col=temp, xlab="", ylab="", xlim=c(min(fit$BUGSoutput$sims.list$body.density), 
#                                        max(fit$BUGSoutput$sims.list$body.density)),
#     ylim=c(min(fit$BUGSoutput$sims.list$tau), 
#            max(fit$BUGSoutput$sims.list$tau)),
#     pch=16, cex=0.4)

#grid(col="darkgrey")

#for(w in 1:length(whales)) {
#  if (dim(fit$BUGSoutput$sims.list$tau)[2]>1) {
#    w2 <- w
#  } else {w2 <-1}
  
#  tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
#                         fit$BUGSoutput$sims.list$tau[,w2])
#  temp <- densCols(tempData, colramp=colFun1)
#  if(location[w]==2) { temp <- densCols(tempData, colramp=colFun2)}

#  points(tempData[,1], tempData[,2],
#         col=temp, xlab="", ylab="", pch=16, cex=0.4) 
#}
#mtext("body density", side=1, line=3, cex=1.2)
#mtext("precision", side=2, line=3, cex=1.2)

#for(w in 1:length(whales)) {
#  text(mean(fit$BUGSoutput$sims.list$body.density[,w]), 
#       mean(fit$BUGSoutput$sims.list$tau[,w]), whales[w], cex=0.7)
#}


#### BODY DENSITY VS Vair
#########################################################
colFun1 <- colorRampPalette(paste0("lightblue", 1:4))
colFun2 <- colorRampPalette(paste0("orange", 1:4)) 

test<-dim(fit$BUGSoutput$sims.list$Vair.d)
if(test[2]==length(whales)){

w <- 1
tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
                       fit$BUGSoutput$sims.list$Vair.d[,w])
temp <- densCols(tempData, colramp=colFun1)
plot(tempData[,1], tempData[,2],
     col=temp, xlab="", ylab="", xlim=c(min(fit$BUGSoutput$sims.list$body.density), 
                                        max(fit$BUGSoutput$sims.list$body.density)),
     ylim=c(min(fit$BUGSoutput$sims.list$Vair.d), 
            max(fit$BUGSoutput$sims.list$Vair.d)),
     pch=16, cex=0.4)
grid(col="darkgrey")

for(w in 1:length(whales)) {
  if (dim(fit$BUGSoutput$sims.list$Vair.d)[2]>1) {
    w2 <- w
  } else {w2 <-1}
  
  tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
                         fit$BUGSoutput$sims.list$Vair.d[,w2])
  temp <- densCols(tempData, colramp=colFun1)
  if(location[w]==2) { temp <- densCols(tempData, colramp=colFun2)}
  
  points(tempData[,1], tempData[,2],
         col=temp, xlab="", ylab="", pch=16, cex=0.4) 
}
mtext("body density", side=1, line=3, cex=1.2)
mtext("Vair", side=2, line=3, cex=1.2)

for(w in 1:length(whales)) {
  text(mean(fit$BUGSoutput$sims.list$body.density[,w]), 
       mean(fit$BUGSoutput$sims.list$Vair.d[,w]), whales[w], cex=0.7)
}

}


test<-dim(fit$BUGSoutput$sims.list$Vair.d)
if(test[2]>length(whales)){        ######### in the case Vair.d is dive-by-dive Vair
  
  diveTab <- data.frame(dive=tapply(sp.data$dive.id, sp.data$dive.id, max))
  diveTab$duration <- tapply(tab$dive.duration, sp.data$dive.id, max)
  diveTab$max.depth <- tapply(tab$dive.max.depth, sp.data$dive.id, max)
  diveTab$ind <- tapply(tab$whale.num, sp.data$dive.id, max)
  diveTab$ind2 <- tapply(tab$whale.id, sp.data$dive.id, unique)
  
  airTab <-data.frame(whale.id = whales)
  BDTab <-data.frame(whale.id = whales)
  
  for(w in 1:length(whales)){
  tempData0<-subset(diveTab, ind==w)
  whaleAir<-fit$BUGSoutput$sims.list$Vair.d[, min(tempData0$dive):max(tempData0$dive)]  
  
  airTab$mean[w]<- mean(whaleAir)
  airTab$L95[w]<- quantile(whaleAir, prob=0.025)
  airTab$U95[w]<- quantile(whaleAir, prob=0.975)
  airTab$U50[w]<-quantile(whaleAir, prob=0.75)
  airTab$L50[w]<-quantile(whaleAir, prob=0.25)
  airTab$range95[w]<-airTab$U95[w]-airTab$L95[w]
  
  BDTab$mean[w]<- fit$BUGSoutput$mean$body.density[w]
  BDTab$L95[w]<- quantile(fit$BUGSoutput$sims.list$body.density[,w], prob=0.025)
  BDTab$U95[w]<- quantile(fit$BUGSoutput$sims.list$body.density[,w], prob=0.975)
  }
  
  # plot posterior density per whale
  plot(1,1,xlim=c(0,120), ylim=c(0,0.2), col=NA, xlab="Vair", ylab="posterior density",
       main="posterior density of Vair - manually obtained per whale")
  for(w in 1:length(whales)){
    tempData0<-subset(diveTab, ind==w)
    whaleAir<-fit$BUGSoutput$sims.list$Vair.d[, min(tempData0$dive):max(tempData0$dive)]  
    lines(density(whaleAir), col=whaleCol[w], lty=ltys[w])  
  }
  legend(80, 0.2, whales, lty=ltys, col=whaleCol, cex=0.5)
  
  # one point per each dive
  plot(1,1,xlim=c(0,120), ylim=c(min(BDTab$mean), max(BDTab$mean)), col=NA, xlab="Vair", ylab="body.density",
       main="")
  whaleCol3<-c("#00000070", "#0000FF70", "#FF00FF70", "#00640070", "#E41A1C70",
               "#377EB870", "#4DAF4A70",  "#984EA370", "#FF7F0070", "#FFFF3370","#A6562870", 
               "#00000070", "#0000FF70", "#FF00FF70", "#00640070", "#E41A1C70",
               "#377EB870", "#4DAF4A70",  "#984EA370", "#FF7F0070", "#FFFF3370","#A6562870",  
               "#00000070", "#0000FF70", "#F781BF70")
  lty3=c(rep(16,11), rep(18,11), rep(17, 3))
  
  
  for(w in 1:length(whales)){
    tempData0<-subset(diveTab, ind==w)
    whaleAir<-fit$BUGSoutput$mean$Vair.d[min(tempData0$dive):max(tempData0$dive)]  
    yy<-rep(BDTab$mean[w], length(whaleAir))
    points(whaleAir, yy, pch=lty3[w], col=whaleCol3[w])
  }
  legend(120, max(BDTab$mean), whales, pch=lty3, col=whaleCol, cex=0.5)
  
  plot(BDTab$mean, airTab$mean, col=whaleCol,
       xlim=c(min(BDTab$L95), max(BDTab$U95)+10), ylim=c(min(airTab$L95), max(airTab$U95)),
       xlab="body density", ylab = "Vair", pch=ltys, cex=1.5)
  segments(BDTab$mean, airTab$L95, BDTab$mean, airTab$U95, col=whaleCol, lty=ltys)
  segments(BDTab$mean, airTab$L50, BDTab$mean, airTab$U50, col=whaleCol, lty=1, lwd=4)
  segments(BDTab$L95, airTab$mean, BDTab$U95, airTab$mean, col=whaleCol, lty=ltys)
  legend(max(BDTab$U95)+2, max(airTab$U95), 
         paste(whales, "(mean=", signif(airTab$mean[1:length(whales)], digits=3), ")", sep=""),
         pch=ltys, col=whaleCol, cex=0.5)
  legend(max(BDTab$U95)-4, max(airTab$U95), c("95% CRI", "50% CRI"), lty=c(1, 1), lwd=c(1, 4), cex=0.5)
  mtext("dive-specific Vair (mean) vs body density", side=3, line=1, cex=1.2, font=2)
  
  
  # dive depth vs Vair.d
  plot(diveTab$max.depth, fit$BUGSoutput$mean$Vair.d, pch=ltys, col=whaleCol,
       ylim=c(0, max(fit$BUGSoutput$mean$Vair.d)+max(fit$BUGSoutput$sd$Vair.d)),
       xlab="dive depth (m)", ylab="Vair.d", main="all dives (mean with SD)")
  segments(diveTab$max.depth, fit$BUGSoutput$mean$Vair.d-fit$BUGSoutput$sd$Vair.d,
           diveTab$max.depth, fit$BUGSoutput$mean$Vair.d+fit$BUGSoutput$sd$Vair.d,
           col=whaleCol)
  
  # dive duration vs Vair.d
  plot(diveTab$duration, fit$BUGSoutput$mean$Vair.d, pch=ltys, col=whaleCol,
       ylim=c(0, max(fit$BUGSoutput$mean$Vair.d)+max(fit$BUGSoutput$sd$Vair.d)),
       xlab="dive duration (s)", ylab="Vair.d", main="all dives (mean with SD)")
  segments(diveTab$duration, fit$BUGSoutput$mean$Vair.d-fit$BUGSoutput$sd$Vair.d,
           diveTab$duration, fit$BUGSoutput$mean$Vair.d+fit$BUGSoutput$sd$Vair.d,
           col=whaleCol)
  
  
  # count No of sub-glides & CRI range  dive
  sp2<-data.frame(dive.id=sp.data$dive.id)
  sp2$whale.id<-sp.data$whale.id
  for(j in 1:sp.data$ND){
    tempD<-subset(sp2, dive.id==j)
    diveTab$whale.id[j]<-max(tempD$whale.id)
    diveTab$numG[j]<-nrow(tempD)
    diveTab$Vair.mean[j]<-mean(fit$BUGSoutput$sims.list$Vair.d[,j])
    diveTab$Vair.sd[j]<-sd(fit$BUGSoutput$sims.list$Vair.d[,j])    
    diveTab$Vair.U50[j]<-quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.75)
    diveTab$Vair.L50[j]<-quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.25)   
    diveTab$Vair.U95[j]<-quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.975)
    diveTab$Vair.L95[j]<-quantile(fit$BUGSoutput$sims.list$Vair.d[,j], prob=0.025)
    diveTab$range95[j]<-diveTab$Vair.U95[j]-diveTab$Vair.L95[j]     
  }  
  plot(range95~numG, diveTab, col="black", bg=whaleCol[diveTab$whale.id], pch=21,
       xlab="No of 5-s subglides in each dive", ylab="95% CRI range for Vair.d")


  # remove any dives have large CRI range (CRI range > 20)
  diveTabNew<-subset(diveTab, range95<=20) 
  #diveTabNew<-subset(diveTab, numG>10)
  
  # & plot Vair.d vs dive depth
  plot(Vair.mean~max.depth, diveTabNew, pch=1, col=whaleCol[diveTabNew$whale.id],
       xlim=c(0, max(diveTabNew$max.depth)), ylim=c(0, max(diveTabNew$Vair.U95)),
       xlab="Dive depth (m)", ylab="dive-specific Vair", cex=1,
       main="dives with CRI range <20 only")
  segments(diveTabNew$max.depth, diveTabNew$Vair.L95, 
           diveTabNew$max.depth, diveTabNew$Vair.U95, col=whaleCol[diveTabNew$whale.id])
  segments(diveTabNew$max.depth, diveTabNew$Vair.L50, 
           diveTabNew$max.depth, diveTabNew$Vair.U50, col=whaleCol[diveTabNew$whale.id], lwd=4)
  legend(max(diveTabNew$max.depth)*0.7, max(diveTabNew$Vair.U95), c("95% CRI", "50% CRI"), 
         lty=c(1, 1), lwd=c(1, 4), cex=0.5)
  
  # & plot Vair.d vs dive duration
  plot(Vair.mean~duration, diveTabNew, pch=1, col=whaleCol[diveTabNew$whale.id],
       xlim=c(0, max(diveTabNew$duration)), ylim=c(0, max(diveTabNew$Vair.U95)),
       xlab="Dive duration (s)", ylab="dive-specific Vair", cex=1,
       main="dives with CRI range <20 only")
  segments(diveTabNew$duration, diveTabNew$Vair.L95, 
           diveTabNew$duration, diveTabNew$Vair.U95, col=whaleCol[diveTabNew$whale.id])
  segments(diveTabNew$duration, diveTabNew$Vair.L50, 
           diveTabNew$duration, diveTabNew$Vair.U50, col=whaleCol[diveTabNew$whale.id], lwd=4)
  legend(max(diveTabNew$duration)*0.7, max(diveTabNew$Vair.U95), c("95% CRI", "50% CRI"), 
         lty=c(1, 1), lwd=c(1, 4), cex=0.7)
  

  
  
  for(w in 1:length(whales)){
    diveTab2<-subset(diveTabNew, ind==w)
    # dive depth vs Vair.d
    plot(diveTab2$max.depth, fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)], pch=19, col=whaleCol[w],
         ylim=c(0, max(fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)])+max(fit$BUGSoutput$sd$Vair.d[min(diveTab2$dive): max(diveTab2$dive)])),
         xlab="dive depth (m)", ylab="Vair.d (mean, sd)", main=whales[w])
    segments(diveTab2$max.depth, 
             fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)]-fit$BUGSoutput$sd$Vair.d[min(diveTab2$dive): max(diveTab2$dive)],
             diveTab2$max.depth, 
             fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)]+fit$BUGSoutput$sd$Vair.d[min(diveTab2$dive): max(diveTab2$dive)],
             col=whaleCol[w])
    # dive duration vs Vair.d
    plot(diveTab2$duration, fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)], pch=19, col=whaleCol[w],
         ylim=c(0, max(fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)])+max(fit$BUGSoutput$sd$Vair.d[min(diveTab2$dive): max(diveTab2$dive)])),
         xlab="dive duration (s)", ylab="Vair.d (mean, sd)", main = whales[w])
    segments(diveTab2$duration, 
             fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)]-fit$BUGSoutput$sd$Vair.d[min(diveTab2$dive): max(diveTab2$dive)],
             diveTab2$duration, 
             fit$BUGSoutput$mean$Vair.d[min(diveTab2$dive): max(diveTab2$dive)]+fit$BUGSoutput$sd$Vair.d[min(diveTab2$dive): max(diveTab2$dive)],
             col=whaleCol[w])
  }  
  
}



# estimated CdAM vs BD
colFun1 <- colorRampPalette(paste0("lightblue", 1:4))
colFun2 <- colorRampPalette(paste0("orange", 1:4)) 

test<-dim(fit$BUGSoutput$sims.list$CdAM)
test2<-dim(fit$BUGSoutput$sims.list$body.density)
if(test[2]==length(whales)&test2[2]==length(whales)){   #when indiv-CdAM & indiv-BD are available
  
  w <- 1
  tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
                         fit$BUGSoutput$sims.list$CdAM[,w])
  temp <- densCols(tempData, colramp=colFun1)
  plot(tempData[,1], tempData[,2],
       col=temp, xlab="", ylab="", xlim=c(min(fit$BUGSoutput$sims.list$body.density), 
                                          max(fit$BUGSoutput$sims.list$body.density)),
       ylim=c(min(fit$BUGSoutput$sims.list$CdAM), 
              max(fit$BUGSoutput$sims.list$CdAM)),
       pch=16, cex=0.4)
  grid(col="darkgrey")
  
  for(w in 1:length(whales)) {
    if (dim(fit$BUGSoutput$sims.list$Vair.d)[2]>1) {
      w2 <- w
    } else {w2 <-1}
    
    tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
                           fit$BUGSoutput$sims.list$CdAM[,w2])
    temp <- densCols(tempData, colramp=colFun1)
    if(location[w]==2) { temp <- densCols(tempData, colramp=colFun2)}
    
    points(tempData[,1], tempData[,2],
           col=temp, xlab="", ylab="", pch=16, cex=0.4) 
  }
  mtext("body density", side=1, line=3, cex=1.2)
  mtext("CdAM", side=2, line=3, cex=1.2)
  
  for(w in 1:length(whales)) {
    text(mean(fit$BUGSoutput$sims.list$body.density[,w]), 
         mean(fit$BUGSoutput$sims.list$CdAM[,w]), whales[w], cex=0.7)
  }
  
}





#### pred
#
#for(w in 1:length(whales)) {
#  
#  depth <- seq(0,2000,by=10)
#  sin.pitch <- seq(-1,1,by=0.01)
#  A <- matrix(NA, length(depth),length(sin.pitch))
#  wBool <- sp.data$whale.id==w
#  
#  for(j in 1:length(sin.pitch)) {
#    preddata <- list(0)
#    preddata$depth <- depth
#    preddata$sin.pitch <- rep(sin.pitch[j], length(depth))
#    preddata$DswGG <- mean(sp.data$DswGG)
#    preddata$mean.speed <- mean(sp.data$mean.speed)
#    preddata$whale.id <- rep(w, length(depth))
#    A[,j] <- getA(fit, preddata)
#  }
#  
#  filled.contour(depth, sin.pitch, A, color = topo.colors, 
#                 xlab="depth", ylab="sin.pitch")
#  #contour(depth, sin.pitch, A, add=T)
#  #points(sp.data$depth[wBool], sp.data$sin.pitch[wBool], 
#  #       cex=abs(sp.data$a[wBool])*50)
#  title(whales[w])
#  #scan()
#  
#}


options(graphics.record=FALSE)      
dev.off() 











#### BODY DENSITY VS Vair ####### if there is no Vair.d
#########################################################
colFun1 <- colorRampPalette(paste0("lightblue", 1:4))
colFun2 <- colorRampPalette(paste0("orange", 1:4)) 
w <- 1
tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
                       fit$BUGSoutput$sims.list$Vair[])   #use Vair instead of Vair.d
temp <- densCols(tempData, colramp=colFun1)
plot(tempData[,1], tempData[,2],
     col=temp, xlab="", ylab="", xlim=c(min(fit$BUGSoutput$sims.list$body.density), 
                                        max(fit$BUGSoutput$sims.list$body.density)),
     ylim=c(min(fit$BUGSoutput$sims.list$Vair), 
            max(fit$BUGSoutput$sims.list$Vair)),    #use Vair instead of Vair.d
     pch=16, cex=0.4)
grid(col="darkgrey")

for(w in 2:length(whales)) {
  tempData <- data.frame(fit$BUGSoutput$sims.list$body.density[,w], 
                         fit$BUGSoutput$sims.list$Vair[])   #use Vair instead of Vair.d
  temp <- densCols(tempData, colramp=colFun1)
  if(location[w]==2) { temp <- densCols(tempData, colramp=colFun2)}
  
  points(tempData[,1], tempData[,2],
         col=temp, xlab="", ylab="", pch=16, cex=0.4) 
}
mtext("body density", side=1, line=3, cex=1.2)
mtext("Vair", side=2, line=3, cex=1.2)

for(w in 1:length(whales)) {
  text(mean(fit$BUGSoutput$sims.list$body.density[,w]), 
       mean(fit$BUGSoutput$sims.list$Vair), whales[w], cex=0.7)
}

