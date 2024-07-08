
# make trajectory plots for the selected model

setwd("/Users/georgesato/R/BD_estimation/oo23/")
dataDir<-"/Users/georgesato/R/BD_estimation/oo23/oo_glides/"
modelDir<-"/Users/georgesato/R/BD_estimation/oo23/oo_models/"
library(R2jags)
library(lattice)
#library(RColorBrewer)

modelName<-c("model(oo12)")   ### insert the selected model
m<-1


# load & filter data -> fitName
load("all_whales.Rd")
fitName <- "_f_pitch30_depth100"
filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>100 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
  is.na(tab$mean.speed)==F & tab$r.roll>0.9 & tab$r.pitch>0.9 & tab$r.heading>0.9  & is.na(tab$DswGG)==F
#& tab$mean.depth>200 & tab$sonar==0 & tab$vertglide==1
tab <- tab[filterBool,]
tab <- tab[rowSums(is.na(tab)) != ncol(tab), ]

whales <- unique(tab$whale.id)


# load model & data & filter data
load(paste(modelName[m], fitName, ".Rd", sep=""))

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


