
# make trajectory plots for the selected model
#dataDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"
modelDir <- "D:\\Analysis\\SW sonar chapter\\statistical_analysis\\data\\body_density\\"
downstreamDir = modelDir

setwd(modelDir)

library(lattice)
library(R2jags)
set.seed(0)

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




pdf(paste(downstreamDir,"plot_best_", modelName, "_tracehistories.pdf", sep=""), width=10,height=5)

options(graphics.record=TRUE)


## trace history

myPlots(fit$BUGSoutput, "deviance")
myPlots(fit$BUGSoutput, "body.density", titles=fit.whales)
myPlots(fit$BUGSoutput, "body.density.g")
myPlots(fit$BUGSoutput, "body.var")
myPlots(fit$BUGSoutput, "CdAM", titles=fit.whales)
myPlots(fit$BUGSoutput, "Vair")
myPlots(fit$BUGSoutput, "Vair.var")
myPlots(fit$BUGSoutput, "compr")

options(graphics.record=FALSE)      
dev.off() 


