### plot results of the selected model
dataDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"

modelDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\SW_Models"
out_dir <- "C:\\Users\\Alec Burslem\\Dropbox\\St Andrews Body Condition PHD\\Progression\\9 month report"

setwd(modelDir)

library(lattice)
library(R2jags)
set.seed(1990)

# Select model

fitName <- "_f_pitch30_depth100"      # name of the fit, based on selection of "good glides"
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



#pdf(paste("plot_best_", modelName, ".pdf", sep=""), width=6,height=5)
#options(graphics.record=TRUE)

minBD <- min(fit$BUGSoutput$sims.list$body.density)
maxBD <- max(fit$BUGSoutput$sims.list$body.density)

m3 = parse(text='m^-3')
xlabel = expression(paste("Tissue density (kg m"^"-3",")"))

setwd(out_dir)
    
## body density ####
tiff("C:\\Users\\Alec Burslem\\Dropbox\\St Andrews Body Condition PHD\\Progression\\9 month report\\bd_noleg.tiff", width = 5000, height = 3200, res= 800)
par(mar = c(5, 5, 3, 5))    
tempI <-  which.min(fit$BUGSoutput$sd$body.density) # set y-axis maximum to most narrow posterior density
test<-density(fit$BUGSoutput$sims.list$body.density[,tempI])
plot(1,1,xlim=c(minBD, maxBD+(maxBD-minBD)/10),
     ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="Posterior density - Individual")
#grid()
for(w in 1:length(fit.whales)) {
  lines(density(fit$BUGSoutput$sims.list$body.density[,w]),col="blue")
}
numG<-c(1)
for(w in 1:length(fit.whales)) {
  filterW<-tab$whale.id==fit.whales[w]
  numG[w]<-sum(filterW)
}
#legend("topright",legend=c("Global","Individual"), lty=1, lwd=7, col=c("blue","#00000050"), cex=0.6)
par(new=TRUE)
plot(density(fit$BUGSoutput$sims.list$body.density.g),cex=0.1, col=c("#00000050"), lwd=4,
     xlim=c(minBD, maxBD+(maxBD-minBD)/10),xlab="",ylab="",yaxt = "n", main ="")
axis(4)
mtext("Posterior density - Global", side = 4, line = 3)
# legend("bottomright","Global mean", lty=1, lwd=7, col=c("#00000050"), cex=0.6)
dev.off()
    



# ratio:density plot ####

val_tab = data.frame(read.csv("D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Internal validation\\validation.csv", header = TRUE))

require(ggplot2)
library(grid)
library(ggstance)

val_tab$Year =  factor(val_tab$Year)
plot(density(val_tab$Density_m))
plot(density(val_tab$Rat_ov_mean))

cor(val_tab$Density_m,val_tab$Rat_ov_mean,method ="spearman")
cor.test(val_tab$Density_m,val_tab$Rat_ov_mean,method ="spearman",exact=F) 

cor(val_tab$Density_m,val_tab$Rat_ov_mean,method ="pearson")
cor.test(val_tab$Density_m,val_tab$Rat_ov_mean,method ="pearson") 


#grob = grobTree(textGrob("Spearman's rho = -0.58\np = 0.0092", x=0.45,  y=0.85, hjust=0,
                  #gp=gpar( fontsize=13, fontface="italic")))

ggplot(data = val_tab) +
  geom_point(aes(x=Density_m, y =Rat_ov_mean,color = Year)) +
  geom_linerange(aes(x=Density_m, y =Rat_ov_mean, ymax = Rat_ov_mean+Rat_ov_STD, ymin = Rat_ov_mean-Rat_ov_STD, color = Year)) +
  geom_linerangeh(aes(x=Density_m, y =Rat_ov_mean,xmax = Density_U95, xmin = Density_L95,color = Year)) +
  labs (x = expression(paste("Tissue density (kg m"^"-3",")")), y ="Glide % (ascent - descent)") +
  theme_classic() +
  theme(legend.position = "right") +
  #annotation_custom(grob)

ggsave('ratio.png',width=5,height=5)
getwd()  

