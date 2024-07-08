# Alec Burslem
# acb35@st-andrews.ac.uk
# May 2024

#housekeeping ---- 
library(momentuHMM)
library(dplyr)
ups_dir = "D:\\Raw\\3S4\\Pseuedotracks\\"
res = 7

# load a track 
deploys = c("oo23_292b_pt", "oo23_295a_pt", "oo23_295b_pt", "oo23_299a_pt","oo23_299b_pt", "oo23_302a_pt")


# look at gps coverage
i = 1
for(deploy in deploys){
  track = R.matlab::readMat(paste(ups_dir,deploy,".mat", sep = ""))
  #plot(track$pos.gps)
  #plot(track$poswh)
  pos = as.data.frame(track$poswh)
  pos$index = 1:nrow(pos)
  pos_ds = pos[(pos$index-1)%%(res*6)==0,]
  pos_ds$ID = i
  if(i == 1){mas_posds = pos_ds}
  else{mas_posds = rbind(mas_posds,pos_ds)}
  i = i+1
  #scan()
}


x = prepData(data = mas_posds, type = "LL", coordNames = c("V2", "V1"))
pdf(paste("2state_",res,"min_explor.pdf", sep = ""))
plot(x, ask = FALSE)
dev.off()
# starting values 
stepPar0 <- c(0.05,0.125,0.02,0.15) # (mu_1,mu_2,sd_1,sd_2...)
# initial angle distribution natural scale parameters
anglePar0 <- c(1,15) #

fit = fitHMM(data = x, nbStates = 2, dist = list(step = "gamma", angle = "vm"),Par0 = list(step = stepPar0, angle = anglePar0),estAngleMean = list(angle=FALSE))
#states = viterbi(fit)
pdf(paste("2state_",res,"min.pdf", sep = ""))
plot(fit, ask = FALSE)
dev.off()
