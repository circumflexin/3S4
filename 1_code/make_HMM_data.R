library(dplyr)
library(momentuHMM)

dsfb_dir = ".\\2_pipeline\\make_dsfb\\"
deploys = c("oo23_292b", "oo23_295a", "oo23_295b", "oo23_299a","oo23_299b", "oo23_301a", "oo23_302a")
res = 1.5

#make data 
for(i in seq_along(deploys)){
  track = R.matlab::readMat(paste(dsfb_dir,deploys[i],"_pt_dsfb.mat", sep = ""))
  wtrack = track[["wtrack"]][[3]]
  time = track[["wtrack"]][[7]]
  time = time[seq(1,length(time),by = 10)]
  ves_dist = (track[["wtrack"]][[8]])
  ves_dist[is.na(ves_dist)] = Inf
  near_col = apply(ves_dist, 1, which.min)
  ves_lat = track[["wtrack"]][[10]]
  ves_lon = track[["wtrack"]][[9]]
  near_dist = sapply(1:length(near_col),function(i){ves_dist[i,near_col[i]]})
  near_lat = sapply(1:length(near_col),function(i){ves_lat[i,near_col[i]]})
  near_lon =  sapply(1:length(near_col),function(i){ves_lon[i,near_col[i]]})
  temp = data.frame(wtrack,near_dist,near_lat,near_lon,time)
  temp$deploy = deploys[i]
  temp$id = i
  #downsample
  index = 1:nrow(temp)
  temp_ds = temp[(index-1)%%(res*6)==0,]
  if(i==1){
    mtab = temp_ds
  }
  else {
    mtab = rbind(mtab,temp_ds)
  }
}

names(mtab) = c("wlat","wlong", "boat_dist", "boat_lat","boat_lon", "time", "deploy", "ID")
mtab$time = as.POSIXct((mtab$time-719529)*86400, origin = "1970-01-01", tz = "UTC")
head(mtab)
summary(mtab)
mtab = mtab[!is.na(mtab$boat_lat),]


# study site is polar so lets use an appropriate projection
library(sp)
#reproject whale
oldProj <- CRS("+proj=longlat +datum=WGS84")
newProj <- CRS("+init=epsg:27700")
coordinates(mtab) <- c("wlong","wlat")
proj4string(mtab) <- oldProj
mtab <- as.data.frame(spTransform(mtab, newProj))
#and nearest vessel
coordinates(mtab) <- c("boat_lon","boat_lat")
proj4string(mtab) <- oldProj
mtab <- as.data.frame(spTransform(mtab, newProj))

boat_data = list(boat=data.frame(time = mtab$time,
                                 x = mtab$boat_lon,
                                 y = mtab$boat_lat))

fit_data = prepData(mtab, coordNames = c("wlong", "wlat"),
                    centroids = boat_data)

fit_data$d = fit_data$boat_dist

dist <- list(step = "gamma",
             angle = "vm",
             d = "gamma")

plot(fit_data, ask = FALSE)

# starting values 

# 2 state model
stepPar0 = c(100,10,20,10) # (mu_1,mu_2,sd_1,sd_2...)
anglePar0 = c(15,1) #
statenames = c("travel", "ARS")

fit0 = fitHMM(data = fit_data, nbStates = 2,
             stateNames = statenames,
             dist = list(step = "gamma", angle = "vm"),
             Par0 = list(step = stepPar0, angle = anglePar0),
             estAngleMean = list(angle=FALSE))
fit0
plot(fit0)


# 4 states no covariates
statenames = c("sea_travel", "boat_travel", "sea_ARS", "boat_ARS")
n_states = 4
stepPar0 <- c(100,190,70,40, 20,40,20,30) # (mu_1,mu_2,sd_1,sd_2...)
anglePar0 <- c(10,50,3,1) #

fixbeta <- matrix(c(NA,NA,-100,NA,-100,NA,NA,NA,NA,NA,NA,NA),nrow=1,byrow=TRUE)

fit1 = fitHMM(data = fit_data, nbStates = n_states,
             stateNames = statenames,
             dist = list(step = "gamma", angle = "vm"),
             Par0 = list(step = stepPar0, angle = anglePar0),
             fixPar=list(beta=fixbeta),
             estAngleMean = list(angle=FALSE))
fit1
plot(fit1)



# 4 states with fishing vessel attraction & transition 
n_states = 4
angle_formula = ~ + state4(boat.angle)
formula = ~ betaCol6(boat_dist) # change to "tostate" formulation like in the fulmars example?


Par0 <- getPar0(model=fit1, nbStates=4,
                DM=list(angle=list(mean=angle_formula, concentration=~1)),
                estAngleMean=list(angle=TRUE),
                circularAngleMean=list(angle=0),formula = formula)


fixbeta <- matrix(c(NA,NA,-100,NA,-100,NA,NA,NA,NA,NA,NA,NA,
                    rep(NA,12)),nrow=2,byrow=TRUE)
fixPar <- list(angle=c(1,NA,NA,NA,NA), beta = fixbeta)


fit2 = fitHMM(data = fit_data, nbStates = n_states,
             dist = list(step = "gamma", angle = "vm"),
             Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle),
             beta0=Par0$beta,
             fixPar=fixPar,
             formula=formula,
             DM=list(angle=list(mean=angle_formula, concentration=~1)),
             estAngleMean=list(angle=TRUE), circularAngleMean=list(angle=0),
             stateNames = statenames)
fit2
plot(fit2)



