
# Assignment 7 

gaussianData <- read.table("gaussians.txt")  
statistics = summary(gaussianData)
numberRows = nrow(gaussianData)


require(pastecs)
par(mfrow=c(2,3))
for(i in 1:5){
  
  densityResult <- density(gaussianData[,i], bw = "sj")
  
#   # ATTEMPT TO FIND MEANS
#   #make it a time series
#   timeSeries_y<-ts(densityResult$y) 
#   # get maxima and minima
#   densityTurnPoints = turnpoints(timeSeries_y)
#   means = densityResult$x[densityTurnPoints$peaks]
  
  gaussmix = normalmixEM(gaussianData[,i], k=3)

  plot(gaussmix, which=2)
  lines(d, lty=2, lwd=2)
  print(gaussmix[c("lambda", "mu", "sigma")])
}
