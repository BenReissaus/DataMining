"Neubert, Reissaus - Assigment 7 - Week 6"
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

"My solution"

setwd("D:/Studium/Datamining/DataMining/Exercise6/")

library(mixtools)

estimateAndPlot <- function(data, nrOfComponents) {
  result <- normalmixEM(data,k = nrOfComponents)
  plot(result,which=2)
  return(result)
}

"Step 1: Read the data and devide it"
"We assume that each column of the table represents a different sampling run. Therefore each column can be treated individually."

gaussians <- read.table("gaussians.txt")

sample1 <- gaussians$V1
sample2 <- gaussians$V2
sample3 <- gaussians$V3
sample4 <- gaussians$V4
sample5 <- gaussians$V5

"Step 2: calculate the distributions and plot the result (for different component numbers)"
result12 <- estimateAndPlot(sample1,2)
result13 <- estimateAndPlot(sample1,3)
result14 <- estimateAndPlot(sample1,4)
"above plots show that the mixed distribution consists of 3 components"

result2 <- estimateAndPlot(sample2,3)
result3 <- estimateAndPlot(sample3,3)
result4 <- estimateAndPlot(sample4,3)
result5 <- estimateAndPlot(sample5,3)
"Result: As we can see in the 5 plots:  the data of all 5 columns seem to be pulled from the same mixture model. The small differences can be explained with the underlying distributions. Because the data is only a sample, each sampling run yields slightly different values."

"Step 3: Estimate the mean and standard deviation of the underlying normal distributions"
mean1 <- result13$mu
mean2 <- result2$mu
mean3 <- result3$mu
mean4 <- result4$mu
mean5 <- result5$mu
stdD1 <- result13$sigma
stdD2 <- result2$sigma
stdD3 <- result3$sigma
stdD4 <- result4$sigma
stdD5 <- result5$sigma
"From this results we can read the mean and standard deviation of the underlying normal distributions"
means <- c(80.1,200,6,301.1)
stdDev <- c(49.8,15.9,39.9)