setwd("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/Code/Exercise5/")


# 4 f
bet <- function(){
    
  # set money to be 1€
  betMoney = 1
  wonMoney = 0
  totalBets = 0
  highestBet = 0
  biggestLoss = 0
  
  winningBetSizes = c()
  while(wonMoney != 3000){
    
    # put money in the middle
    wonMoney = wonMoney - betMoney
    if(wonMoney < biggestLoss){ biggestLoss = wonMoney}
    
    # throw ball 
    fieldColor = sample(c("red","black","green"), 1, prob=c(18/37,18/37, 1/37), replace = T)
    totalBets = totalBets + 1
    
    if(fieldColor == "red"){
      
      winningBetSizes = c(winningBetSizes, betMoney)
      wonMoney = wonMoney + (betMoney * 2)
      
      if(betMoney > highestBet) { highestBet = betMoney}
      betMoney = 1
    }
    else {
      betMoney = betMoney * 2
    }
  }
  
  print(paste(c("total bets: ", totalBets, "highest bet: ", highestBet, " highest loss: ", biggestLoss), collapse = " "))
  #hist(winningBetSizes, xlim=c(0,3000), xlab="occurrences", ylab="Winning Bet Size", main="Winning Bet Sizes")
  #plot(winningBetSizes, log="x", type='h',lend=2, lwd=10 )
  library(ggplot2)
  dfr <- data.frame(x = winningBetSizes)
  ggplot(dfr, aes(x)) + geom_histogram() + scale_x_continuous(trans="log2") + xlab("Winning Bet Size") + ylab("Count") + ggtitle("Winning Bet Sizes Histogram")
}

bet()

# Assignment 7

irisData <- read.csv("../../iris.data", header=T, sep=",",stringsAsFactors=F)  

meansPerClassPerFeature = aggregate(irisData[,c(1,3)], iris["Species"], function(x) mean(x))

setosaMeans = data.matrix(meansPerClassPerFeature[1,c(2,3)])
versicolorMeans = data.matrix(meansPerClassPerFeature[2,c(2,3)])
virginicaMeans = data.matrix(meansPerClassPerFeature[3,c(2,3)])

setosa = data.matrix(irisData[irisData$Species == "Iris-setosa",c(1,3)])
versicolor = data.matrix(irisData[irisData$Species == "Iris-versicolor",c(1,3)])
virginica = data.matrix(irisData[irisData$Species == "Iris-virginica",c(1,3)])

# training model
oneVector = matrix(rep(1,50))

setosaRepeatedMeans = oneVector %*% setosaMeans 
versicolorRepeatedMeans = oneVector %*% versicolorMeans 
virginicaRepeatedMeans = oneVector %*% virginicaMeans 

setosaDiffToMeanValues = setosa - setosaRepeatedMeans
versicolorDiffToMeanValues = versicolor - versicolorRepeatedMeans
virginicaDiffToMeanValues = virginica - virginicaRepeatedMeans

setosaScatter = t(setosaDiffToMeanValues) %*% setosaDiffToMeanValues
versicolorScatter = t(versicolorDiffToMeanValues) %*% versicolorDiffToMeanValues
virginicaScatter = t(virginicaDiffToMeanValues) %*% virginicaDiffToMeanValues

setosaCovariance = setosaScatter/50
versicolorCovariance = versicolorScatter/50
virginicaCovariance = virginicaScatter/50

qda <- function(dataInstance, mean, priorProbability, covarianceMatrix){  
  return(-0.5 * log(det(covarianceMatrix)) - 0.5*(dataInstance-mean) %*% solve(covarianceMatrix) %*% t(dataInstance-mean) + log(priorProbability)) 
}

# find best class 
# not finished!!!
data1 = matrix(c(4.5, 2), nrow=1, ncol=2,byrow=FALSE)

qda(data1, setosaMeans, 1/3, setosaCovariance)
qda(data1, setosaMeans, 1/3, versicolorCovariance)
qda(data1, setosaMeans, 1/3, virginicaCovariance)

# some likelihood ratio now????


#Assignment 8

library(MASS)
plot(x=menarche$Age, y=menarche$Menarche/menarche$Total,xlab="Age", ylab="Menarche / Total", main="Femal Menarche")
prob_distr = plogis(8:19, location = 13, scale=1)
n = 8:19
lines(n,prob_distr, lwd=3, col="dark red")

glm.out <- glm(cbind(Menarche, Total-Menarche) ~ Age, family="binomial", data=menarche)
plot(Menarche/Total ~ Age, data=menarche, main="Female Menarche")
lines(menarche$Age, glm.out$fitted, type="l", col="red")

