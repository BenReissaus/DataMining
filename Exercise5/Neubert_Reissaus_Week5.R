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
  
#   print(paste(c("total bets: ", totalBets, "highest bet: ", highestBet, " highest loss: ", biggestLoss), collapse = " "))
#   hist(winningBetSizes, xlim=c(0,3000), xlab="occurrences", ylab="Winning Bet Size", main="Winning Bet Sizes")
  
  library(ggplot2)
  dfr <- data.frame(x = winningBetSizes)
  ggplot(dfr, aes(x)) + geom_histogram() + scale_x_continuous(trans="log2") + xlab("Winning Bet Size") + ylab("Count") + ggtitle("Winning Bet Sizes Histogram")
}

bet()
