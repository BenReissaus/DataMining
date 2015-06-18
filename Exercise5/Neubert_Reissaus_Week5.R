setwd("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/Code/Exercise5/")


# 4 f

bet <- function(){
  
  # create probability distribution
  
  # set money to be 1€
  betMoney = 1
  wonMoney = 0
  throws = 0
  highestBet = 0
  
  winningBetSizes = c()
  while(wonMoney != 3000){
    
    # put money in the middle
    wonMoney = wonMoney - betMoney
    
    # throw ball 
    fieldColor = sample(c("red","black","green"), 1, prob=c(18/37,18/37, 1/37), replace = T)
    throws = throws + 1
    
    if(fieldColor == "red"){
      print(paste(c("fieldColor: ", fieldColor), collapse = " "))
      
      winningBetSizes = c(winningBetSizes, betMoney)
      wonMoney = wonMoney + (betMoney * 2)
      
      if(betMoney > highestBet) { highestBet = betMoney}
      betMoney = 1
    }
    else {
      betMoney = betMoney * 2
    }
    print(paste(c("won money: ", wonMoney), collapse = " "))
  }
  print("winning bet sizes")
  print(winningBetSizes)
  print(paste(c("throws: ", throws, "highestBet: ", highestBet), collapse = " "))
}

bet()
