# Neubert Reissaus - Week 7

library("neuralnet")

# Assignment 5
traininginput <-  as.data.frame(c(1:16))
trainingoutput <- traininginput

trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

nn <- neuralnet(Output~Input,trainingdata)
print(nn)

plot(nn)

testdata <- as.data.frame(c(1:32))
results <- compute(nn, testdata)
results
# results show: neural network performs well in the range of the trained data and becomes more worse out of that range
nn <- neuralnet(Output~Input,trainingdata,hidden=4)
print(nn)

plot(nn)

testdata <- as.data.frame(c(1:32))
results <- compute(nn, testdata)
results
# the difference to the desired value increases the higher the hidden value is
nn <- neuralnet(Output~Input,trainingdata,threshold=0.01)
print(nn)

plot(nn)

testdata <- as.data.frame(c(1:32))
results <- compute(nn, testdata)
results
# playing around with the threshold doesnt seem to change much
# Assignment 6
fert <- swiss$Fertility
agri <- swiss$Agriculture
exam <- swiss$Examination
educ <- swiss$Education
cath <- swiss$Catholic
mort <- swiss$Infant.Mortality

# plot all
plot(swiss)

# check each direct dependence with linear regression
fm <- lm(mort ~ fert)
plot(fert,mort)
abline(fm,col = "red")
# => if fertility goes up, mortality increases

fm <- lm(mort ~ agri)
plot(agri,mort)
abline(fm,col = "red")
# not correlated or maybe: if agriculture increases, mortality slightly decreases

fm <- lm(mort ~ exam)
plot(exam,mort)
abline(fm,col = "red")
# => if examination increases, mortality decreases

fm <- lm(mort ~ educ)
plot(educ,mort)
abline(fm,col = "red")
# not correlated or maybe: if education increases, mortality slightly decreases

fm <- lm(mort ~ cath)
plot(cath,mort)
abline(fm,col = "red")
# => if catholics increase, mortality increases too


# Assignment 7
setwd('D:/Studium/Datamining/Uebung7/')
data <- read.csv("titanic.csv",header=TRUE,sep=",")
survived <- data[data$Survived==1,]
dead <- data[data$Survived==0,]

# how many passengers of each passenger class have survived
a <- survived[survived$Pclass==3,]
b <- dead[dead$Pclass==3,]
ratio <- length(a$PassengerId) / length(b$PassengerId)
ratio

a <- survived[survived$Pclass==2,]
b <- dead[dead$Pclass==2,]
ratio <- length(a$PassengerId) / length(b$PassengerId)
ratio

a <- survived[survived$Pclass==1,]
b <- dead[dead$Pclass==1,]
ratio <- length(a$PassengerId) / length(b$PassengerId)
ratio

# how many womans and man have survived
womanS <- survived[survived$Sex=="female",]
womanD <- dead[dead$Sex=="female",]
ratio <- length(womanS$PassengerId) / length(womanD$PassengerId)
ratio

manS <- survived[survived$Sex=="male",]
manD <- dead[dead$Sex=="male",]
ratio <- length(manS$PassengerId) / length(manD$PassengerId)
ratio

womanManRatio <- length(womanS$PassengerId) / length(manS$PassengerId)
womanManRatio

# how many children survived
childsS <- survived[survived$Age<18,]
childsD <- dead[dead$Age<18,]
ratio <- length(childsS$PassengerId) / length(childsD$PassengerId)
ratio
