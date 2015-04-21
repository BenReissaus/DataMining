"Authors: Tommy Neubert(), Benjamin Reissaus(749897)"


"Assignment 1a"
x<-c(0.03, 0.04, 0.05, 0.49, 0.5, 0.59, 0.66, 0.72, 0.83, 1.17)
mean(x)
median(x)
var(x)


"Assignment 1b"
random_numbers = runif(100,0,2)
mean(random_numbers) 
median(random_numbers) 
var(random_numbers)

"Assignment 1c"

random_numbers = runif(10000, 0, 1)
dev.new()
#png(filename="Assignment_1c.png")
hist(random_numbers, breaks=20, main="Assignment 1c")
#dev.off()

"Assignment 2"
min(faithful$eruptions) 
max(faithful$eruptions) 
median_val <- median(faithful$eruptions) 
median_val
mean_val <- mean(faithful$eruptions) 
mean_val
median_val - mean_val

min(faithful$waiting)
max(faithful$waiting)
median_val <- median(faithful$waiting)
median_val
mean_val <- mean(faithful$waiting)
mean_val
median_val - mean_val

"Assignment 3"
iris_data <- read.csv("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/iris.data", header=T, sep=",",stringsAsFactors=F)	

"Assignment 3a"
uniques=unique(iris_data$Species)
"Unique Values"
for (n in 1:length(uniques)){ 
	print(paste(uniques[n], sum(iris_data$Species == uniques[n]), sep=" : "))
}  

"Assignment 3b"
dev.new()
#png(filename="Assignment_3b.png")
plot(iris_data$Sepal.length, iris_data$Petal.length, xlab="Sepal Length", ylab="Petal Length", main="Assignment 3b")
#dev.off()

"Assignment 3c"
iris_setosa <- iris_data[iris_data$Species == "Iris-setosa", ]
dev.new()
#png(filename="Assignment_3c.png")
plot(iris_setosa$Sepal.length, iris_setosa$Petal.length, xlab="Iris Setosa Sepal Length", ylab="Iris Setosa Petal Length", main="Assignment 3c")
#dev.off()

"Assignment 3d"
dev.new()
#png(filename="Assignment_3d.png")
plot(iris_setosa$Sepal.length, iris_setosa$Petal.length, col="red", xlab="Iris Setosa Sepal Length", xlim=c(3,10), ylab="Iris Setosa Petal Length", ylim=c(0,10), main="Assignment 3d")

iris_rest <- iris_data[iris_data$Species != "Iris-setosa", ]
points(iris_rest$Sepal.length, iris_rest$Petal.length, col="blue")
#dev.off()

"Assignment 4a"
data_points = data.frame(matrix(runif(50*100), 50, 100))
correlations = cor(data_points)
dev.new()
#png(filename="Assignment_4a.png")
hist(correlations,  main="Assignment 4a")
#dev.off()

"Assignment 4b"
dev.new()
#png(filename="Assignment_4b.png")
hist(correlations, breaks=100, xlim=c(-0.6,+0.6), col="blue", main="Assignment 4b")
#dev.off()

"Assignment 5: Regression"

x_age = c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
y_height = c(76.2, 77.1, 78.1, 78.4, 78.8, 79.7, 79.7, 81.1, 81.2, 81.4, 82.8, 83.5)
fm <- lm(y_height ~ x_age)
dev.new()
#png(filename="Assignment_5.png")
plot(x_age, y_height, xlim=c(0,30), ylim=c(0,90), main="Assignment 5")
abline(fm, col = "red")
#dev.off()



"Bonus Assignment 1: ROC Curves"
library(ROCR)
classes = c(1,1,0,1,1,1,1,1,0,1,0,1,1,0,1,0,1,0,1)
predicted<-c(0.95,0.93,0.93,0.88,0.86,0.85,0.82,0.8,0.8,0.79,0.77,0.76,0.73,0.65,0.63,0.58,0.56,0.49,0.48)
pred<-prediction(predicted, classes)
perf <- performance(pred, "tpr", "fpr")
dev.new()
#png(filename="BonusAssignment1.png")
plot(perf, main="Bonus Assignment 1")
#dev.off()





