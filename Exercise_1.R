
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
hist(random_numbers, breaks=20)


"Assignment 2"
TODO

min(faithful$eruptions) 
max(faithful$eruptions) 
median(faithful$eruptions) 
mean(faithful$eruptions) 
"difference between mean and median: 0.512"

min(faithful$waiting)
max(faithful$waiting)
median(faithful$waiting)
mean(faithful$waiting)
"difference between mean and median: 5,10294"


"Assignment 3"

iris_data <- read.csv("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/iris.data", header=T, sep=",")

"Assignment 3a"

TODO
"Iris-setosa    :50 " 
"Iris-versicolor:50"
"Iris-virginica :50"  

"Assignment 3b"
dev.new()
plot(iris_data$Sepal.length, iris_data$Petal.length, xlab="Sepal Length", ylab="Petal Length", main="Iris Data Petal & Sepal Lenth")

"Inference: 
The petal length and the sepal length seem to be correlated positively. The higher the petal length, the higher also the sepal length"

"Assignment 3c"
iris_setosa <- iris_data[iris_data$Species == "Iris-setosa", ]
dev.new()
plot(iris_setosa$Sepal.length, iris_setosa$Petal.length, xlab="Iris Setosa Sepal Length", ylab="Iris Setosa Petal Length", main="Iris Setosa Petal & Sepal Lenth")

iris_virginica <- iris_data[iris_data$Species == "Iris-virginica", ]
dev.new()
plot(iris_virginica$Sepal.length, iris_virginica$Petal.length, xlab="Iris Virginica Sepal Length", ylab="Iris Virginica Petal Length", main="Iris Virginica Petal & Sepal Lenth")

iris_versicolor <- iris_data[iris_data$Species == "Iris-versicolor", ]
dev.new()
plot(iris_versicolor$Sepal.length, iris_versicolor$Petal.length, xlab="Iris Versicolor Sepal Length", ylab="Iris Versicolor Petal Length", main="Iris Versicolor Petal & Sepal Lenth")

"The Petal Length and Sepal Length of Iris Virginica and Versicolor are also correlated positively.  
The Petal Length and Seapal Length of Iris-Setosa does not seem to be correlated. "

"Assignment 3d"
dev.new()
plot(iris_setosa$Sepal.length, iris_setosa$Petal.length, col="red", xlab="Iris Setosa Sepal Length", xlim=c(3,10), ylab="Iris Setosa Petal Length", ylim=c(0,10), main="Iris Setosa Petal & Sepal Lenth")

iris_rest <- iris_data[iris_data$Species != "Iris-setosa", ]
points(iris_rest$Sepal.length, iris_rest$Petal.length, col="blue")


"Assignment 4a"
data_points = data.frame(matrix(runif(50*100), 50, 100))
correlations = cor(data_points)
dev.new()
hist(correlations)

"Assignment 4b"
dev.new()
hist(correlations, breaks=20, xlim=c(-0.6,+0.6), col="blue")

"Assignment 5: Regression"

x_age = c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
y_height = c(76.2, 77.1, 78.1, 78.4, 78.8, 79.7, 79.7, 81.1, 81.2, 81.4, 82.8, 83.5)
fm <- lm(y_height ~ x_age)
dev.new()
plot(x_age, y_height, xlim=c(0,30), ylim=c(0,90))
abline(fm, col = "red")
fitted.values(fm)


"Assignment 6: Performance Measures I"
TODO 

"High accuracy: 
- Cancer Detection --> People who do not have cancer should not be treated with chemo therapy(high precision) and all people who have cancer should be classified as having cancer (the earlier the cancer is found and treated the better)."


"Assignment 7: Performance Measures II

maximum precision: score >= 0.95
maximum recall: score >= 0.48
maximum accuracy: score >= 0.95"

"Bonus Assignment 1: ROC Curves"

library(ROCR)
classes = c(1,1,0,1,1,1,1,1,0,1,0,1,1,0,1,0,1,0,1)
predicted<-c(0.95,0.93,0.93,0.88,0.86,0.85,0.82,0.8,0.8,0.79,0.77,0.76,0.73,0.65,0.63,0.58,0.56,0.49,0.48)
pred<-prediction(predicted, classes)
perf <- performance(pred, "tpr", "fpr")
dev.new()
plot(perf)





