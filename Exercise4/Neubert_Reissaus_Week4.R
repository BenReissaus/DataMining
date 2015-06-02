setwd("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/Code/Exercise4")


# 3

par(mfrow=c(2,3))

subMtcars = mtcars[, c("mpg", "disp","hp", "wt", "drat" )]

plot(x=subMtcars$disp, y=subMtcars$hp, xlab="displacement", ylab="horsepower", main="Mtcars: Displacement - Horsepower")
plot(x=subMtcars$disp, y=subMtcars$wt, xlab="displacement", ylab="weight", main="Mtcars: Displacement - Weight")
plot(x=subMtcars$disp, y=subMtcars$drat, xlab="displacement", ylab="rear axle ration", main="Mtcars: Displacement - Rear Axle Ratio")
plot(x=subMtcars$hp, y=subMtcars$wt, xlab="horsepower", ylab="weight", main="Mtcars: Horsepower - Weight")
plot(x=subMtcars$hp, y=subMtcars$drat, xlab="horsepower", ylab="rear axle ration", main="Mtcars: Horsepower - Rear Axle Ratio")
plot(x=subMtcars$wt, y=subMtcars$drat, xlab="weight", ylab="rear axle ration", main="Mtcars: Weight - Rear Axle Ratio")

disp = subMtcars$disp
hp = subMtcars$hp
drat = subMtcars$drat
wt = subMtcars$wt
mpg = subMtcars$mpg

fm <- lm(mpg ~ disp + hp + drat + wt, )
summary(fm)

newData = data.frame(disp=230, hp=146, wt=3.2, drat=3.6)

predict(fm, newData)

# 4
whiteWineData <- read.csv("winequality-white.csv", header=T, sep=",",stringsAsFactors=F)  

# all columns except quality
kmeansData <- whiteWineData[, names(whiteWineData) != "quality"]

euclideanDistance <- function(e1, e2){
  return(sqrt(sum((e1- e2)^2)))
}

manhattanDistance <- function(e1, e2){
  return(sum(abs(e1-e2)))
}

AlgoResults <- setClass( 
  "AlgoResults",
  slots = c(
    iterations = "numeric",
    centroids = "data.frame",
    clusteredData = "data.frame"
  )
)

getClosestClusterId <- function(distMeasure,element, centroids){
  
  distances = apply(centroids, 1, function(x) distMeasure(element, x))
  
  # sort distances to centroids and get name of smallest one
  centroidName = names(sort(distances))[[1]]
  centroidIndex = which(row.names(centroids) == centroidName)

  return(centroidIndex)
}

calculateAvgDistance <- function(distMeasure, clusteredData, centroids){

  clusterGroups = split(clusteredData, clusteredData[["cluster"]])  
  
  for(clusterGroup in clusterGroups){
    
    clusterId = clusterGroup[1,"cluster"]
    centroid = centroids[clusterId, ! colnames(centroids) %in% c("cluster", "avgDistance")]
    
    distances = apply(clusterGroup, 1, function(x) distMeasure(centroid, x))
    sumDistance = sum(distances)

    centroids[clusterId, "avgDistance"] = sumDistance/nrow(clusterGroup) 
  }
  return(centroids)
}

kmeans <- function(k, maxIterations, distMeasure, data){
  
  # set default cluster
  data[, "cluster"] = -1
  clusteredData = data
  
  # choose k centroids randomly
  centroids = data[sample(nrow(data), k), ]
  
  iterations = 0
  while(TRUE){
    iterations = iterations + 1
    print(paste(c("iterations: ", iterations), collapse = " "))

    clusteredData[, "cluster"] = apply(data, 1, function(x) getClosestClusterId(distMeasure, x, centroids))
    
    # cluster elements have not changed or number of max iterations was reached
    if(all(clusteredData[, "cluster"] == data[, "cluster"]) || iterations == maxIterations){    
      centroids = calculateAvgDistance(distMeasure, clusteredData, centroids)
      algoResults = AlgoResults(iterations = iterations, centroids = centroids, clusteredData = clusteredData)
      return(algoResults)
    }
  
    #update centroids
    clusterGroups = split(clusteredData, clusteredData[["cluster"]])  
    for(clusterGroup in clusterGroups){
      clusterId = clusterGroup[["cluster"]][[1]]
      centroids[clusterId, ] = colMeans(clusterGroup)
    }
    data = clusteredData
  }
}

algoResults = kmeans(k=7, maxIterations=100, distMeasure=euclideanDistance, data=kmeansData)







