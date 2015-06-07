setwd("D:/Studium/Datamining/DataMining/Exercise4/")

library(package = "NbClust")

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
  centroidId = which(row.names(centroids) == centroidName)

  return(centroidId)
}

calcEvalMeasures <- function(distMeasure, clusteredData, centroids){

  clusterGroups = split(clusteredData, clusteredData[["cluster"]])  
  
  for(clusterGroup in clusterGroups){
    
    clusterId = clusterGroup[1,"cluster"]
    centroid = centroids[clusterId, ! colnames(centroids) %in% c("cluster", "avgDistance", "sumSquaredDistance")]
    
    distances = apply(clusterGroup, 1, function(x) distMeasure(centroid, x))
    squaredDistances = distances^2
    sumDistance = sum(distances)

    centroids[clusterId, "sumSquaredDistance"] = sum(squaredDistances)
    centroids[clusterId, "avgDistance"] = sumDistance/nrow(clusterGroup) 
  }
  return(centroids)
}



kmeans <- function(k, maxIterations, distMeasure, data){
  
  # set default cluster
  data[, "cluster"] = -1
  clusteredData = data
  
  # choose k centroids randomly
#   centroids = data[sample(nrow(data), k), ]
  centroids = data[1:k, ]
  
  
  iterations = 0
  while(TRUE){
    iterations = iterations + 1
    print(paste(c("iterations: ", iterations), collapse = " "))

    clusteredData[, "cluster"] = apply(data, 1, function(x) getClosestClusterId(distMeasure, x, centroids))
    
    # cluster elements have not changed or number of max iterations was reached
    if(all(clusteredData[, "cluster"] == data[, "cluster"]) || iterations == maxIterations){    
      centroids = calcEvalMeasures(distMeasure, clusteredData, centroids)
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

# prints infos about centroids with sum of squared distances and average distance
algoResults@centroids



# 5

whiteWineWOClass <- whiteWineData[, names(whiteWineData) != "class"]
  
k <- c(2:10)
kMeansResults <- list()
kMediansResults <- list()

for(currentK in k) {
  cat("current k: ")
  cat(currentK)
  kMeansResults[currentK] <- kmeans(k=currentK, maxIterations=100, distMeasure=euclideanDistance, data=whiteWineWOClass)
  kMediansResults[currentK] <- kmeans(k=currentK, maxIterations=100, distMeasure=manhattanDistance, data=whiteWineWOClass)
}

sumsMeans <- c(0,0,0,0,0,0,0,0,0)
sumsMedians <- c(0,0,0,0,0,0,0,0,0)
for(currentK in k) {
  sumsMeans[currentK-1] <- sum(kMeansResults[[currentK]]@centroids$sumSquaredDistance)
  sumsMedians[currentK-1] <- sum(kMediansResults[[currentK]]@centroids$sumSquaredDistance)
}
plot(c(2:10), sumsMeans, main="k-Means Plot",xlab="k",ylab="sum of squared distances")
plot(c(2:10), sumsMedians, main="k-Medians Plot",xlab="k",ylab="sum of squared distances")

print("We would choose k = 10 for both methods because the plots show that this k minimizes the squared distances in the clusters. This means that the elements within a cluster are much closer together.")

indexes <- list("kl","ch","hartigan","ccc","scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap","frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
results <- list()
for(index in indexes) {
  result <- NbClust(data=whiteWineWOClass,diss=NULL,distance="euclidean",min.nc=1,max.nc=,method="kmeans",index=index,alphaBeale=0.1)
  results[index] <- result
}
counts <- c()
for(i in length(results)) {
  counts[i] <- 0
}
for(name in names(results)) {
  counts[results[[name]]] <- counts[results[[name]]] + 1
}
counts

#7 print distances between nodes

a = c(1,1,1)
b = c(1,3,3)
c = c(2,4,5)
d = c(5,1,1)
e = c(8,2,1)
f = c(6,1,2)
g = c(5,3,2)

elements = list(a=a,b=b,c=c,d=d,e=e,f=f,g=g)
numberElements = length(elements)

for(i in 1:numberElements){

  if(i == numberElements) { 
    break 
  }
  beg = i+1
  end = length(elements)
  sequence = beg:end
  for(j in sequence){
    print(paste(c(names(elements)[[i]], "-", names(elements)[[j]], " distance: ", manhattanDistance(elements[[i]], elements[[j]])), collapse = " "))  
  }
}



