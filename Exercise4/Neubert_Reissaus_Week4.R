setwd("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/Code/Exercise4")


# 3

# par(mfrow=c(2,3))
# 
# subMtcars = mtcars[, c("mpg", "disp","hp", "wt", "drat" )]
# 
# plot(x=subMtcars$disp, y=subMtcars$hp, xlab="displacement", ylab="horsepower", main="Mtcars: Displacement - Horsepower")
# plot(x=subMtcars$disp, y=subMtcars$wt, xlab="displacement", ylab="weight", main="Mtcars: Displacement - Weight")
# plot(x=subMtcars$disp, y=subMtcars$drat, xlab="displacement", ylab="rear axle ration", main="Mtcars: Displacement - Rear Axle Ratio")
# plot(x=subMtcars$hp, y=subMtcars$wt, xlab="horsepower", ylab="weight", main="Mtcars: Horsepower - Weight")
# plot(x=subMtcars$hp, y=subMtcars$drat, xlab="horsepower", ylab="rear axle ration", main="Mtcars: Horsepower - Rear Axle Ratio")
# plot(x=subMtcars$wt, y=subMtcars$drat, xlab="weight", ylab="rear axle ration", main="Mtcars: Weight - Rear Axle Ratio")
# 
# 
# fm <- lm(subMtcars$mpg ~ subMtcars$disp + subMtcars$hp + subMtcars$drat + subMtcars$wt)
# summary(fm)

# 4
whiteWineData <- read.csv("winequality-white.csv", header=T, sep=",",stringsAsFactors=F)  

# all columns except quality
kmeansData <- whiteWineData[, names(whiteWineData) != "quality"]

euclideanDistance <- function(e1, e2){
  return(sqrt(sum((e1- e2)^2)))
}

kmeans <- function(k, data){
  data[, "cluster"] = -1
  clusteredData = data
  
  # choose k centroids randomly
  centroids = data[sample(nrow(data), k), ]
  
  iterations = 0
  while(TRUE){
    iterations = iterations + 1
    print(paste(c("iterations: ", iterations), collapse = " "))
       
    for(i in 1:nrow(data)){
      cluster = 0
      
      # set min distance to max possible value
      minDistance = .Machine$integer.max
      for(centroidIdx in 1:nrow(centroids)){
        distance = euclideanDistance(data[i,], centroids[centroidIdx, ])
        if(distance < minDistance){
          minDistance = distance
          cluster = centroidIdx
        }
      }
      clusteredData[i, "cluster"] = cluster
    }
    
    # clusters have not changed
    if(all(clusteredData[, "cluster"] == data[, "cluster"])){
      
      print("Centroids")
      print(centroids)
      print(paste(c("iterations: ", iterations), collapse= " "))
      
      return(clusteredData)
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

cluster = kmeans(7, kmeansData[1:200,])






