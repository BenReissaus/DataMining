setwd("/Users/Benji/Documents/Uni/Master/3.Semester/Data_Mining_Probabilistic_Reasoning/Exercises/Exercise3/")

# 1

Impurity <- function(categoryCounts, totalInstanceCount){
  
  # categoryCounts for textiles 
  #       few:    (3,2)
  #       medium: (2,2,2)
  #       many:   (2,2,2)

  weightedEntropy = 0
  for(categoryCountsPerValue in categoryCounts){
    
    # sum up category counts for one value e.g. "few"
    featureInstancesCount = Reduce("+", categoryCountsPerValue)
    
    # calculate weighted entropy over all feature values
    entropy = 0 
    for (categoryCount in categoryCountsPerValue){
      if(categoryCount != 0){
        relativeCount = categoryCount/featureInstancesCount
        entropy = entropy - (relativeCount) * log2(relativeCount)   
      }     
    }
    weightedEntropy = weightedEntropy + (featureInstancesCount/totalInstanceCount) * entropy   
  } 
  return(weightedEntropy)
}


BestSplit <- function(dataInstances, features, categories, classifierAttribute){

  I_min = log2(length(categories)) 
  splitFeature = NULL
  
  for(feature in features){
   
    # group by feature values
    if(class(dataInstances[[feature]]) == "character"){
      groups = split(dataInstances, f = dataInstances[[feature]])   
      
    } else if(class(dataInstances[[feature]]) == "numeric"){   
      
      # do not attempt to split if there is only one value in that column left
      if(length(unique(dataInstances[[feature]])) == 1){
        next
      }
      groups = split(dataInstances, cut(dataInstances[[feature]], breaks=2,  dig.lab = 20))
      
    } else {
      print("Other data type")
    }
    
    categoryCounts = list()
    for( group in groups){
      
      # group by categories
      categoryGroups = split(group, f = group[[classifierAttribute]])
      
      categoryCount = list()
      for(categoryGroup in categoryGroups){
        categoryCount = c(categoryCount, nrow(categoryGroup))
      }
      categoryCounts = c(categoryCounts, list(categoryCount))
    }

    totalInstanceCount = nrow(dataInstances)
    imp = Impurity(categoryCounts, totalInstanceCount)  

    if(imp < I_min){
      I_min = imp
      splitFeature = feature
    }  
  }

  return(splitFeature)
}

Homogeneous <- function(node, classifierAttribute){
  
  uniques=unique(c(node@dataInstances[, classifierAttribute]))  
  return(length(uniques) == 1)
}

CategoryLabel <- function(node, classifierAttribute){

  # return the most often ocurring category as label
  node@label = names(which.max(table(node@dataInstances[[classifierAttribute]])))

  return(node)
}


SplitByFeature <- function(node, splitFeature){
  
  dataInstances = node@dataInstances  
  dataSubsetNodes = list()
  
  if(class(dataInstances[[splitFeature]]) == "character"){
  
    groups = split(dataInstances, f = dataInstances[[splitFeature]])   
    for( group in groups){
      
      groupValue = group[[splitFeature]][[1]]
      newNode = CharacterNode(dataInstances=group, feature=splitFeature, value=groupValue)  
      dataSubsetNodes = append(dataSubsetNodes, newNode)
    }    
  } else if(class(dataInstances[[splitFeature]]) == "numeric"){
    
    cutData = cut(dataInstances[[splitFeature]], breaks=2,  dig.lab = 20)
    groups = split(dataInstances, cutData)    
    cutDataLevels = levels(cutData)   
    
    intervals = strsplit(cutDataLevels, " ")    
    for(i in 1:length(intervals)){
      lowerBound = as.numeric(sub("[\\[\\(](.+),.*", "\\1", intervals[[i]]))
      upperBound = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", intervals[[i]]))

      newNode = NumericNode(dataInstances=groups[[i]], feature=splitFeature, lowerBound=lowerBound, upperBound=upperBound)  
      dataSubsetNodes = append(dataSubsetNodes, newNode)   
    }
  } 
  return(dataSubsetNodes)
}

GrowTree <- function(node, features, categories, classifierAttribute) {
  
  if (Homogeneous(node, classifierAttribute)){
    node = CategoryLabel(node, classifierAttribute)
    node@dataInstances = data.frame()
    return(node)
  }

  # find best split feature
  splitFeature <- BestSplit(node@dataInstances, features, categories, classifierAttribute)

  # assign feature to parent node
  node@label = splitFeature
    
  # create child nodes with split data instances
  childNodes = SplitByFeature(node, splitFeature)  
  node@dataInstances = data.frame()
  
  for(childNode in childNodes){
      
    #add each childnode to parent
    childNode = GrowTree(childNode, features, categories, classifierAttribute)     
    node@children = append(node@children, childNode)  
  }
  
  return(node) 
}

Node <- setClass( 
  "Node",
  slots = c(
    children = "list",
    label = "character",
    feature = "character",
    dataInstances = "data.frame"
  ), 
  prototyp=list(
    children = list(),
    label = ""
  ) 
)

NumericNode <- setClass(
  "NumericNode",
  slots = c(
    lowerBound = "numeric",
    upperBound = "numeric"    
    ), 
  contains="Node"
  )

CharacterNode <- setClass(
  "CharacterNode",
  slots = c(
    value = "character"  
  ),
  contains="Node" 
  )

setGeneric(name="fullFillsCondition", 
           def = function(nodeObject, dataInstance){
             standardGeneric("fullFillsCondition")
           })
setMethod(f="fullFillsCondition",
          signature="CharacterNode",
          definition=function(nodeObject, dataInstance){
            feature = nodeObject@feature
            return(dataInstance[feature] ==  nodeObject@value)
          })
setMethod(f="fullFillsCondition",
          signature="NumericNode",
          definition=function(nodeObject, dataInstance){
            feature = nodeObject@feature
     
            return((dataInstance[feature] > nodeObject@lowerBound) && (dataInstance[feature] <=  nodeObject@upperBound))
          })

classifyInstance <- function(node, instance){

  if(length(node@children) == 0){
    print(paste(c("Instance: ", row.names(instance), " Category: ", node@label), collapse = " "))
    return()
  }

  for(i in 1:length(node@children)){  
    child = node@children[[i]]

    if(fullFillsCondition(child, instance)){
      classifyInstance(child, instance)
      return()
    } 
  } 
   print("none found")
}

classifyInstances <- function(rootNode, dataInstances){

  for(i in 1:nrow(dataInstances)){  
    instance = dataInstances[i,]  
    classifyInstance(rootNode, instance)
  }
}

numberOfLeaves <- function(node){

  if(length(node@children) == 0){
    return(1)
  }
  totalNumber = 0
  for(i in 1:length(node@children)){  
    child = node@children[[i]]  
    
    totalNumber = totalNumber + numberOfLeaves(child)
  } 
  return(totalNumber)  
}

numberOfNodes <- function(node){
  if(length(node@children) == 0){
    return(1)
  }
  totalNumber = 0
  for(i in 1:length(node@children)){  
    child = node@children[[i]]  
    
    totalNumber = totalNumber + numberOfNodes(child)
  } 
  totalNumber = totalNumber + 1
  return(totalNumber) 
}

treeDepth <- function(rootNode){
  
}


# 1
# 
# customer = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
# textiles = c("medium", "few", "medium", "many", "few", "many", "few", "medium", "many", "few", "few", "many")
# gifts = c("few", "medium", "many", "few", "medium", "medium", "many", "few", "few", "few", "many", "many")
# avg_price = c("medium", "low", "medium", "high", "high", "low", "low", "low", "low", "high", "medium", "high")
# category = c("T", "N", "TG", "T", "G", "TG", "G", "N", "T", "N", "G", "TG")
# 
# df = data.frame(customer, textiles, gifts, avg_price, category, stringsAsFactors=F)
# 
# features = c("textiles", "gifts", "avg_price")
# categories = c("T", "G", "TG", "N")
# classifierAttribute = "category"
# 
# root = Node(dataInstances=df)
# root = GrowTree(root, features, categories, classifierAttribute)
# #classifyInstances(root, df)
# totalLeaves = numberOfLeaves(root)
# totalNodes = numberOfNodes(root)


# 2
white_wine_data <- read.csv("winequality-white.csv", header=T, sep=",",stringsAsFactors=F)  
columnNames = names(white_wine_data)
features = columnNames[columnNames != "quality"]
categories = unique(white_wine_data$quality)
classifierAttribute = "quality"

root = Node(dataInstances=white_wine_data)
root = GrowTree(root, features, categories, classifierAttribute)
# classifyInstances(root, white_wine_data)
totalLeaves = numberOfLeaves(root)
totalNodes = numberOfNodes(root)

# 3

# 4
# appends an element to an existing list
append <- function(data,value) {
  data[[length(data) + 1]] <- value
  return(data)
}

# sets the value at the specified index of the list to the given value
set <- function(data,index,value) {
  data[[index]] <- value
  return(data)
}

# randomly splits a list of data into k folds (equally sized)
split_folds <- function(k,data) {
  folds <- list()
  #sort the data randomly
  data <- sample(data)
  currentFold <- 0
  firstRound <- T
  # for next data: put into next fold, if each fold has received one new element -> start from the beginning
  for(n in 1:length(data)) {
    if(currentFold >= k) {
      currentFold <- 0
      firstRound <- F
    }
    currentFold <- currentFold + 1
    if(firstRound) {
      correctFold <- folds[currentFold] 
    } else {
      tmp <- folds[[currentFold]]
      correctFold <- tmp[1]
    }
    value <- data[[n]]
    if(firstRound) {
      correctFold <- set(correctFold,1,value)
    } else {
      correctFold <- append(correctFold, value)
    }
    folds <- set(folds,currentFold,correctFold)
  }
  return(folds)
}