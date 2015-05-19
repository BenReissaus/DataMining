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


BestSplit <- function(trainingInstances, features, categories, classifierAttribute){

  I_min = log2(length(categories)) 
  splitFeature = NULL
  
  for(feature in features){
   
    # group by feature values
    if(class(trainingInstances[[feature]]) == "character"){
      groups = split(trainingInstances, f = trainingInstances[[feature]])   
      
    } else if(class(trainingInstances[[feature]]) == "numeric"){   
      
      # do not attempt to split if there is only one value in that column left
      if(length(unique(trainingInstances[[feature]])) == 1){
        next
      }
      groups = split(trainingInstances, cut(trainingInstances[[feature]], breaks=2,  dig.lab = 20))
      
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

    totalInstanceCount = nrow(trainingInstances)
    imp = Impurity(categoryCounts, totalInstanceCount)  

    if(imp < I_min){
      I_min = imp
      splitFeature = feature
    }  
  }

  return(splitFeature)
}

Homogeneous <- function(node, classifierAttribute){
  
  uniques=unique(c(node@trainingInstances[, classifierAttribute]))  
  return(length(uniques) == 1)
}

CategoryLabel <- function(node, classifierAttribute){

  # return the most often ocurring category as label
  node@label = names(which.max(table(node@trainingInstances[[classifierAttribute]])))

  return(node)
}


SplitByFeature <- function(node, splitFeature){
  
  trainingInstances = node@trainingInstances  
  dataSubsetNodes = list()
  
  if(class(trainingInstances[[splitFeature]]) == "character"){
  
    groups = split(trainingInstances, f = trainingInstances[[splitFeature]])   
    for( group in groups){
      
      groupValue = group[[splitFeature]][[1]]
      newNode = CharacterNode(trainingInstances=group, feature=splitFeature, value=groupValue)  
      dataSubsetNodes = append(dataSubsetNodes, newNode)
    }    
  } else if(class(trainingInstances[[splitFeature]]) == "numeric"){
    
    cutData = cut(trainingInstances[[splitFeature]], breaks=2,  dig.lab = 20)
    groups = split(trainingInstances, cutData)    
    cutDataLevels = levels(cutData)   
    
    intervals = strsplit(cutDataLevels, " ")   
    splitValue = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", intervals[[1]]))
    
    newNodeUpper = NumericNode(trainingInstances=groups[[1]], feature=splitFeature, splitValue=splitValue, position="lower")  
    dataSubsetNodes = append(dataSubsetNodes, newNodeUpper)
    
    newNodeLower = NumericNode(trainingInstances=groups[[2]], feature=splitFeature, splitValue=splitValue, position="upper")  
    dataSubsetNodes = append(dataSubsetNodes, newNodeLower)
    

  } 
  return(dataSubsetNodes)
}

GrowTree <- function(node, features, categories, classifierAttribute) {
  
  if (Homogeneous(node, classifierAttribute)){
    node = CategoryLabel(node, classifierAttribute)
    #node@trainingInstances = data.frame()
    return(node)
  }

  # find best split feature
  splitFeature <- BestSplit(node@trainingInstances, features, categories, classifierAttribute)

  # assign feature to parent node
  node@label = splitFeature
    
  # create child nodes with split data instances
  childNodes = SplitByFeature(node, splitFeature)  
  #node@trainingInstances = data.frame()
  
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
    trainingInstances = "data.frame", 
    testInstances = "data.frame"
  ), 
  prototyp=list(
    children = list(),
    label = "", 
    testInstances = data.frame()
  ) 
)

NumericNode <- setClass(
  "NumericNode",
  slots = c(
      splitValue = "numeric", 
      position = "character" # can be lower or upper   
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
            position = nodeObject@position
            splitValue = nodeObject@splitValue
            
            if(position == "upper"){
              return((dataInstance[feature] > splitValue))
            }
            else {
              return((dataInstance[feature] <= splitValue))
            }
            
          })

classifyInstance <- function(node, instance){

  if(length(node@children) == 0){
    copyInstance = instance
    copyInstance[, "classified"] = node@label
    node@testInstances = rbind(node@testInstances, copyInstance)
#     print(paste(c("Instance: ", row.names(instance), " Category: ", node@label), collapse = " "))
    return(node)
  }

  for(i in 1:length(node@children)){  
    child = node@children[[i]]

    if(fullFillsCondition(child, instance)){
      node@children[[i]] = classifyInstance(child, instance)
      return(node)
    } 
  } 
  return(node)

}

classifyInstances <- function(rootNode, testInstances){

  for(i in 1:nrow(testInstances)){  
    instance = testInstances[i,]  
    rootNode = classifyInstance(rootNode, instance)
  }
  return(rootNode)
}

collectClassifiedInstances <- function(node){
  if(length(node@children) == 0){ 
    return(node@testInstances)
  }

  instances = data.frame()
  for(i in 1:length(node@children)){  
    child = node@children[[i]]  
    
    instances = rbind(instances, collectClassifiedInstances(child)) 
  } 
  return(instances) 
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

minTreeDepth <- function(node){
  
  if(length(node@children) == 0){
    return(0);
  }
  
  minimas = c()
  for(i in 1:length(node@children)){  
    child = node@children[[i]]  
    
    minimas = c(minimas, minTreeDepth(child)) 
  } 
  return(min(minimas) + 1)  
}

maxTreeDepth <- function(node){
  
  if(length(node@children) == 0){
    return(0);
  }
  
  maximas = c()
  for(i in 1:length(node@children)){  
    child = node@children[[i]]  
    
    maximas = c(maximas, maxTreeDepth(child)) 
  } 
  return(max(maximas) + 1)  
}

getBestSubTree <- function(node, classifiedInstances, classifierAttribute){
  
  totalChildrenPositives = nrow(subset(classifiedInstances, classifiedInstances[[classifierAttribute]] == classifiedInstances[["classified"]]))
  
  # set defaults
  majorityVotingPositives = 0
  majorityClass = node@children[[1]]@trainingInstances[1,][[classifierAttribute]]
  
  if(length(classifiedInstances) > 0){ # subtree contains classified elements
    
    # save all classified instances in the node for future pruning in upper tree
    node@testInstances = classifiedInstances 
    
    majorityVotingPositives = nrow(subset(classifiedInstances, classifiedInstances[[classifierAttribute]] == majorityClass))
    majorityClass = names(which.max(table(classifiedInstances[[classifierAttribute]])))
    
    if(majorityVotingPositives >= totalChildrenPositives){ # prune the tree 
      
      node@testInstances[["classified"]] = majorityClass       
      node@label = as.character(majorityClass)
      node@feature = ""
      node@children = list()    
    }
  }
  else { # none of the chlildren has classified a test element
    node@label = as.character(majorityClass)
    node@feature = ""
    node@children = list() 
  }
  
  return(node)
}

prune <- function(node, classifierAttribute){
  
  if(length(node@children) == 0){    
    return(node)
  }
  
  classifiedInstances = data.frame()
  for(i in 1:length(node@children)){  
    
    node@children[[i]] = prune(node@children[[i]], classifierAttribute)
    classifiedInstances = rbind(classifiedInstances, node@children[[i]]@testInstances)
  } 
  node = getBestSubTree(node, classifiedInstances, classifierAttribute)
  return(node) 
}


# Exercise 1
customer = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
textiles = c("medium", "few", "medium", "many", "few", "many", "few", "medium", "many", "few", "few", "many")
gifts = c("few", "medium", "many", "few", "medium", "medium", "many", "few", "few", "few", "many", "many")
avg_price = c("medium", "low", "medium", "high", "high", "low", "low", "low", "low", "high", "medium", "high")
category = c("T", "N", "TG", "T", "G", "TG", "G", "N", "T", "N", "G", "TG")

df = data.frame(customer, textiles, gifts, avg_price, category, stringsAsFactors=F)

features = c("textiles", "gifts", "avg_price")
categories = c("T", "G", "TG", "N")
classifierAttribute = "category"

root = Node(trainingInstances=df)
root = GrowTree(root, features, categories, classifierAttribute)
root = classifyInstances(root, df)
classiefiedInstances = collectClassifiedInstances(root)

print("Classified Instances: ")
print(classiefiedInstances)


# 2
whiteWineData <- read.csv("winequality-white.csv", header=T, sep=",",stringsAsFactors=F)  
columnNames = names(whiteWineData)
features = columnNames[columnNames != "quality"]
categories = unique(whiteWineData$quality)
classifierAttribute = "quality"

trainingSet = whiteWineData
testSet = whiteWineData

root = Node(trainingInstances=trainingSet)
root = GrowTree(root, features, categories, classifierAttribute)
root = classifyInstances(root, testSet)
classInstances = collectClassifiedInstances(root)

print(paste(c("Number of leaves: ", numberOfLeaves(root)), collapse = " "))
print(paste(c("Number of nodes: ", numberOfNodes(root)), collapse = " "))
print(paste(c("Minimum Tree Depth: ", minTreeDepth(root)), collapse = " "))
print(paste(c("Maximum Tree Depth: ", maxTreeDepth(root)), collapse = " "))


# 3
# grow tree with the training set and prune with the pruning set
trainingSet = whiteWineData[c(seq(1,4286)),]
pruningSet = whiteWineData[c(seq(4286,nrow(whiteWineData))),]

root = Node(trainingInstances=trainingSet)
root = GrowTree(root, features, categories, classifierAttribute)
root = classifyInstances(root, pruningSet)

root = prune(root, classifierAttribute)

classInstances = collectClassifiedInstances(root)

print(paste(c("Number of leaves: ", numberOfLeaves(root)), collapse = " "))
print(paste(c("Number of nodes: ", numberOfNodes(root)), collapse = " "))
print(paste(c("Minimum Tree Depth: ", minTreeDepth(root)), collapse = " "))
print(paste(c("Maximum Tree Depth: ", maxTreeDepth(root)), collapse = " "))


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

# Assignment 5

# support
f_0 <- 0.55
#confidence
c_0 <- 0.75

readData <- function() {
  s1 <- c("beef","pickled cucumber","milk",1)
  s2 <- c("beef","cheese","milk",1)
  s3 <- c("milk","boots",0)
  s4 <- c("beef","chicken","cheese",0)
  s5 <- c("beef","chicken","clothes","pickled cucumber","cheese","milk",1)
  s6 <- c("chicken","clothes","pickled cucumber",1)
  s7 <- c("chicken","pickled cucumber","clothes",1)
  
  df <- data.frame(id=c(1,2,3,4,5,6,7))
  items <- list(s1,s2,s3,s4,s5,s6,s7)
  df$items <- I(items)
  return(df)
}

merge <- function(X,Y) {
  return(unique(c(X,Y)))
}

extractPossibleItems <- function(itemsets) {
  items <- list()
  for(itemset in itemsets) {
    for(value in itemset) {
      if(!value %in% items) {
        items[length(items) + 1] <- value
      }
    }
  }
  return(items)
}

Supp <- function(D,itemset) {
  occ <- 0
  for(row in data$items) {
    contained <- T
    for(item in itemset) {
      tmp <- (!is.na(item)) & (!item %in% row) 
      if(tmp) {
        contained <- F
      }
    }
    if(contained) {
      occ <- occ + 1
    }
  }
  return(occ/length(data$id))
}

Conf <- function(D,X,Y) { 
  return(Supp(D,merge(X,Y))/Supp(D,X))
}

extend <- function(itemset,allItems) {
  extendedItemsets <- list()
  for(ext in allItems) {
    if(!ext %in% itemset[[1]]) {
      extension <- c()
      for(item in itemset[[1]]) {
        extension[[length(extension)+1]] <- item
      }
      extension[[length(extension)+1]] <- ext
      extendedItemsets[[length(extendedItemsets)+1]] <- extension
    }
  }
  return(extendedItemsets)
}

minimize <- function(itemsets) {
  minimizedSets <- list()
  minimizedSets[[1]] <- itemsets[[1]]
  for(itemset in itemsets) {
    newSet <- T
    tmp <- list()
    for(minimizedSet in minimizedSets) {
      if(length(itemset[[1]]) == 1) {
        tmp <- itemset
      } else {
        tmp <- itemset[[1]]
      }
      if(length(tmp) == length(minimizedSet)) {
        a <- setdiff(tmp,minimizedSet)
        if(length(a) == 0) {
          newSet <- F
        }
      } 
    }
    if(newSet) {
      tmp2 <- list()
      if(!length(tmp) == 1) {
        for(value in tmp) {
          tmp2[[length(tmp2)+1]] <- value
        }
      } else {
        tmp2 <- tmp
      }
      minimizedSets[[length(minimizedSets)+1]] <- tmp2
    }
  }
  return(minimizedSets)
}

FrequentItems <- function(D,f_0) {
  M <- list()
  allItems <- extractPossibleItems(D$items)
  Q <- extractPossibleItems(D$items)
  while(!length(Q) == 0) {
    I <- Q[1]
    Q[1] <- NULL
    max <- T
    for(I_prime in extend(I,allItems)) {
      if(Supp(D,I_prime) >= f_0) {
        max <- F
        Q[[length(Q)+1]] <- I_prime
      }
    }
    if(max) {
      M[[length(M)+1]] <- I
    }
  }
  return(M)
}

data <- readData()

buildRule <- function(left,right) {
  rule <- list()
  rule[[1]] <- left
  rule[[2]] <- right
  return(rule)
}

pruneSubsets <- function(subsets,max) {
  subsets[[1]] <- NULL
  for(n in length(subsets):1) {
    if(length(subsets[[n]]) == max) {
      subsets[[n]] <- NULL
    }
  }
  return(subsets)
}

calculateRightSides <- function(leftSides, set) {
  completeSet <- set
  for(m in 1:length(leftSides)) {
    if(length(leftSides[[m]]) == 1) {
      tmp <- list()
      tmp[[1]] <- leftSides[[m]]
      leftSides[[m]] <- tmp
    } 
  }
  for(n in 1:length(leftSides)) { 
    split <- list()
    split[[1]] <- leftSides[[n]]
    
    # no clue why this dont work
    difference <- setdiff(leftSides[[n]],set)
    #####
    
    split[[2]] <- difference
    leftSides[[n]] <- split
  }
  return(leftSides)
}

powerset <- function(set){
  ps = list()
  ps[[1]] = numeric()
  for(element in set){
    temp = vector(mode="list",length=length(ps))
    for(subset in 1:length(ps)){
      temp[[subset]] = c(ps[[subset]],element)
    }
    ps=c(ps,temp)
  }
  ps <- pruneSubsets(ps,length(set))
  
  ps <- calculateRightSides(ps,set)
  
  return(ps)
}

AssociationRules <- function(D,f_0,c_0) {
  # calculate frequent itemsets
  frequentSets <- FrequentItems(data,f_0)
  minimizedFrequentSets <- minimize(frequentSets)
  
  # calculate rules
  rules <- list()
  for(set in minimizedFrequentSets) {
    if(length(set) == 1) {
      if(Supp(D,set) >= c_0) {
        rules[[length(rules)+1]] <- buildRule(list(),set)
      }
    } else {
      subsets <- powerset(set)
      for(subset in subsets) {
        confidence <- Conf(D,subset[[1]],subset[[2]])
        if(confidence >= c_0) {
          rules[[length(rules)+1]] <- buildRule(subset[[1]], subset[[2]])
        }
      } 
    }
  }
  return(rules)
}

a <- AssociationRules(data,f_0,c_0)