################################################################################
## Decision forest modeling
################################################################################
#Decision Forest
#using rxDForest() to build ML model
DForest_model <- rxDForest(formula = formula, 
                           data = trainingdata, 
                           seed = 10, 
                           cp = 0.01, 
                           nTree = 50, 
                           mTry = 2,                   
                           overwrite = TRUE, 
                           reportProgress = 0)
DForest_model
class(DForest_model) #"rxDForest" 

################################################################################
## Boosted tree modeling
################################################################################
BoostedTree_model = rxBTrees(formula = formula,
                             data = trainingdata,
                             learningRate = 0.2,
                             minSplit = 10,
                             minBucket = 10,
                             nTree = 100,
                             lossFunction = "multinomial",
                             reportProgress = 0)
BoostedTree_model
class(BoostedTree_model)
################################################################################
## Decision Tree Modelling
################################################################################

#rxDTree
DTree_model = rxDTree(formula = formula,
                      data = trainingdata,
                      minSplit = 10,
                      minBucket = 10,
                      nTree = 100,
                      reportProgress = 0)
DTree_model
class(DTree_model)

################################################################################