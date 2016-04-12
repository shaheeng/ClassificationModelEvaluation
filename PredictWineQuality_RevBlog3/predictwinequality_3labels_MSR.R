# Purpose: Build classification models to predict wine quality
#          Use three different classification algorithms and compare their accuracies
# Author : Shaheen Gauher - Data Scientist at Microsoft

# Note: The code below requires MRS (Microsoft R Server, formally Revolution R Enterprise (RRE)) 
# http://blog.revolutionanalytics.com/2016/01/microsoft-r-open.html 
# MRS can be downloaded from https://www.dreamspark.com/Product/Product.aspx?productid=105

##download data from
#https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv
data_wine = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                       header=T,sep=";",na.strings="NA") #1599 12
class(data_wine)  #"data.frame"

#Relabel quality ratings as follows
#3,4,5 as Low
#6 as Med
#7,8 as High

data_wine$qualityV2 = ifelse(data_wine$quality <=5, 'Low','None')
TorF = data_wine$quality == 6
data_wine$qualityV2[TorF] = 'Med'
TorF = data_wine$quality > 6
data_wine$qualityV2[TorF] = 'High'

data_wine$quality = NULL
names(data_wine)[names(data_wine)=='qualityV2'] = 'quality'
#=============================

pathc = getwd()  #working directory
#convert data frame to xdf object using rxDataStep() 
dataclassi_xdf = file.path(pathc,'dataclassi_xdf.xdf') 
data_classi = rxDataStep(inData = data_wine, outFile = dataclassi_xdf ,
                         rowsPerRead=500, overwrite=TRUE, reportProgress=0)
class(data_classi)  # "RxXdfData"

#make a new column factorQuality from quality col -- make it categorical
rxFactors(inData = data_classi, outFile = data_classi, overwrite = TRUE,
          factorInfo = list(factorQuality = list(varName = "quality")),reportProgress=0)

#can remove the col quality now
ColsToKeep = setdiff(names(data_classi),c('quality'))

data_classi = rxDataStep(inData = data_classi, outFile = 'data_classi_temp.xdf',varsToKeep = ColsToKeep, overwrite = TRUE)

#rename the label col 'factorQuality' as 'LabelsCol'
names(data_classi)[names(data_classi)=='factorQuality'] = 'LabelsCol'

#use rxDataStep() to create a col called 'splitcol' to use for splitting
rxDataStep(inData=data_classi,outFile=data_classi,transforms=list(splitcol=factor(rbinom(.rxNumRows,1,0.8),labels=c('test','train'))),overwrite=T)  

#split using the col "splitcol" 
#rxSplit() -- Splits an input '.xdf' file or data frame into multiple '.xdf' files or a list of data frames.
listofxdfs = rxSplit(data_classi,outFileBase='data_classi_split',outFileSuffixes=c("Train", "Test"),splitByFactor = "splitcol",overwrite=T )

trainingdata = listofxdfs[[2]]
testdata     = listofxdfs[[1]]

#collect names of columns (features) to be used for modelling
allfeatures = setdiff(names(data_classi),c('LabelsCol','splitcol'))

#create formula for modelling
formula = as.formula(paste('LabelsCol',paste(allfeatures,collapse=' + '),sep=' ~ '))  
formula

Algorithms <- c("Decision Forest Classification",
                "Boosted Decision Tree Classification",
                "Decision Tree Classification")

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
#=======================================================
# Compute the accuracy of the trained models and how it performs on the test data
#=======================================================

#Function to compute accuracy of the trained model on the given data
computeaccuracy <- function(ML_model,scoredata){
  if(file.exists("modelout_xdf.xdf") ) {  file.remove("modelout_xdf.xdf") }
  modelout_xdf = RxXdfData("modelout_xdf.xdf") #initialise xdf object 
  rxPredict(ML_model, data = scoredata, outData = modelout_xdf, overwrite = TRUE,
            writeModelVars = TRUE, reportProgress = 0)
  #head(modelout_xdf)  #contains the actual and predicted cols
  
  #get the columns "LabelsCol_Pred" and "LabelsCol" from modelout_xdf
  
  
  mytemp_xdf = RxXdfData("mytemp_xdf.xdf") #initialise an xdf object 
  results_model = rxDataStep(inData = modelout_xdf, outFile = mytemp_xdf,varsToKeep = c('LabelsCol_Pred','LabelsCol'),
                             overwrite = TRUE, reportProgress = 0 )
  
  cm = rxCrossTabs(~LabelsCol_Pred:LabelsCol,results_model,returnXtabs = T, reportProgress=0) #create a confusion matrix
  cm
  accuracy = sum(diag(cm)) / sum(cm)
  accuracy
  
  #cat('The model produced an accuracy = ',accuracy,'\n')
  return(accuracy)
}
# to invoke function:
# computeaccuracy(ML_model,testdata)
# computeaccuracy(ML_model,trainingdata)
#====================================================

ML_model = DForest_model

cat('For Decision Forest: accuracy = ',computeaccuracy(ML_model,trainingdata),'\n')
cat('For Decision Forest: accuracy on test data = ',computeaccuracy(ML_model,testdata),'\n')
#====================================================
ML_model = BoostedTree_model

cat('For Boosted tree: accuracy = ',computeaccuracy(ML_model,trainingdata),'\n')
cat('For Boosted tree: accuracy on test data = ',computeaccuracy(ML_model,testdata),'\n')
#====================================================
ML_model = DTree_model

cat('For Decision Tree: accuracy = ',computeaccuracy(ML_model,trainingdata),'\n')
cat('For Decision Tree: accuracy on test data = ',computeaccuracy(ML_model,testdata),'\n')


