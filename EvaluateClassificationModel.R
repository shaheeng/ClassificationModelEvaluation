# Purpose: Evaluate your classifcation model against some baseline metrics
#          Establish that your model is not just making lucky guesses but is significantly better than a random model
# Author : Shaheen Gauher - Data Scientist at Microsoft

# Note: The code below requires MRS (Microsoft R Server, formally Revolution R Enterprise (RRE))  
# http://blog.revolutionanalytics.com/2016/01/microsoft-r-open.html    
# MRS can be downloaded from https://www.dreamspark.com/Product/Product.aspx?productid=105

#download data from
#https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
datad_iris=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", 
                      header=F,sep=",",na.strings="NA")  #150 rows and 5 columns for iris data
class(datad_iris)  #"data.frame"
#convert data frame to xdf object using rxDataStep() 
pathc=getwd()
iris_xdf =file.path(pathc,'iris_xdf.xdf') 
data_iris = rxDataStep(inData = datad_iris, outFile = iris_xdf ,
                       rowsPerRead=50, overwrite=TRUE)
class(data_iris)  # "RxXdfData"

#rename the label col 'V5' as 'LabelsCol'
names(data_iris)[names(data_iris)=='V5'] = 'LabelsCol'


#Split the data into training and testing data. 

#use rxDataStep() to create a col called 'splitcol' to use for splitting
rxDataStep(inData=data_iris,outFile=data_iris,transforms=list(splitcol=factor(rbinom(.rxNumRows,1,0.8),labels=c('test','train'))),overwrite=T)  

#split using the col "splitcol" 
#rxSplit() -- Splits an input '.xdf' file or data frame into multiple '.xdf' files or a list of data frames.
listofxdfs = rxSplit(data_iris,outFileBase='irissplit',outFileSuffixes=c("Train", "Test"),
                     splitByFactor = "splitcol",overwrite=T )

trainingdata = listofxdfs[[2]]
testdata = listofxdfs[[1]]

#Build Machine Learning model - Decision Forest rxDForest()

#collect names of columns (features) to be used for modelling
allfeatures = setdiff(names(data_iris),c('LabelsCol','splitcol'))

#create formula for modelling
formula = as.formula(paste('LabelsCol',paste(allfeatures,collapse=' + '),sep=' ~ '))  
formula

#using rxDForest() to build ML model
DForest_model <- rxDForest(formula , data = trainingdata, seed = 10, cp = 0.01, nTree = 50, mTry = 2,                   
                           overwrite = TRUE, reportProgress = 0)
class(DForest_model) #"rxDForest" 
ML_model = DForest_model
ML_model

#Compute the accuracy of the model and compare to Kappa for the model
train_modelout = rxPredict(ML_model, data = trainingdata, outData = 'temp.xdf', overwrite = TRUE, writeModelVars = TRUE, predVarNames = "PredictedLabels",type='class')
class(train_modelout) #"RxXdfData"

mytemp_xdf = RxXdfData("mytemp_xdf.xdf") #initialise an xdf object 
results_model = rxDataStep(inData = train_modelout, outFile = mytemp_xdf,varsToKeep = c('PredictedLabels','LabelsCol'),overwrite = TRUE )
dim(results_model)

cmatrix = rxCrossTabs(~PredictedLabels:LabelsCol,results_model,returnXtabs = T)
class(cmatrix) #"xtabs" "table"

#Compute accuracy of the model
accuracy = sum(diag(cmatrix)) / sum(cmatrix)
cat('accuracy=',accuracy,'\n')

#Accuracy of a random guess classifier
num_classes = nrow(cmatrix)  #number of classs
accuracy_randgs = 1 / num_classes   
cat('accuracy_random_guess = ',accuracy_randgs,'\n')

#Accuracy of a weighted guess classifier
x = apply(cmatrix,1, sum) / sum(cmatrix)  ##fraction of instances per class
accuracy_wtdgs = sum(x^2)
cat('accuracy_weighted_guess = ',accuracy_wtdgs,'\n')

#Accuracy of Majority Class classifier
num_eachclass = apply(cmatrix,1, sum)
majorityClass = which(num_eachclass==max(num_eachclass))
fraction_eachclass = num_eachclass / sum(cmatrix)

accuracy_maj_class = fraction_eachclass[majorityClass[1]]  
cat('accuracy_maj_class',accuracy_maj_class,'\n')

#Compute Kappa
#get marginal frequency for each class
Marg_freq_class = function(i){rowSums(cmatrix)[i] * colSums(cmatrix)[i] / sum(cmatrix) }
n = sum(cmatrix)  #total number of instances
accuracy_exp = sum(sapply(1:3,FUN=function(x){sum(Marg_freq_class(x))}))/n
cat('Expected Accuracy=',accuracy_exp)

kappa = (accuracy - accuracy_exp) / (1 - accuracy_exp)
cat('kappa=',kappa,'\n')

#remove file
if(file.exists("iris_xdf.xdf") ) {  file.remove("iris_xdf.xdf") }