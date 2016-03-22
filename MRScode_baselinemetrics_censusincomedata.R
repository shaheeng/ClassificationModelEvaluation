# Purpose: Evaluate your classification model against some baseline metrics
#          Establish that your model is not just making lucky guesses but is significantly better than a random model
# Author : Shaheen Gauher - Data Scientist at Microsoft

# Note: The code below requires MRS (Microsoft R Server, formally Revolution R Enterprise (RRE))  
# http://blog.revolutionanalytics.com/2016/01/microsoft-r-open.html    
# MRS can be downloaded from https://www.dreamspark.com/Product/Product.aspx?productid=105

#download data from
#https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
datad_adult=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                      header=F,sep=",",na.strings="NA")  #32561 rows and 15 columns for adult data
names(datad_adult)=c('age', 'workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','income')

dropcols = c('workclass','occupation','native_country')
datad_adult = datad_adult[ , !(names(datad_adult) %in% dropcols)] 

class(datad_adult)  #"data.frame"
#convert data frame to xdf object using rxDataStep() 
pathc     = getwd()
adult_xdf = file.path(pathc,'adult_xdf.xdf') 
data_adult = rxDataStep(inData = datad_adult, outFile = adult_xdf ,
                       rowsPerRead=1000, overwrite=TRUE)
class(data_adult)  # "RxXdfData"

#rename the label col 'income' as 'LabelsCol'
names(data_adult)[names(data_adult)=='income'] = 'LabelsCol'

#Split the data into training and testing data. 

#use rxDataStep() to create a col called 'splitcol' to use for splitting
rxDataStep(inData=data_adult,outFile=data_adult,transforms=list(splitcol=factor(rbinom(.rxNumRows,1,0.8),labels=c('test','train'))),overwrite=T)  

#split using the col "splitcol" 
#rxSplit() -- Splits an input '.xdf' file or data frame into multiple '.xdf' files or a list of data frames.
listofxdfs = rxSplit(data_adult,outFileBase='adultsplit',outFileSuffixes=c("Train", "Test"),splitByFactor = "splitcol",overwrite=T )

trainingdata = listofxdfs[[2]]
testdata = listofxdfs[[1]]

#Build Machine Learning model - Decision Forest rxDForest()

#collect names of columns (features) to be used for modelling
allfeatures = setdiff(names(data_adult),c('LabelsCol','splitcol'))

#create formula for modelling
formula = as.formula(paste('LabelsCol',paste(allfeatures,collapse=' + '),sep=' ~ '))  
formula

#using rxDForest() to build ML model
DForest_model <- rxDForest(formula , data = trainingdata, seed = 10, cp = 0.01, nTree = 50, mTry = 2,                   
                           overwrite = TRUE, reportProgress = 0)
class(DForest_model) #"rxDForest" 
ML_model = DForest_model
ML_model

#Compute the accuracy of the model and compare to accuracy of trivial models
train_modelout = rxPredict(ML_model, data = trainingdata, outData = 'temp.xdf', overwrite = TRUE, writeModelVars = TRUE, predVarNames = "PredictedLabels",type='class')
class(train_modelout) #"RxXdfData"

mytemp_xdf = RxXdfData("mytemp_xdf.xdf") #initialise an xdf object 
results_model = rxDataStep(inData = train_modelout, outFile = mytemp_xdf,varsToKeep = c('PredictedLabels','LabelsCol'),overwrite = TRUE )

#Confusion Matrix
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


#remove file
if(file.exists("adult_xdf.xdf") ) {  file.remove("adult_xdf.xdf") }