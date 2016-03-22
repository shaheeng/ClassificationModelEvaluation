# Purpose: Evaluate your classification model against some baseline metrics
#          Establish that your model is not just making lucky guesses but is significantly better than a random model
# Author : Shaheen Gauher - Data Scientist at Microsoft


#download data from
#https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
datad_adult=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                       header=F,sep=",",na.strings="NA")  #32561 rows and 15 columns for adult data
names(datad_adult)=c('age', 'workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','income')

dropcols = c('workclass','occupation','native_country')
datad_adult = datad_adult[ , !(names(datad_adult) %in% dropcols)] 

class(datad_adult)  #"data.frame"

#rename the label col 'income' as 'LabelsCol'
names(datad_adult)[names(datad_adult)=='income'] = 'LabelsCol'

#Split the data into training and testing data. 
# randomly split your data frame into 60% and 40% partitions (stored in a list):
set.seed(1)
listsplit = split(datad_adult, sample(1:nrow(datad_adult) > round(nrow(datad_adult) * .4)))
trainingdata = listsplit[[2]]
testdata     = listsplit[[1]]


#Build Machine Learning model - Random Forest randomForest()

#collect names of columns (features) to be used for modelling
allfeatures = setdiff(names(datad_adult),c('LabelsCol','splitcol'))

#create formula for modelling
formula = as.formula(paste('LabelsCol',paste(allfeatures,collapse=' + '),sep=' ~ '))  
formula

#using randomForest() to build ML model
library(randomForest)
ML_model = randomForest(formula , data = trainingdata, ntree=50, nodesize=5, mtry=9)
ML_model

#Compute the accuracy of the model and compare to accuracy of trivial models

train_modelout = predict(ML_model,trainingdata,type="response")

#Confusion Matrix
cmatrix = as.matrix(table(Actual = trainingdata$LabelsCol, Predicted = train_modelout))
class(cmatrix) #"table"

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
