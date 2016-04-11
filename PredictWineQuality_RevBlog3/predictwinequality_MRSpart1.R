##download data from
#https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv
data_wine = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                       header=T,sep=";",na.strings="NA") #1599 12
class(data_wine)  #"data.frame"

#Relabel quality ratings as follows
#3,4,5 as Low
#6     as Med
#7,8   as High

data_wine$qualityV2 = ifelse(data_wine$quality <=5, 'Low','None')
TorF = data_wine$quality == 6
data_wine$qualityV2[TorF] = 'Med'
TorF = data_wine$quality > 6
data_wine$qualityV2[TorF] = 'High'

data_wine$quality = NULL 
names(data_wine)[names(data_wine)=='qualityV2'] = 'quality'
#=============================

pathc = getwd()  #current working directory
#convert data frame to xdf object using rxDataStep() 
dataclassi_xdf = file.path(pathc,'dataclassi_xdf.xdf') 
data_classi = rxDataStep(inData = data_wine, outFile = dataclassi_xdf ,
                         rowsPerRead=500, overwrite=TRUE, reportProgress=0)
class(data_classi)  # "RxXdfData"
