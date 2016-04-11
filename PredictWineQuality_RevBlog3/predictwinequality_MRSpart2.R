#make a new column factorQuality from the column quality -- make the label column categorical
rxFactors(inData = data_classi, outFile = data_classi, overwrite = TRUE,
          factorInfo = list(factorQuality = list(varName = "quality")),reportProgress=0)

#can remove the column quality now
ColsToKeep = setdiff(names(data_classi),c('quality'))

data_classi = rxDataStep(inData = data_classi, outFile = 'data_classi_temp.xdf',varsToKeep = ColsToKeep, overwrite = TRUE)

#rename the label col 'factorQuality' as 'LabelsCol'
names(data_classi)[names(data_classi)=='factorQuality'] = 'LabelsCol'