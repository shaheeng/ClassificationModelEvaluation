#use rxDataStep() to create a col called 'splitcol' to use for splitting
rxDataStep(inData=data_classi,outFile=data_classi,transforms=list(splitcol=factor(rbinom(.rxNumRows,1,0.8),labels=c('test','train'))),overwrite=T)  

#split using the col "splitcol" 
#rxSplit() -- Splits an input '.xdf' file or data frame into multiple '.xdf' files or a list of data frames.
listofxdfs = rxSplit(data_classi,outFileBase='data_classi_split',outFileSuffixes=c("Train", "Test"),splitByFactor = "splitcol",overwrite=T )

trainingdata = listofxdfs[[2]]
testdata     = listofxdfs[[1]]