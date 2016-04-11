#=======================================================
# Compute the accuracy of the trained models and how it performs on the test data
#=======================================================

#Function to compute accuracy of the trained model on the given data
computeaccuracy <- function(ML_model,scoredata){
  if(file.exists("modelout_xdf.xdf") ) {  file.remove("modelout_xdf.xdf") }
  modelout_xdf = RxXdfData("modelout_xdf.xdf") #initialise xdf object 
  rxPredict(ML_model, data = scoredata, outData = modelout_xdf, overwrite = TRUE,
            writeModelVars = TRUE, reportProgress = 0)
  
  #get the columns "LabelsCol_Pred" and "LabelsCol" from modelout_xdf
  results_model_df = rxDataStep(inData=modelout_xdf,outFile=NULL,varsToKeep=c('LabelsCol_Pred','LabelsCol'),reportProgress = 0)
  head(results_model_df)
  
  actual    = results_model_df$LabelsCol
  predicted = results_model_df$LabelsCol_Pred
  cm = as.matrix(table(Actual=actual, Predicted=predicted)) #create a confusion matrix
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
# For Decision Forest: accuracy =  0.6883426 
cat('For Decision Forest: accuracy on test data = ',computeaccuracy(ML_model,testdata),'\n')
# For Decision Forest: accuracy on test data =  0.6360947 
#====================================================
ML_model = BoostedTree_model

cat('For Boosted tree: accuracy = ',computeaccuracy(ML_model,trainingdata),'\n')
# For Boosted tree: accuracy =  0.7192704 
cat('For Boosted tree: accuracy on test data = ',computeaccuracy(ML_model,testdata),'\n')
# For Boosted tree: accuracy on test data =  0.6508876 
#====================================================
ML_model = DTree_model

cat('For Decision Tree: accuracy = ',computeaccuracy(ML_model,trainingdata),'\n')
# For Decision Tree: accuracy =  0.77954 
cat('For Decision Tree: accuracy on test data = ',computeaccuracy(ML_model,testdata),'\n')
# For Decision Tree: accuracy on test data =  0.6390533 
