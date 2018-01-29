# main function
network_classify <- function(data_train,data_test,feature_type,p,corr,nf,powerS,nc,L,classifier,kern,normal){
  if(missing(p)) p=0;
  if(missing(corr)) corr=0;
  if(missing(nf)) nf=0;
  if(missing(powerS)) powerS=1;
  if(missing(nc)) nc=1;
  if(missing(classifier)) classifier = "SVM";
  if(missing(kern)) kern = "linear";
  if(missing(normal)) normal=1;
  if(missing(L)) L="label";

  nc = round(max(1,min(nc,nf/5)))

  newdata <- network_features(data_train,data_test,feature_type,p,corr,nf,powerS,nc,L,normal)

  # test
  if(classifier=="SVM")
  {
    require('e1071', quietly = TRUE)
    library(e1071)

    wts <- nrow(newdata$new_train) / table(newdata$train_label)
    model1 <- svm(newdata$new_train,newdata$train_label,type="C-classification",class.weights = wts,kernel = kern)
    prediction <- predict(model1, newdata$new_test)
  }

  if(classifier=="RF")
  {
    require('randomForest', quietly = TRUE)
    library(randomForest)
    model_rf <- randomForest(newdata$new_train,newdata$train_label,importance=TRUE, proximity=TRUE)
    prediction <- predict(model_rf, newdata$new_test)
  }

  return(list(pred = prediction, acc = sum(prediction==newdata$test_label)/nrow(data_test)))
}


