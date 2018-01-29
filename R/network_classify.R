# main function
network_classify <- function(data_train,data_test,feature_type,p,corr,nf,powerS,nc,L,classifier,kern,normal,add){
  if(missing(p)) p=0;
  if(missing(corr)) corr=0;
  if(missing(nf)) nf=0;
  if(missing(powerS)) powerS=1;
  if(missing(nc)) nc=1;
  if(missing(classifier)) classifier = "SVM";
  if(missing(kern)) kern = "linear";
  if(missing(normal)) normal=1;
  if(missing(L)) L="label";
  if(missing(add)) add=0;

  nc = round(max(1,min(nc,nf/5)))

  newdata <- network_features(data_train,data_test,feature_type,p,corr,nf,powerS,nc,L,normal)

  if(add==1)
  {
    data_trainx <- cbind(newdata$new_train,data_train)
    data_trainx <- subset(data_trainx,select=-label)
    data_testx <- cbind(newdata$new_test,data_test)
    data_testx <- subset(data_testx,select=-label)

  } else{
    data_trainx <- newdata$new_train
    data_testx <- newdata$new_test
  }

  # test
  if(classifier=="SVM")
  {
    require('e1071', quietly = TRUE)
    library(e1071)

    wts <- nrow(data_trainx) / table(newdata$train_label)
    model1 <- svm(data_trainx,newdata$train_label,type="C-classification",class.weights = wts,kernel = kern)
    prediction <- predict(model1, data_testx)
  }

  if(classifier=="RF")
  {
    require('randomForest', quietly = TRUE)
    library(randomForest)
    model_rf <- randomForest(ndata_trainx,newdata$train_label,importance=TRUE, proximity=TRUE)
    prediction <- predict(model_rf, data_testx)
  }

  return(list(pred = prediction, acc = sum(prediction==newdata$test_label)/nrow(data_test)))
}


