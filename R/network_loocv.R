network_loocv <- function(data_matrix,feature_type,nf,powerS,nc,L,classifier,kern){

  if(missing(L)){
    L='label'
  }

  if(missing(nf)){
    nf=0
  }

  if(missing(powerS)){
    powerS=1
  }

  if(missing(nc)){
    nc=1
  }

  if(missing(classifier)){
    classifier = "SVM"
  }

  if(missing(kern)){
    kern = "linear"
  }

  names(data_matrix)[colnames(data_matrix)==L] <- paste("label")
  pred <- NULL

  classes <- unique(data_matrix$label)
  for(i in 1:nrow(data_matrix)){
    data_train <- data_matrix[-i,]  # training data
    data_test <- data_matrix[i,] # test data
    result <- network_classify(data_train,data_test,feature_type,nf,powerS,nc,L,classifier,kern)
    pred[[i]] <- result$pred
  }
  predx <- as.numeric(as.factor(pred))
  testx <- as.numeric(as.factor(data_matrix$label))
  accuracy = sum(predx==testx)/nrow(data_matrix)
  return(accuracy)
}
