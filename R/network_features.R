network_features <- function(data_train,data_test,feature_type=c("smoothness","calculus"),p,corr,nf,powerS,nc,L,vars)
{
  if(missing(p)) p=0;
  if(missing(corr)) corr=0;
  if(missing(L)) L='label';
  if(missing(powerS)) powerS=1;
  if(missing(nc)) nc=1;
  if(missing(nf)) nf=0
  if(missing(vars)) vars=0;

  classes <- unique(data_train$label)

  names(data_train)[colnames(data_train)==L] <- paste("label")
  names(data_test)[colnames(data_test)==L] <- paste("label")

  data_trainm <- data_train[,colnames(data_train)!=L]
  data_testm <- data_test[,colnames(data_test)!=L]

  train_label <- data_train$label
  test_label <- data_test$label

  # feature selection
  if(nf < ncol(data_trainm) & nf>0) {
    nf = round(min(ncol(data_train),nf))

    # rank feature by ttest
    indx <- rankfeature(L,data_train,classes,nf)
    data_trainm <- data_trainm[,indx]
    data_testm <- data_testm[,indx]
  }


  # feature map
  if(feature_type=="calculus"){
    new_data <- new_feature_type1(data_trainm,train_label,data_testm,classes,p,corr,powerS)
  }


  # network classifier with sub-networks
  if(feature_type=="smoothness"){
    if(nc==1){
      new_data <- new_feature_type2(data_trainm,train_label,data_testm,classes,p,corr,powerS,vars)
    } else{
      new_data <- new_feature_type3(data_trainm,train_label,data_testm,classes,p,corr,powerS,nc)
    }

  }

  # remove na and inf
  new_data <- data.frame(log(new_data))

  is.na(new_data) <- sapply(new_data, is.infinite)
  is.na(new_data) <- sapply(new_data, is.nan)
  ind_na <- colSums(is.na(new_data))==0
  new_data <- new_data[,ind_na]

  xx = seq(from=1,to=nrow(data_train),by=1)

  new_train <- new_data[xx,]
  new_test <- new_data[-xx,]

  return(list(new_train = new_train, new_test = new_test, train_label = train_label, test_label = test_label))

}
