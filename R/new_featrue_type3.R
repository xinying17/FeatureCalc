new_feature_type3 <- function(data_trainm,train_label,data_testm,classes,p,corr,powerS,normal,nc){

  if(missing(p)) p=0;
  if(missing(corr)) corr=0;
  if(missing(powerS)) powerS=1;
  if(missing(normal)) normal=1;
  if(missing(nc)) nc=1;

  # network classifier with 2*nc networks

  train_nets <- structure(list(types = character(),
                               featureIDX = list(),
                               nets = list()))

  require(igraph)
  # build network for each class
  aa = 1
  for(t in classes){
    class_train <- data_trainm[train_label==t,]
    corr <- cor(class_train)
    g=graph.adjacency(abs(corr), mode="plus",weighted=TRUE)
    fc <- fastgreedy.community(g)
    clusterCut <- fc$membership
    nc = max(clusterCut)
    for(i in 1:nc){
      x = data.frame(class_train[,clusterCut==i])
      if(ncol(x)>2){
        nets <- network_build(as.matrix(x), p, corr)
        train_nets$types[[aa]] <- t
        train_nets$featureIDX[[aa]] <- colnames(x)
        train_nets$nets[[aa]] <- nets
        train_nets$means[[aa]] <- colMeans(x)
        aa = aa+1
      }
    }

  }
  new_train <- matrix(nrow = nrow(data_trainm),ncol = length(train_nets$types))
  new_test <- matrix(nrow = nrow(data_testm),ncol = length(train_nets$types))

  # new train data


  for(b in 1:length(train_nets$types)){
    nets <- train_nets$nets[[b]]
    if(normal==1){
      centroid <- train_nets$means[[b]]
      data_trainmx = as.matrix(t(t(data_trainm[,train_nets$featureIDX[[b]]])-centroid))
      data_testmx = as.matrix(t(t(data_testm[,train_nets$featureIDX[[b]]])-centroid))
    } else{
      data_trainmx = data_trainm[,train_nets$featureIDX[[b]]]
      data_testmx = data_testm[,train_nets$featureIDX[[b]]]
    }
    smooth_value1 <- smoothness(Lap = nets$laplacian,data_trainmx,powerS)
    new_train[,b] <- smooth_value1

    smooth_value2 <- smoothness(nets$laplacian,data_testmx,powerS)
    new_test[,b] <- smooth_value2
  }

  new_data <- rbind(new_train,new_test)

  return(new_data)
}
