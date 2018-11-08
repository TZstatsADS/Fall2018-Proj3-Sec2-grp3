xgb_train <- function(dat_train, label_train, par=NULL){

  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("xgboost")
  
  ### creat model list
  modelList <- list()
  
  ### Train gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  # library(doParallel)
  # cl <- makeCluster(2)
  # registerDoParallel(cl)
  # 
  # foreach(i=1:12, .combine=combine, .packages='xgboost') %dopar% {
    
  for(i in 1:12){
    ## calculate column and channel
    cat("i=", i, "\n")
    
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_xgb <- xgboost(data=featMat, label=labMat,
                       eta=0.5,
                       max.depth=depth,
                       min_child_weight=10,
                       subsample = 0.5,
                       colsample_bytree = 0.8,
                       nthread = 3, 
                       nrounds = 200,
                       early_stopping_rounds = 20,
                       objective = "reg:linear",
                       verbose=0)
    modelList[[i]] <- list(fit=fit_xgb)
  }
  
  return(modelList)
}