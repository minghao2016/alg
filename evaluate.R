#######################################################################################

##################     CLASSIFICATION         #########################################

#######################################################################################


select_columns <- function(df, target, ind){
  
  goods <- df %>% select(target)
  #goods <- as.logical(as.numeric(as.character(goods$target)))
  
  cnames <- colnames(df)
  cnames <- cnames[-which(cnames==target)]
  
  selected_columns <- cnames[as.logical(ind)]
  
  df <- df %>% select(selected_columns)
  colnames(df)
  #df<- df %>% select(-GOOD) 
  df <- df %>% dummy.data.frame() 
  df <- cbind(df,goods)
  #df <- na.omit(df,cols=target)
  return(df)
}

head(df)


perform_classification <- function(df, target, remove_NA=TRUE){
  
  if(remove_NA==TRUE){
    df <- na.omit(df,cols=target)
  }
  ndf <- normalizeFeatures(df, target = target)
  
  smp_size = floor(0.75*nrow(df))
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size )
  
  train_dat <- df[train_ind,]
  test_dat <- df[-train_ind,]
  
  trainTask <- makeClassifTask(data = train_dat, target = target, positive=1)
  testTask <- makeClassifTask(data = test_dat, target = target)
  
  set.seed(1)
  
  xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "response",
    par.vals = list(
      objective = "binary:logistic",
      eval_metric = "error",
      nrounds = 200
    )
  )
  xgb_model <- train(xgb_learner, task = trainTask)
  result <- predict(xgb_model, testTask)
}


#######################################################################################

##################   OBJECTIVE FUNCTIONS     ##########################################

#######################################################################################

accuracy <- function(pred_result){
  ans <- sum(pred_result$data$truth == pred_result$data$response) / nrow(pred_result$data)
  return(ans)
}


get_precision <- function(res){
  cm <- confusionMatrix(res$data$truth, res$data$response)
  precision <- as.numeric(cm$byClass["Precision"])
  return(precision)
}



#######################################################################################

##################            WRAPPER         #########################################

#######################################################################################



evaluate_ind <- function(ind, df, target, objectives){
  
  dat <- select_columns(df, target, ind)
  res <- perform_classification(dat, target)
  
  obj_vals <- data.frame()
  #obj_names <- c()
  for(i in 1:length(objectives)){
    x <- objectives[[i]](res)
    obj_vals[1,i] <- x
    #  obj_names[i] <- as.character(substitute(objectives[[i]]))
  }
  n <- length(obj_vals)+1
  obj_vals[1,n]<- sum(ind)
  #obj_names[n] <- "n features"
  
  #colnames(obj_vals) <- obj_names
  
  return(obj_vals)
}


evaluate_population <- function(pop, df, target, objectives){
  evaluated_pop <- data.frame()
  
  for(i in 1:length(pop)){
    ind <- pop[[i]]
    evaluated_ind <- evaluate_ind(ind, df, target, objectives)
    rownames(evaluated_ind)<-i
    
    evaluated_pop <- rbind(evaluated_pop, evaluated_ind)
  }
  return(evaluated_pop)
}


