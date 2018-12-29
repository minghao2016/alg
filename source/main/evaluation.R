
#######################################################################################

##################     EVALUATION STEP       #########################################

#######################################################################################



select_columns <- function(df, target, ind){
  
  goods <- data.frame(df[,c(target)])
  colnames(goods) <- target
  
  cnames <- colnames(df)
  cnames <- cnames[-which(cnames==target)]
  
  selected_columns <- cnames[as.logical(ind)]
  
  df <- df[,selected_columns]
  df <- cbind(df,goods)
  df <- createDummyFeatures(df, target = target, method = 'reference') 
  
  return(df)
}


perform_classification <- function(df, target, model, resampling., remove_NA=TRUE){
  
  if(remove_NA==TRUE){
    df <- na.omit(df,cols=target)
  }
  ndf <- normalizeFeatures(df, target = target)
  
  smp_size = floor(0.75*nrow(df))
  
  #set.seed(123)
  
  trainTask <- makeClassifTask(data = df, target = target, positive=1)
  
  set.seed(1)
  
  learner <- model
  
  rdesc <- resampling
  
  pred <- resample(learner, trainTask, rdesc, show.info = FALSE,
                   measures = list(mmce, fpr, fnr, timetrain))
  res <- pred$pred
  return(res)
}


evaluate_ind <- function(ind, df, target, objectives, model = model, 
                         resampling. = resampling,
                         num_features = num_features,
                         feature_cost = feature_cost){
  
  dat <- select_columns(df, target, ind)
  res <- perform_classification(dat, target, model = model, 
                                resampling = resampling)
  
  get_objective_values <- function(a) {
    # call each function to a
    lapply(objectives, function(f) f(a))
  }
  
  ans <- get_objective_values(res)
  
  obj_vals <- data.frame()
  
  for(i in 1:length(ans)){
    obj_vals[1,i] <- ans[[i]]
  }
  
  if(num_features == TRUE){
    n <- length(obj_vals)+1
    obj_vals[1,n]<- sum(ind)
  }
  
  #if(feature_cost != FALSE){
    if(any(feature_cost)){
      cost <- sum(feature_cost[as.logical(ind)])
      n <- length(obj_vals)+1
      obj_vals[1,n]<- cost    
    }
 # }
  
  return(obj_vals)
}


evaluate_population <- function(pop, df, target, objectives, 
                                model = model,
                                resampling = resampling,
                                num_features = num_features,
                                feature_cost = feature_cost){
  evaluated_pop <- data.frame()
  
  for(i in 1:length(pop)){
    ind <- pop[[i]]
    evaluated_ind <- evaluate_ind(ind, df, target, objectives, model = model, 
                                  resampling = resampling, 
                                  num_features = num_features,
                                  feature_cost = feature_cost)
    rownames(evaluated_ind)<-i
    
    evaluated_pop <- rbind(evaluated_pop, evaluated_ind)
  }
  return(evaluated_pop)
}

