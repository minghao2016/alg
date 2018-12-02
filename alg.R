library("caret")    #used only for confusionMatrix(), would gladly replace it
library("rPref")
library("plotly")
library("dplyr")
library("xgboost")
library("mlr")
library("dummies")


#INITIAL POPULATION

generate_ind <- function(len){
  ind <- sample(0:1, size = len, replace = TRUE)
  return(ind)
} 

generate_init_pop <- function(data, size){
  len <- ncol(data)-1
  population <- list()
  for(i in 1:size){
    population[[i]] <- generate_ind(len)
  }
  #population <- rep(generate_ind(len), size)
  return(population)
}

#NON-DOMINATED SORTING

non_dom_sort <- function(pop, pareto_criteria){
  #works with data frames only
  sorted_pop <- psel(pop, pareto_criteria, top = nrow(pop), show_level = TRUE)
  return(sorted_pop)
}

#3D PLOT PARETO FRONTS
plot_pareto_3d <- function(sorted_pop, x, y, z){ 
  plt <- plot_ly(sorted_pop, x=x, y=y, z=z, 
        type="scatter3d", mode="markers", color=~.level)
  return(plt)
}


#######################################################################################

##################     CROSSOVER AND MUTATION   #######################################

#######################################################################################



#CROSSOVER

crossover <- function(ind1, ind2){
  child <- vector(length=length(ind1))
  for(i in 1:length(child)){
    if(runif(1,0,1)>0.5){
      child[i] <- ind1[i]
    } else {
      child[i] <- ind2[i]
    }
  }
  return(child)
}


create_children <- function(mating_pool){
  children <- list()
  len <- length(mating_pool)
  #for(i in 1:(round(len/2,0))){
  for(i in 1:len){
    children[[i]] <- crossover(mating_pool[[i]],mating_pool[[(len-i+1)]])
  }
  return(children)
}


#MUTATION


mutate_ind <- function(ind, mutation_rate){
  for(i in 1:length(ind)){
    if(runif(1,0,1) < mutation_rate){
      ind[i] <- as.integer(!ind[i])
    }
  }
  return(ind)
}

mutate_pop <- function(pop, mutation_rate=0.1){
  mutated_pop <- mpop <- lapply(pop,mutate_ind,mutation_rate)
  return(mutated_pop)  
}



#PREDICTION


#######################################################################################

##################     EVALUATION STEP       #########################################

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




evaluate_ind <- function(ind, df, target, objectives){
  
  dat <- select_columns(df, target, ind)
  res <- perform_classification(dat, target)
  
  get_objective_values <- function(a) {
    # call each function to a
    lapply(objectives, function(f) f(a))
  }
  
  ans <- get_objective_values(res)
  
  obj_vals <- data.frame()
  
  for(i in 1:length(ans)){
    obj_vals[1,i] <- ans[[i]]
  }
  n <- length(obj_vals)+1
  obj_vals[1,n]<- sum(ind)
  
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


