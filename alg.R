library("rPref")
library("plotly")
library("dplyr")

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
  sorted_pop <- psel(pop, pareto_criteria, top = nrow(pop), show_level = TRUE)
  return(sorted_pop)
}

#3D PLOT PARETO FRONTS
plot_ly(sorted_pop, x=~obj1, y=~obj2, z=~obj3, 
        type="scatter3d", mode="markers", color=~.level)


#CROSSOVER

crossover <- function(ind1, ind2){
  child <- vector(length=length(ind1))
  
  gene_a <- as.integer(runif(1,0,1)*length(ind1))
  gene_b <- as.integer(runif(1,0,1)*length(ind1))
  
  start_gene <- max(min(gene_a, gene_b),1)
  end_gene <- min(max(gene_a, gene_b), length(ind1))
  
  print(start_gene)
  print(end_gene)
  
  for(i in start_gene:end_gene){
    child[i] <- ind1[i]
  }
  
  if(start_gene != 1){
    for(i in 1:(start_gene-1)){
      child[i] <- ind2[i]
    }
  }
  if(end_gene!=length(ind1)){
    for(i in (end_gene+1):length(ind1)){
      child[i] <- ind2[i]
    }
  }
  return(child)
}

create_children <- function(mating_pool){
  children <- list()
  len <- length(mating_pool)
  for(i in 1:(round(len/2,0))){
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

mutate_pop <- function(pop, mutation_rate){
  mutated_pop <- mpop <- lapply(pop,mutate_ind,mutation_rate)
  return(mutated_pop)  
}



#PREDICTION



prep_dataset <- function(dat, smp_size=0.75, ind_features, target){
  target_dat <- dat %>% select(target) 
  fdat <- dat %>% select(-target)
  fdat <- fdat[,as.logical(ind_features)]
  tr_dat <- cbind(target_dat, fdat)
  norm_dat <- normalizeFeatures(tr_dat, target)
  
  smp_size = floor(smp_size*nrow(niris))
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(norm_dat)), size = smp_size )
  
  train_dat <- norm_dat[train_ind,]
  test_dat <- norm_dat[-train_ind,]
  
  trainTask <- makeClassifTask(data = train_dat, target = target)
  testTask <- makeClassifTask(data = test_dat, target = target)
  res <- list(trainTask, testTask)
  return(res)
}

set.seed(1)

#NEED TO WORK ON PARAMS
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    eval_metric = "merror",
    nrounds = 200
  )
)

xgb_model <- train(xgb_learner, task = trainTask)

pred_result <- predict(xgb_model, testTask)

accuracy <- function(pred_result){
  ans <- mean(pred_result$data$truth == pred_result$data$responce)
  return(ans)
}

head(pred_result$data)

#CLASSIFY

classify <- function(ind, dat, target, smp_size=0.75){
  dat <- prep_dataset(dat=dat, ind_features = ind, target = target)
  trainTask <- dat[[1]]
  testTask <- dat[[2]]
  xgb_model <- train(xgb_learner, task = trainTask)
  
  pred_result <- predict(xgb_model, testTask)
  return(pred_result)
}

#EVALUATE
evaluate_objectives <- function(pred_result, ind){
  n_features <- sum(ind)
  acc <- accuracy(pred_result)
  
  ind_eval <- list(ind,n_features,acc)
  return(ind_eval)
}

evaluation_step <- function(pop){
  pop_eval <- vector("list", length = length(pop))
  
  for(i in 1:length(pop)){
    ind <- pop[[i]]
    ind_eval <- ind %>%
                classify(dat) %>%
                evaluate_objectives(ind)
    pop_eval[[i]] <- ind_eval
  }
}
