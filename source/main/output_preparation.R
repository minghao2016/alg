
#######################################################################################

##################            PREP OUTPUT          ####################################

#######################################################################################




maj_vote <- function(pop, df.){
  
  nfeatures <- length(pop[[1]])
  cols <- lapply(pop,as.logical)
  res <- data.frame()
  for(i in 1:length(cols)){
    for(j in 1:nfeatures){
      res[i,j] <- pop[[i]][j]
    }
  }
  votes <- data.frame()
  names <- colnames(df.)[-length(df.)]
  for(i in 1:nfeatures){
    votes[i,1] <- names[i]
    votes[i,2] <- sum(res[,i])/nrow(res)
  }
  colnames(votes) <- c("feature", "vote")
  
  return(votes)
}

evaluate_maj_vote <- function(vote, threshold,
                         df., target = target, objectives, model = model, 
                         resampling. = resampling,
                         num_features = num_features,
                         feature_cost = feature_cost){
  
  for(i in 1:nrow(vote)){
    if(vote[i,]$vote >= threshold){
      vote[i,]$vote <- 1
    } else {
      vote[i,]$vote <- 0
    }
  }
  ind <- vote$vote
  
  evaluated <- evaluate_ind(ind, df = df., target, objectives, model = model, 
                         resampling. = resampling,
                         num_features = num_features,
                         feature_cost = feature_cost)
  
  return(evaluated)
}

output_per_individual <- function(df., pop, objective_vals){
  feature_names <- colnames(df.)[1:ncol(df.)-1]
  
  rownames(objective_vals) <- 1:nrow(objective_vals)
  
  individuals <- list()
  for(i in 1:nrow(objective_vals)){
    
    values <-  objective_vals[i,]
    features <-  feature_names[as.logical(pop[[i]])]
    element <- list(values, features)
    
    individuals[i] <- list(element) 
  }
  
  return(individuals)
}



prep_output <- function(pop., evaluated_pop., df., threshold,
                        target, objectives, model = model,
                        pareto = pareto, obj_names.,
                         resampling. = resampling,
                         num_features = num_features,
                         feature_cost = feature_cost){
  
  rownames(evaluated_pop.) <- 1:nrow(evaluated_pop.)
  sorted_fin_pop <- non_dom_sort(evaluated_pop., pareto)
  pf <- sorted_fin_pop[which(sorted_fin_pop$.level==1),]
  pf <- pf[,-ncol(pf)]
  ids <- rownames(pf)
  ids <- sapply(ids, as.integer)
  
  top_gen = pop.[ids]
  
  raw <- list(top_gen, pf)
  names(raw) <- c("ind", "objective_values")
  
  per_ind <- output_per_individual(df. = df., top_gen, pf)
  
  
  votes <- maj_vote(top_gen, df.) 
  print(votes)
  features <- votes[which(votes$vote >= threshold),]$feature
  evaluated_vote <- evaluate_maj_vote(votes, threshold = threshold,
                         df. = df., target, objectives, model = model, 
                         resampling. = resampling,
                         num_features = num_features,
                         feature_cost = feature_cost)
  colnames(evaluated_vote) <- obj_names.
  
  majority_vote <- list(votes, features, evaluated_vote)
  names(majority_vote) <- list("votes", "features", "objective_values")
  
  
  result <- list(raw, per_ind, majority_vote)
  
  names(result) <- c("pf_raw", "per_ind", "majority_vote")
  return(result)
}
