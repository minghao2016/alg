
#######################################################################################

##################            PREP OUTPUT          ####################################

#######################################################################################




maj_vote <- function(pop){
  cols <- lapply(pop,as.logical)
  res <- data.frame()
  for(i in 1:length(cols)){
    for(j in 1:20){
      res[i,j] <- pop[[i]][j]
    }
  }
  votes <- data.frame()
  names <- colnames(df)[-length(df)]
  for(i in 1:20){
    votes[i,1] <- names[i]
    votes[i,2] <- sum(res[,i])/nrow(res)
  }
  colnames(votes) <- c("feature", "vote")
  return(votes)
}

prep_output <- function(pop., evaluated_pop.){
  rownames(evaluated_pop.) <- 1:nrow(evaluated_pop.)
  sorted_fin_pop <- non_dom_sort(evaluated_pop., pareto)
  pf <- sorted_fin_pop[which(sorted_fin_pop$.level==1),]
  pf <- pf[,-ncol(pf)]
  ids <- rownames(pf)
  ids <- sapply(ids, as.integer)
  
  top_gen = pop.[ids]
  
  
  votes <- maj_vote(top_gen) 
  result <- list(top_gen,pf,votes)
  names(result) <- c("fin_pop", "fin_pop_fitness", "maj_vote")
  return(result)
}
