
#######################################################################################

##################            ALGORITHM            ####################################

#######################################################################################




alg <- function(df, target, obj_list, obj_names, 
                pareto, n, max_gen,
                model,
                resampling,
                num_features = TRUE,
                mutation_rate=0.1,
                threshold = 0.5,
                feature_cost = FALSE){  
  
  start_time <- Sys.time()
  print("Initializing algorithm ...")
  
  
  #replace target column at the end
  forder <- colnames(df)
  forder <- forder[-which(forder == target)]
  forder <- c(forder, target)
  df <- df[,forder]
  
  #m = number of objective functions
  m <- length(obj_names)
  
  print("- Generating reference points...")
  #generate reference points
  rp <- ref_points(m)
  
  print("- Generating initial population...")
  #generating initial population
  initial_pop <- generate_init_pop(df, n)    
  
  
  #Measure approx time per loop
  
  eval_start <- Sys.time()
  
  #getting values for objective functions
  evaluated_pop <- evaluate_population(pop = initial_pop, df = df, target = target, 
                                       objectives = obj_list, 
                                       model = model,
                                       resampling = resampling,
                                       num_features = num_features,
                                       feature_cost = feature_cost)
  colnames(evaluated_pop)<-obj_names
  
  eval_end <- Sys.time()
  print(paste("Init evaluation time: ",capture.output(eval_end - eval_start)))
  print(paste("Approx. run time: ",capture.output((eval_end - eval_start)*max_gen)))
              
  
  current_generation <- 0
  
  pop <- initial_pop
  
  print("Performing iterations: ")
  
  all_gens <- list()
  
  while(current_generation < max_gen){
    
    
    # assigning new id's to previously selected pouints
    rownames(evaluated_pop) <- 1:nrow(evaluated_pop)
    
    #crossover
    children <- create_children(pop) 
    
    #mutation
    mutated_children <- mutate_pop(children, 0.1)
    
    #evaluate obj fns for children
    evaluated_children <- evaluate_population(pop = mutated_children,df = df, target = target, 
                                              objectives = obj_list, 
                                              model = model,
                                              num_features=num_features,
                                              feature_cost = feature_cost)
    colnames(evaluated_children) <- obj_names
    rownames(evaluated_children) <- (length(pop)+1):(length(pop)+length(children))
    
    #combine parent and child
    combined_pop_individuals <- c(pop, mutated_children) #individuals with actual binary vectors
    
    evaluated_comb_pop <- rbind(evaluated_pop,evaluated_children) #id's of individuals with obj funs values
    
    #non-dominated sort
    sorted_evaluated_comb_pop <- non_dom_sort(evaluated_comb_pop, pareto)
    
    #select individs for next generation
    res <- select_next_generation(sorted_evaluated_comb_pop, combined_pop_individuals, rp, n)
    pop <- res[[1]]
    evaluated_pop <- res[[2]]

#print(sorted_evaluated_comb_pop[sorted_evaluated_comb_pop$.level ==1,])    
#print(non_dom_sort(evaluated_pop, pareto))
    

    plt <- view_pareto(sorted_evaluated_comb_pop, rp)
    print(plt)
    
    #ideal_point <- compute_ideal_point(sorted_evaluated_comb_pop)[-length(sorted_evaluated_comb_pop)]
    current_generation <- current_generation + 1
    
    
    #test_pop <- list(evaluated_pop, pop)
    #all_gens[current_generation] <- test_pop
    
    #hypervolume
    invisible(capture.output(hpvlm <- hypervolume(sorted_evaluated_comb_pop[which(sorted_evaluated_comb_pop$.level==1),1:m])))
    
    print(paste0("- Iteration ", current_generation, "/", max_gen, 
                 "   |   Hypervolume: ", hpvlm@Volume))
    
    #print(paste("  Ideal point: ",ideal_point, collapse = " : "))
    
  }
  
  end_time <- Sys.time()
  
  ex_time <- end_time - start_time
  
  result <- prep_output(pop. = pop, evaluated_pop. = evaluated_pop, 
                        df. = df, threshold = threshold,
                        target = target, objectives = obj_list, 
                        obj_names. = obj_names,
                        model = model, pareto = pareto,
                        resampling. = resampling,
                        num_features = num_features,
                        feature_cost = feature_cost,
                        ex_time = ex_time)
  
  
  print(paste("Time: ",end_time - start_time))
  
  #abc <- list(result, all_gens)
  return(result)
}
