
#######################################################################################

##################            ALGORITHM            ####################################

#######################################################################################




alg <- function(df, target, obj_list, obj_names, 
                pareto, n, max_gen,
                model,
                resampling,
                num_features = TRUE,
                mutation_rate=0.1,
                feature_cost = FALSE){  
  
  start_time <- Sys.time()
  print("Initializing algorithm ...")
  #m = number of objective functions
  m <- length(obj_list)+num_features
  
  print("- Generating reference points...")
  #generate reference points
  rp <- ref_points(m)
  
  print("- Generating initial population...")
  #generating initial population
  initial_pop <- generate_init_pop(df, n)    
  
  #getting values for objective functions
  evaluated_pop <- evaluate_population(pop = initial_pop, df = df, target = target, 
                                       objectives = obj_list, 
                                       model = model,
                                       resampling = resampling,
                                       num_features = num_features,
                                       feature_cost = feature_cost)
  colnames(evaluated_pop)<-obj_names
  
  current_generation <- 0
  
  pop <- initial_pop
  
  print("Performing iterations: ")
  
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
    
    
    
    plt <- plot_ly(sorted_evaluated_comb_pop, x=~mshare, y=~emp, z=~nf,
                   color = ~.level,
                   type="scatter3d", mode = 'markers')
    print(plt)
    #ideal_point <- compute_ideal_point(sorted_evaluated_comb_pop)[-length(sorted_evaluated_comb_pop)]
    current_generation <- current_generation + 1
    
    print(paste0("- Iteration ", current_generation, "/", max_gen))
    #print(paste("  Ideal point: ",ideal_point, collapse = " : "))
    
  }
  
  result <- prep_output(pop, evaluated_pop)
  
  end_time <- Sys.time()
  print(paste("Time: ",end_time - start_time))
  
  
  return(result)
}
