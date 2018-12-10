if (require(pacman) == F) install.packages("pacman")
library("pacman")
p_load("rPref", "plotly", "dplyr", "xgboost", "mlr" ,'EMP' )

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)


#INITIAL POPULATION

generate_ind <- function(len, p){
  ind <- sample(0:1, size = len, replace = TRUE, prob = c(p,1-p))
  return(ind)
} 


generate_init_pop <- function(data, size){
  
  len <- ncol(data)-1
  population <- list()
  probs <- seq(0.1,0.9,length.out = size)
  
  for(i in 1:size){
    population[[i]] <- generate_ind(len,probs[i])
  }
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
  if(sum(ind) == 0){
    i <- sample(length(ind))
    ind[i] <- 1
  }
  return(ind)
}

mutate_pop <- function(pop, mutation_rate=mutation_rate){
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
  df <- cbind(df,goods)
    #df<- df %>% select(-GOOD) 
  df <- df %>% createDummyFeatures(target = target, method = 'reference') 
 

  #df <- na.omit(df,cols=target)
  return(df)
}



perform_classification <- function(df, target, model, resampling., remove_NA=TRUE){
  
  if(remove_NA==TRUE){
    df <- na.omit(df,cols=target)
  }
  ndf <- normalizeFeatures(df, target = target)
  
  smp_size = floor(0.75*nrow(df))
  
  set.seed(123)
  
  trainTask <- makeClassifTask(data = df, target = target, positive=1)

  set.seed(1)
  
  learner <- model
  
  rdesc <- resampling

  mlr_model <- train(learner, task = trainTask)
  
  pred <- resample(xgb_learner, trainTask, rdesc, show.info = FALSE,
                   measures = list(mmce, fpr, fnr, timetrain))
  res <- pred$pred
  return(res)
}


evaluate_ind <- function(ind, df, target, objectives, model = model, 
                         resampling. = resampling,
                         num_features = num_features){
  
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
  
  return(obj_vals)
}


evaluate_population <- function(pop, df, target, objectives, 
                                model = model,
                                resampling = resampling,
                                num_features = num_features){
  evaluated_pop <- data.frame()
  
  for(i in 1:length(pop)){
    ind <- pop[[i]]
    evaluated_ind <- evaluate_ind(ind, df, target, objectives, model = model, 
                                  resampling = resampling, 
                                  num_features = num_features)
    rownames(evaluated_ind)<-i
    
    evaluated_pop <- rbind(evaluated_pop, evaluated_ind)
  }
  return(evaluated_pop)
}


#######################################################################################

#######################################################################################

##################   SELECTION ALGORITHM      #########################################

#######################################################################################

#######################################################################################

#Input: Pareto Front L, number of vacant spots "k",  


compute_ideal_point <- function(pareto_front){
  z_hat <- vector(length=ncol(pareto_front))
  for(i in 1:length(z_hat)){
    z_hat[i] <- min(pareto_front[,(i)])
  }
  return(z_hat)
}

translate_objectives <- function(pareto_front, ideal_point){
  for(i in 1:length(ideal_point)){
    for(j in 1:length(pareto_front[,i])){
      pareto_front[j,i] <- pareto_front[j,i] - ideal_point[i]
    }
  }
  return(pareto_front)
}

#different from paper. Just delivers the extrem points
#description of the achievement scalarizing function (ASF) in paper is not clear

get_extreme_points <- function(t_pareto_front){
  df <- t_pareto_front
  extreme_points <- vector("list",length=(length(t_pareto_front)))
  for(i in 1:length(extreme_points)){
    extreme_points[[i]] <- df[which.max(df[,i]),]
  }
  return(extreme_points)
}

#part-time sol, to be replaced

get_intercepts <- function(extreme_points){
  intercept <- vector(length=length(extreme_points))
  for(i in 1:length(intercept)){
    intercept[i] <- extreme_points[[i]][[i]]
  }
  return(intercept)
}


#a is already calculated from the f_prime, so no need to substract z_min
normalize <- function(f,a,z_min){
  if(a==0){
    f_n <- f/0.000001
  } else {
  f_n <- f/a
  return(f_n)
  }
}

normalize_objectives <- function(front, intercept, ideal_point){
  for(i in 1:ncol(front)){
    for(row in 1:nrow(front)){
      front[row,i] <- normalize(front[row,i], intercept[i], ideal_point[i])
    }
  }
  return(front)
}



#######################################################################################

##################   REFERENCE POINTS         #########################################

#######################################################################################




ref_points <- function(n_objectives){
  m <- n_objectives
  p <- m+1
  
  #N = number of points
  a <- m+p-1
  n <- factorial(a)/(factorial(p)*factorial(a-p))
  
  #divisions per side
  d <- n/m-1
  step <- 1/d
  
  options <- c(0,1)
  for(j in 1:d){
    options[(2+j)]<- step*j
  }
  
  point <- vector(length=m)
  point[1]<-1
  for(i in 2:m){
    point[i]<-0
  }
  set <- rbind(point)
  
  while(nrow(unique(set))<n){
    point <- vector(length=m)
    for(col in 1:m){
      point[col] <- sample(options,1)
    }
    if(sum(point)==1){
      set <- rbind(set,point)
    }
  }
  set <- unique(set)
  return(set)
}



#######################################################################################

##################            NICHING         #########################################

#######################################################################################



find_ref_point <- function(point, rp){
  res <- matrix(ncol=ncol(point)+1, nrow=nrow(rp))
  for(i in 1:nrow(rp)){
    d <- dist(rbind(point,rp[i,]))
    res[i,1:(ncol(res)-1)] <- rp[i,]
    res[i,ncol(res)] <- d
  }
  ref <- rp[which.min(res[,4]),]
  ans <- which.min(res[,4])
  
  return(ans)
}

gen_refs <- function(data, rp){
  ref <- data.frame()
  for(point in 1:nrow(data)){
    p_ref <- find_ref_point(data[point,], rp)
    ref <- rbind(ref,c(p_ref,point))
  }
  colnames(ref) <- c("rp", "data")
  return(ref)
}

sel_points <- function(ref_list, dat, k){
  ref_list <- data.frame(ref_list)
  u <- unique(ref_list[,1])
  r <- data.frame()
  
  for(i in u){
    c <- length(ref_list[ref_list$rp==i,1])
    x <- c(i,c)
    r <- rbind(r,x)
  } 
  colnames(r) <- c("rp", "count")
  r <- r[order(r$count),]
  
  points <- c()
  while(length(points)< k){
    for(i in 1:nrow(r)){
      val <- r[i,1]
      point <- ref_list[ref_list$rp==val,][,2]
      if(length(point)>1){
        point <- sample(point,1)
      }
      if(point %in% points){next}
      else{
        points[length(points)+1] <- point 
      }
    }
  }
  res <- dat[points,]
  return(res)
}

execute_selection <- function(pf, k){
  m <- ncol(pf)
  
  ip <- compute_ideal_point(pf) #ip = ideal point
  tpf <- translate_objectives(pf, ip) #tpf = translated pareto front
  ep <- get_extreme_points(tpf) #ep = exteme points
  inter <- get_intercepts(ep) #intercept
  npf <- normalize_objectives(tpf, inter, ip) #normalized pareto front
  #colnames(npf) <- c("x", "y", "z")
  
  rp <- ref_points(m)
  
  selected_points <- npf %>% gen_refs(rp) %>% sel_points(npf,k)
  return(selected_points)
}




#######################################################################################

##################            ALGORITHM BUILDER    ####################################

#######################################################################################




#iterator for selecting points from current generation


#iterator for selecting points from current generation
select_next_generation <- function(sorted_comb_pop, combined_pop, rp, n){
  next_pop = c()
  #n <- 50
  lvl <- 1
  while(length(next_pop) != n){
    
    pf <- sorted_comb_pop[which(sorted_comb_pop$.level==lvl),]
    pf <- pf[,-ncol(pf)]
    
    len <- length(next_pop)
    
    if((nrow(pf)+len) <= n){
      
      for(i in 1:nrow(pf)){
        next_pop[len+i] <- rownames(pf)[i]
      }
      
      lvl <- lvl+1
      
    } else {
      k <- n-len
      selected_points <- execute_selection(pf, k)
      
      for(i in 1:k){
        next_pop[len+i] <- rownames(selected_points)[i]
      }
    }
  }
  
  next_gen = list()
  eval_next_gen = data.frame()
  for(i in 1:length(next_pop)){
    id <- as.integer(next_pop[i])
    next_gen[[i]] <- combined_pop[[id]]
    eval_next_gen <- rbind(eval_next_gen, sorted_comb_pop[id,-ncol(sorted_comb_pop)])
  }
  ans <- list(next_gen, eval_next_gen)
  return(ans)
}



#wrap 


alg <- function(df, target, obj_list, obj_names, 
                pareto, n, max_gen,
                model,
                resampling,
                num_features = TRUE,
                mutation_rate=0.1){  
  
  #m = number of objective functions
  m <- length(obj_list)+1
  
  #generate reference points
  rp <- ref_points(m)
  
  #generating initial population
  pop <- generate_init_pop(df, n)    
  
  #getting values for objective functions
  epop <- evaluate_population(pop = pop,df = df, target = target, 
                              objectives = obj_list, 
                              model = model,
                              resampling = resampling,
                              num_features = num_features)
  colnames(epop)<-obj_names
  
  print(epop)
  
  current_generation <- 0
  
  while(current_generation < max_gen){
    
    # assigning new id's to previously selected pouints
    rownames(epop) <- 1:nrow(epop)
    
    #crossover
    children <- create_children(pop) 
    
    #mutation
    mchildren <- mutate_pop(children, 0.1)
    
    #evaluate obj fns for children
    echildren <- evaluate_population(pop = mchildren,df = df, target = target, 
                                     objectives = obj_list, 
                                     model = model,
                                     num_features=num_features)
    colnames(echildren) <- obj_names
    rownames(echildren) <- (length(pop)+1):(length(pop)+length(children))
    
    #combine parent and child
    combined_pop <- c(pop, mchildren) #individuals with actual binary vectors
    
    comb_pop <- rbind(epop,echildren) #id's of individuals with obj funs values
    
    #non-dominated sort
    sorted_comb_pop <- non_dom_sort(comb_pop, pareto)
    
    #select individs for next generation
    res <- select_next_generation(sorted_comb_pop, combined_pop, rp, n)
    pop <- res[[1]]
    epop <- res[[2]]
    
    print("Selected generation")
    print(epop)
    
    current_generation <- current_generation + 1
    
    print(current_generation)
  }
  result <- pop
  return(result)
}
