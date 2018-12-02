#######################################################################################

#######################################################################################

##################   NORMALISATION ALGORITHM #########################################

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
  f_n <- f/a
  return(f_n)
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
  res <- matrix(ncol=4, nrow=nrow(rp))
  for(i in 1:nrow(rp)){
    d <- dist(rbind(point,rp[i,]))
    res[i,c(1,2,3)] <- rp[i,]
    res[i,4] <- d
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

