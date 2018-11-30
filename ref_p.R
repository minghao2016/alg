gen_ref_points <- function(n_obj){
  m <- n_obj #number of objective functions
  p <- m+1   #number of divisions per axis
  a <- m+p-1 #auxilary
  
  n <- factorial(a)/(factorial(p)*factorial(a-p))
  #print(n)
  
  d <- n/m-1 #divisions per side
  #print(d)
  
  step <- 1/d
  
  options <- c(0,1)
  
  for(i in 1:d){
    options[2+i] <- i*step
  }
  #print(options)
  
  n_elems <- (length(options)-1)*m
  #print(n_elems)
  
  ref_points <- matrix(ncol = m)
  
  while(nrow(unique(ref_points)) < n){
    
    point <- vector(length = m)
    for(i in 1:m){
      point[i] <- sample(options,1)
    }
    #print(point)
    if(sum(point)==1){
      ref_points <- rbind(ref_points,point)
      
    }
    
  }
  ref_points <- unique(ref_points)
  return(ref_points)
}

ans <- gen_ref_points(3)
p <- plot_ly(x=ans[,1], y=ans[,2], z=ans[,3], type='scatter3d', mode = 'markers')
