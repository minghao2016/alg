#Assignment and niching

#Euclidean distance 

#dist(rbind(p1,p2))

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
  u <- unique(ref_list[,1])
  r <- data.frame()
  
  for(i in u){
    c <- length(a[a$rp==i,1])
    x <- c(i,c)
    r <- rbind(r,x)
  } 
  colnames(r) <- c("rp", "count")
  r <- r[order(r$count),]
  
  points <- c()
  while(length(points)<k){
    for(i in 1:nrow(r)){
      val <- r[i,1]
      point <- ref_list[ref_list$rp==val,][,2]
      if(length(point)>1){
        point <- sample(point,1)
      }
      if(point %in% points){next}
      else{
        points[i] <- point 
      }
    }
  }
  res <- dat[points,]
  return(res)
}
