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
  ref <- matrix(ncol=2)
  colnames(ref) <- c("rp", "data")
    for(point in 1:nrow(data)){
      p_ref <- find_ref_point(data[point,], rp)
      ref <- rbind(ref,c(p_ref,point))
    }
  ref <- ref[-1,]
  return(ref)
}

#need to write the niching part
points <- a[sample(nrow(a),5),][,2]
s <- npf[points,]
