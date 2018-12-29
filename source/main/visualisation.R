
view_pareto <- function(pareto_fronts, reference_points){
  
  pf <- pareto_fronts[which(pareto_fronts$.level==1),]
  rpoints <- data.frame(reference_points)
  
  for(i in 1:(ncol(pf)-1)){
    
    pf[,i] <- (pf[,i] - min(pf[,i]))/(max(pf[,i]) - min(pf[,i]))
  }
  p1 <- plot_ly(pf, 
                x=pf[,1], y=pf[,2], z=pf[,3],
                type="scatter3d", mode = 'markers')
  
  p2 <- plot_ly(rpoints, x = rpoints[,1], y = rpoints[,2], z = rpoints[,3], 
                type="scatter3d", mode = 'markers')
  
  plt <- subplot(p1,p2)
  
  return(plt)
}
