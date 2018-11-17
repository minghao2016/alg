library(ggplot2)

city <- c(index,x,y)

city_distance <- function(x1, y1, x2, y2){
  xDis <- abs(x1-x2)
  yDis <- abs(y1-y2)
  distance <- sqrt(xDis^2+yDis^2)
  return(distance)
}

route_distance <- function(route){
  pathDistance <- 0
  for (i in 1:len(route)){
    fromCity <- route[i]
    toCity <- NaN 
    if(i+1 < len(route)){
      toCity <- route[i+1]
    } else {
      toCity <- route[0]
    }
    pathDistance <- pathDistance + 
      city_distance(fromCity[2],fromCity[3], toCity[2],toCity[3])
  } 
  return(pathDistance)
}

route_Fitness <- function(distance){
  fitness <- 1/distance
  return(distance)
}
