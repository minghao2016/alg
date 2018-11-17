City <- setClass(
        #name of the class
        "City",
        
        #defining slots
        #in this case its coordinates
        
        slots = c(
                  x = "numeric",
                  y = "numeric"
                  ),
        #setting default values for the slots (optional)
        prototype = list(
                  x=0,
                  y=0
                  )
        )

#calculating distance from the city to another city
setGeneric(name="getDistance",
           def=function(theObject, city)
           {
             standardGeneric("getDistance")
           }
           )
setMethod(f="getDistance",
          signature = "City",
          definition = function(theObject, city)
          {
            xDis <- abs(theObject@x-city@x)
            yDis <- abs(theObject@y-city@y)
            distance <- sqrt(xDis^2+yDis^2)
            return(distance)
          }
          )


Fitness <- setClass("Fitness",
                    slots=c(
                      route="list",
                      distance="numeric",
                      fitness="numeric"
                    )
                    )
#calculating route distance

#REDO AS A FUNCTION!!!!!!!!!!!!!!!!!!!!

setGeneric(name="routeDistance",
           def=function(theObject)
           {standardGeneric("routeDistance")}
)
setMethod(f="routeDistance",
          signature = "Fitness",
          definition = function(theObject)
          {
            if(theObject@distance==0)
            {
              pathDistance <- 0
              for(i in 1:length(theObject@route))
              {
                fromCity <- theObject@route[i][[1]]
                toCity <- NaN
                if(i+1 < length(theObject@route))
                {
                  toCity <- theObject@route[i+1][[1]]
                } else {toCity <- theObject@route[1][[1]]}
                pathDistance <- pathDistance + getDistance(fromCity, toCity)
              }
              theObject@distance <- pathDistance
            }
            return(theObject@distance) 
          }
)
