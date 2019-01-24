rm(list = ls())

library(digest)

library(dplyr)

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(cd))

# setting inner folders
code.folder <- "alg/source"
test.folder <- "alg/test"
data.folder <- "alg/data"

#file.folder <- "R/files"
#resu.folder <- "R/output"

# loading functions
source(file.path(code.folder, "alg_builder.R"))
source(file.path(test.folder, "obj_funs.R"))
source(file.path(test.folder, "model.R"))
output.folder <- "alg/output/"


#getting input ready


f <- 'german.rds'

experiment <- function(f, xgb_learner){
  
  df <- readRDS(file.path(data.folder, f))
  
  levels(df$BAD)[levels(df$BAD) == "GOOD"] <- "0"
  levels(df$BAD)[levels(df$BAD) == "BAD"] <- "1"
  
  
  
  #costs <- runif(n = 20, min = 1, max = 20)
  #names(costs) <- colnames(df[1:(ncol(df)-1)])
  #specify the name of the target column in the data
  #target <- "GOOD"
  #n <- 50 # number of individs per population
  
  obj_list <- c(mshare, emp) #get_spec) #list of objective functions
  obj_names <- c("mshare", "emp", "nf")#names of objective fns will be used as column names
  
  #specify pareto criteria
  pareto <- low(mshare)*low(emp)*low(nf)#*low(fcost) # high = maximize
  
  parallelStartSocket(2, show.info = FALSE)
  
  
  start_time <- Sys.time()
  ans <- alg(df, "BAD", obj_list, obj_names, pareto, 
             n = 100, max_gen = 100, 
             model = xgb_learner,
             resampling = resampling,
             num_features = TRUE,
             #feature_cost = costs,
             mutation_rate = 0.01)
  
  end_time <- Sys.time()
  end_time - start_time
  
  parallelStop()
 
  output_path = paste0(output.folder, f)
  
  output_path.file = paste0(output_path,"1.RData")
  
  write.csv(ans$pf_raw$objective_values)
  save(ans, file = output_path.file)
   
  return(ans)
}

a<-experiment(f, xgb_learner)

#if you want to visualize the result in terms of objectives

#colnames(epop)<-c("auc", "emp", "nf")
#spop <- non_dom_sort(epop, pareto)

#plt


#b <- ans[[2]]
#test_df <- b[[1]]

#for(i in b){
#  test_df <- rbind(test_df, i)
#}

#tdf <- unique(test_df)

#stdf <- non_dom_sort(tdf, pareto)

#ab <- stdf[stdf$.level==1,]

#ab <- ab %>% select(-.level)

#a <- ans[[1]]$pf_raw$objective_values
#abc <- rbind(a,ab)

#tyu <- non_dom_sort(abc,pareto)

#plt <- plot_ly(ab, x=~mshare, y=~emp, z=~nf, 
               #color= ~.level, 
#               type="scatter3d", mode = 'markers')
