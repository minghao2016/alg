rm(list = ls())

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

#getting input ready


f <- 'german.rds'
df <- readRDS(file.path(data.folder, f))


df <- df %>% 
  mutate(GOOD = recode(df$BAD, 
                       "BAD" = "0", 
                       "GOOD" = "1"))
df <- df %>% select(-BAD)


costs <- runif(n = 20, min = 1, max = 20)
names(costs) <- colnames(df[1:(ncol(df)-1)])
#specify the name of the target column in the data
#target <- "GOOD"
#n <- 50 # number of individs per population

obj_list <- c(f_auc, emp) #get_spec) #list of objective functions
obj_names <- c("auc", "emp", "nf", "fcost") #, "nf") #names of objective fns will be used as column names

#specify pareto criteria
pareto <- low(auc)*low(emp)*low(nf)*low(fcost) # high = maximize

start_time <- Sys.time()

ans <- alg(df, "GOOD", obj_list, obj_names, pareto, 
           n = 5, max_gen = 5, 
           model = xgb_learner,
           resampling = resampling,
           num_features = TRUE,
           feature_cost = costs,
           mutation_rate = 0.01)

end_time <- Sys.time()
end_time - start_time
ans

#if you want to visualize the result in terms of objectives

pop <- ans$fin_pop
epop <- evaluate_population(pop = pop,df = df, target = "GOOD", model = xgb_learner,
                            objectives = obj_list, num_features = TRUE)
colnames(epop)<-c("auc", "emp", "nf")
spop <- non_dom_sort(epop, pareto)
plt <- plot_ly(spop, x=~auc, y=~emp, z=~nf, 
               color= ~.level, type="scatter3d", mode = 'markers')
plt

#view selected column names

cols <- lapply(ans,as.logical)
res <- data.frame()
for(i in 1:length(cols)){
  print(i)
  for(j in 1:20){
    print(j)
    res[i,j] <- ans[[i]][j]
    #print(colnames(df[,cols[[i]]]))
  }
}
report <- data.frame()
for(i in 1:20){
  report[1,i] <- sum(res[,i])/nrow(res)
}

colnames(report) <- colnames(df)[-length(df)]
report
