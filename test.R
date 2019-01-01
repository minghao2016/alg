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

obj_list <- c(mshare, emp) #get_spec) #list of objective functions
obj_names <- c("mshare", "emp", "nf")#names of objective fns will be used as column names

#specify pareto criteria
pareto <- low(mshare)*low(emp)*low(nf)#*low(fcost) # high = maximize

start_time <- Sys.time()

ans1 <- alg(df, "GOOD", obj_list, obj_names, pareto, 
           n = 5, max_gen = 1, 
           model = xgb_learner,
           resampling = resampling,
           num_features = TRUE,
           #feature_cost = costs,
           mutation_rate = 0.01)

end_time <- Sys.time()
end_time - start_time
ans1

#if you want to visualize the result in terms of objectives

colnames(epop)<-c("auc", "emp", "nf")
spop <- non_dom_sort(epop, pareto)
plt <- plot_ly(spop, x=~auc, y=~emp, z=~nf, 
               color= ~.level, type="scatter3d", mode = 'markers')
plt

