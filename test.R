# clearing the memory
rm(list = ls())

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(cd))

# setting inner folders
code.folder <- "R/code"
data.folder <- "R/data"
file.folder <- "R/files"
resu.folder <- "R/output"

# loading functions
source(file.path(code.folder, "nsgaiii.R"))
source(file.path(code.folder, "obj_funs.R"))



#getting input ready


f <- 'german.rds'
df <- readRDS(file.path(data.folder, f))


df <- df %>% 
  mutate(GOOD = recode(df$BAD, 
                       "BAD" = "0", 
                       "GOOD" = "1"))
df <- df %>% select(-BAD)


#specify the name of the target column in the data
#target <- "GOOD"
#n <- 50 # number of individs per population

obj_list <- c(accuracy, get_precision) #get_spec) #list of objective functions
obj_names <- c("acc", "prec" )#, "spec", "nf") #names of objective fns will be used as column names

#specify pareto criteria
pareto <- high(acc)*high(prec)#*high(spec)*low(nf) # high = maximize

ans <- alg(df, "GOOD", obj_list, obj_names, pareto, 
          n = 10, max_gen = 10, 
          model = xgb_learner,
          num_features = FALSE)

ans



#if you want to visualize the result in terms of objectives

pop <- ans
epop <- evaluate_population(pop = pop,df = df, target = "GOOD", 
                            objectives = obj_list, num_features = TRUE)
colnames(epop)<-c("acc", "prec", "spec", "nf")
spop <- non_dom_sort(epop, pareto)
plt <- plot_ly(spop, x=~acc, y=~prec, z=~nf, 
               color= ~.level, type="scatter3d", mode = 'markers')
plt

#view selected column names

cols <- lapply(ans,as.logical)
for(i in 1:length(cols)){
  print(i)
  print(colnames(df[,cols[[i]]]))
}

