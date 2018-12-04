#getting input ready

f <- 'path.../german.rds'
df <- readRDS(f)


df <- df %>% 
  mutate(GOOD = recode(df$BAD, 
                       "BAD" = "0", 
                       "GOOD" = "1"))
df <- df %>% select(-BAD)

#specify the name of the target column in the data
#target <- "GOOD"
#n <- 50 # number of individs per population

obj_list <- c(accuracy, get_precision) #list of objective functions
obj_names <- c("acc", "prec", "nf") #names of objective fns will be used as column names

#specify pareto criteria
pareto <- high(acc)*high(prec)*low(nf) # high = maximize

ans <- alg(df, "GOOD", obj_list, obj_names, pareto, n = 50, max_gen = 50)

ans



#if you want to visualize the result in terms of objectives

pop <- ans
epop <- evaluate_population(pop = pop,df = df, target = "GOOD", objectives = obj_list)
colnames(epop)<-c("acc", "prec", "nf")
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

