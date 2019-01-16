

target <- "BAD"
model <- xgb_learner

f <- 'german.rds'
f <- "australian.rds"

df <- readRDS(file.path(data.folder, f))

levels(df$BAD)[levels(df$BAD) == "GOOD"] <- "0"
levels(df$BAD)[levels(df$BAD) == "BAD"] <- "1"

task <- makeClassifTask(data = df, target = "BAD", positive=1)

methods = c("information.gain", "chi.squared") 
fv <- generateFilterValuesData(task, method = methods)


fv$data[fv$data$information.gain>mean(fv$data$information.gain),1]
fv$data[fv$data$chi.squared>mean(fv$data$chi.squared),1]

flt <- fv$data[fv$data$information.gain>mean(fv$data$information.gain),1]

head(df[,flt])


goods <- data.frame(df[,c(target)])
colnames(goods) <- target

cnames <- colnames(df)
cnames <- cnames[-which(cnames==target)]

df <- df[,flt]
df <- cbind(df,goods)

resampling <- makeResampleDesc("CV", iters = 5)


params <- train_model(df, target= "BAD")


xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    early_stopping_rounds = 100,
    nrounds = 10000,
    max_depth = params$max_depth,
    lambda = params$lambda,
    alpha = params$alpha,
    eta = params$eta,
    subsample = params$subsample,
    min_child_weight = params$min_child_weight,
    colsample_bytree = params$colsample_bytree
  )
)

objectives <- obj_list

df <- createDummyFeatures(df, target = target, method = 'reference') 

  
  res <- perform_classification(df, target, model = xgb_learner, 
                                resampling = resampling)
  
  get_objective_values <- function(a) {
    # call each function to a
    lapply(objectives, function(f) f(a))
  }
  
  ans <- get_objective_values(res)
  
  obj_vals <- data.frame()
  
  for(i in 1:length(ans)){
    obj_vals[1,i] <- ans[[i]]
  }
  
    n <- length(obj_vals)+1
    obj_vals[1,n]<- length(flt)
  
  
obj_vals














#########
df <- createDummyFeatures(df, target = target, method = 'reference') 


listFilterMethods()


task <- makeClassifTask(data = df, target = "BAD", positive=1)

#fv <- generateFilterValuesData(task, method = "information.gain")

methods = c("information.gain", "chi.squared", "gain.ratio", 
            "anova.test", "auc",  "kruskal.test", "relief")
fv <- generateFilterValuesData(task, method = methods)

filtered.task = filterFeatures(task, method = "information.gain", perc= 0.25)

fv$data[fv$data$information.gain>mean(fv$data$information.gain),1]
fv$data[fv$data$chi.squared>mean(fv$data$chi.squared),1]
fv$data[fv$data$gain.ratio>mean(fv$data$gain.ratio),1]
fv$data[fv$data$anova.test>mean(fv$data$anova.test),1]
fv$data[fv$data$auc>mean(fv$data$auc),1]
fv$data[fv$data$kruskal.test>mean(fv$data$kruskal.test),1]
fv$data[fv$data$relief>mean(fv$data$relief),1]





