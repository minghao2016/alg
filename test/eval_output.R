#?makeFeatSelControlSequentiaL
#makeFeatSelControlSequential

target <- "BAD"
f <- 'german.rds'

df <- readRDS(file.path(data.folder, f))

levels(df$BAD)[levels(df$BAD) == "GOOD"] <- "0"
levels(df$BAD)[levels(df$BAD) == "BAD"] <- "1"

#ind <- ans[[1]]$ind[[4]]

result <- list()

n <- 1

for(ind in ans[[1]]$ind){
  
  df <- readRDS(file.path(data.folder, f))
  
  levels(df$BAD)[levels(df$BAD) == "GOOD"] <- "0"
  levels(df$BAD)[levels(df$BAD) == "BAD"] <- "1"
  
  if(sum(ind)==0){next} else {
  
  #select columns of an individual
  goods <- data.frame(df[,c(target)])
  colnames(goods) <- target
  
  cnames <- colnames(df)
  cnames <- cnames[-which(cnames==target)]
  
  selected_columns <- cnames[as.logical(ind)]
  
  df <- df[,selected_columns]
  df <- cbind(df,goods)
  
  print(paste(f, "rows: ", nrow(df), "features: ", ncol(df)))

  ## 75% of the sample size
  smp_size <- floor(0.75 * nrow(df))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  test <- df[-train_ind, ]
  df <- df[train_ind, ]
  
  seed <- 777
  options(mlr.debug.seed = seed)
  set.seed(seed)
  
  
  #create dummies
  df <- createDummyFeatures(df, target = target, method = 'reference') 
  test <- createDummyFeatures(test, target = target, method = 'reference') 
  
  
  print("training model")
  
  params <- train_model(df)
  
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
  
  print("Performing classification")
  
  #Cross Validation
  trainTask <- makeClassifTask(data = df, target = target, positive=1)
  
  mod <- train(learner = xgb_learner, task = trainTask)
  
  pred <- predict(mod, newdata = test)
  
  print("Evaluating result")
  
  res <- data.frame()
  res[1,1] <- mshare(pred)
  res[1,2] <- emp(pred)
  res[1,3] <- sum(ind)
  
  colnames(res) <- c("mshare", "emp",  "nf")
  
  eind <- list(ind, res)
  
  result[[n]] <- eind
  
  n <- n+1
  print(n)
  
  }
}
  
  