
################################################################

rm(list = ls())

library(digest)
library(dplyr)

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(cd))

# setting inner folders
code.folder <- "alg/source"
test.folder <- "alg/test"
data.folder <- "alg/data"
output.folder <- "alg/output/"

#file.folder <- "R/files"
#resu.folder <- "R/output"

# loading functions
source(file.path(code.folder, "alg_builder.R"))
source(file.path(test.folder, "obj_funs.R"))
#source(file.path(test.folder, "model.R"))
source(file.path(test.folder, "train_model.R"))
source(file.path(test.folder, "experiment.R"))

datasets <- list.files(path = data.folder)

for(f in datasets){
  
  target <- "BAD"
  
  df <- readRDS(file.path(data.folder, f))
  
  levels(df$BAD)[levels(df$BAD) == "GOOD"] <- "0"
  levels(df$BAD)[levels(df$BAD) == "BAD"] <- "1"
  
  print(paste(f, "rows: ", nrow(df), "features: ", ncol(df)))
  
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
  
  smp_size <- floor(0.75 * nrow(df))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  test <- df[-train_ind, ]
  df <- df[train_ind, ]
  df <- createDummyFeatures(df, target = target, method = 'reference')
  
  
  
  func <- function(task, model, pred, feats, extra.args){
    m <- EMP::empCreditScoring(pred$data$prob.1, pred$data$truth)
    emp <- m$EMPC
    return(as.numeric(1-emp))
  }
  
  EMP <- makeMeasure(id = "emp", minimize = TRUE, properties = "classif", 
                     fun = func)
  
  trainTask <- makeClassifTask(data = df, target = target, positive=1)
  #Cross Validation
  rdesc <- makeResampleDesc("CV", iters = 5)
  
  ctrl <- makeFeatSelControlSequential(method = "sffs", 
                                       alpha = 0.0001, beta = -0.00001, 
                                       log.fun = "default",
                                       maxit = 100000) 
  
  parallelStartSocket(2, show.info = FALSE)
  
  sfeats = selectFeatures(learner = xgb_learner, task = trainTask, resampling = rdesc,
                          control = ctrl, show.info = TRUE, measures = COMB)
  print(sfeats)
  
  parallelStop()
  
  
  output_path = paste0(output.folder, f)
  output_path.file = paste0(output_path,"sffs.RData")
  save(sfeats, file = output_path.file)
}


  