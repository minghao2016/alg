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
      early_stopping_rounds = 10,
      nrounds = 10,
      max_depth = params$max_depth,
      lambda = params$lambda,
      alpha = params$alpha,
      eta = params$eta,
      subsample = params$subsample,
      min_child_weight = params$min_child_weight,
      colsample_bytree = params$colsample_bytree
    )
  )
  
  #Cross Validation
  resampling <- makeResampleDesc("CV", iters = 5)
  
  experiment(df, xgb_learner)
  
}




