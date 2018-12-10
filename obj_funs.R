get_acc <- function(res){
  cm <- calculateROCMeasures(res)
  acc <- as.numeric(cm$measures$acc)
  return(acc)
}


get_tpr <- function(res){
  cm <- calculateROCMeasures(res)
  tpr <- as.numeric(cm$measures$tpr)
  return(tpr)
}

get_tnr <- function(res){
  cm <- calculateROCMeasures(res)
  tnr <- as.numeric(cm$measures$tnr)
  return(tnr)
}

xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    early_stopping_rounds = 10
  )
)

resampling <- makeResampleDesc("CV", iters = 5)
