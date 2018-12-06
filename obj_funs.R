accuracy <- function(pred_result){
  ans <- sum(pred_result$data$truth == pred_result$data$response) / nrow(pred_result$data)
  return(ans)
}


get_precision <- function(res){
  cm <- confusionMatrix(res$data$truth, res$data$response)
  precision <- as.numeric(cm$byClass["Precision"])
  return(precision)
}

get_spec <- function(res){
  cm <- confusionMatrix(res$data$truth, res$data$response)
  spec <- as.numeric(cm$byClass["Specificity"])
  return(spec)
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
