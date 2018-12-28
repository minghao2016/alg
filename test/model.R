

#Gradient boost
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    early_stopping_rounds = 10
  )
)

#Cross Validation
resampling <- makeResampleDesc("CV", iters = 5)
