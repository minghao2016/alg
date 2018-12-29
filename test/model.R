

#Gradient boost
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    early_stopping_rounds = 10,
    nrounds = 1000,
    max_depth = 6,
    lambda = 0.2878754,
    alpha = 0.08954572,
    eta = 0.0262058,
    subsample = 0.8923049,
    min_child_weight = 0.8923049,
    colsample_bytree = 0.9694954
  )
)

#Cross Validation
resampling <- makeResampleDesc("CV", iters = 5)
