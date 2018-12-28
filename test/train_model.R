library(digest)
set.seed(1991)

source(file.path(code.folder, "model.R"))

f <- 'german.rds'
df <- readRDS(file.path(data.folder, f))


target <- 'GOOD'


df <- df %>% 
  mutate(GOOD = recode(df$BAD, 
                       "BAD" = "0", 
                       "GOOD" = "1"))
df <- df %>% select(-BAD)

df <- df %>% createDummyFeatures(target = target, method = 'reference')


ndf <- normalizeFeatures(df, target = target)

# Define machine learning task
ml_task <- makeClassifTask(data = ndf, target = target, positive=1)

# Create repeated cross validation folds
cv_folds <- makeResampleDesc("CV", iters = 5)

# Define model
model <- makeLearner( "classif.xgboost", predict.type = "prob")

#
random_tune <- makeTuneControlRandom(maxit = 1000L)

# Define parameters of model and search grid ~ !!!! MODEL SPECIFIC !!!!
model_Params <- makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=20),
  makeIntegerParam("max_depth",lower=1,upper=5),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

parallelStartSocket(2)

# Tune model to find best performing parameter settings using random search algorithm
tuned_model <- tuneParams(learner = model,
                          task = ml_task,
                          resampling = cv_folds,
                          measures = auc,       # R-Squared performance measure, this can be changed to one or many
                          par.set = model_Params,
                          control = random_tune,
                          show.info = FALSE)
parallelStop()


tuned_model

params <- tuned_model$x

