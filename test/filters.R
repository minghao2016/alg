rm(list = ls())


cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(cd))

# setting inner folders
code.folder <- "alg/source"
test.folder <- "alg/test"
data.folder <- "alg/data"

#file.folder <- "R/files"
#resu.folder <- "R/output"

# loading functions
source(file.path(code.folder, "alg_builder.R"))
source(file.path(test.folder, "obj_funs.R"))
source(file.path(test.folder, "model.R"))
output.folder <- "alg/output/"


target <- "BAD"
model <- xgb_learner

f <- 'german.rds'

df <- readRDS(file.path(data.folder, f))

levels(df$BAD)[levels(df$BAD) == "GOOD"] <- "0"
levels(df$BAD)[levels(df$BAD) == "BAD"] <- "1"

listFilterMethods()

task <- makeClassifTask(data = df, target = "BAD", positive=1)

fv <- generateFilterValuesData(task, method = "information.gain")

methods = c("information.gain", "chi.squared", "gain.ratio")
fv <- generateFilterValuesData(task, method = methods)

filtered.task = filterFeatures(task, method = "information.gain", perc= 0.25)

fv$data[fv$data$information.gain>mean(fv$data$information.gain),1]
fv$data[fv$data$chi.squared>mean(fv$data$chi.squared),1]
fv$data[fv$data$gain.ratio>mean(fv$data$gain.ratio),1]



