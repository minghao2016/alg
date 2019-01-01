i <- sample(0:1, size = 20, replace = TRUE)

d <- select_columns(df,"GOOD",i)

a <- perform_classification(d, "GOOD", xgb_learner, resampling)

sum(as.integer(as.character(pred$data$response)))

mshare(a)

ans$fin_pop_fitness[which()]

normalizeFeatures(ans$fin_pop_fitness, cols = "nf", method = "range", range= c(0, 1))