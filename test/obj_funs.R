

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

f_auc <- function(pred){
  auc <- mlr::performance(pred, auc)
  return(as.numeric(auc))
}

emp <- function(pred){
  m <- empCreditScoring(pred$data$prob.1, pred$data$truth)
  emp <- m$EMPC
  return(as.numeric(emp))
}



#d = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
#plotROCCurves(d)
#mlr::performance(pred$pred, auc)
#plotThreshVsPerf(d)
