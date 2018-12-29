

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
  return(as.numeric(1-auc))
}

emp <- function(pred){
  m <- empCreditScoring(pred$data$prob.1, pred$data$truth)
  emp <- m$EMPC
  return(as.numeric(1-emp))
}

mshare <- function(pred){
  prediction <- pred$data$response
  share <- sum(as.integer(as.character(prediction)))/length(prediction)
  #print(share)
  inv_share <- 1-share
  return(as.numeric(inv_share))
}



#d = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
#plotROCCurves(d)
#mlr::performance(pred$pred, auc)
#plotThreshVsPerf(d)
