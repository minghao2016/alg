

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
  m <- EMP::empCreditScoring(pred$data$prob.1, pred$data$truth)
  emp <- m$EMPC
  return(as.numeric(1-emp))
}

mshare <- function(pred){
  m <- EMP::empCreditScoring(pred$data$prob.1, pred$data$truth)
  emp <- m$EMPC
  cutoff <- m$EMPCfrac
  
  share <- 1 - cutoff
  #print(paste("EMPCS: ", emp, "cutoff: ", cutoff, "share: ", share))
  
  inv_share <- 1-share
  return(as.numeric(inv_share))
}

######################################################################

# combined unscaled objective #

comb <- function(task, model, pred, feats, extra.args){
  m <- EMP::empCreditScoring(pred$data$prob.1, pred$data$truth)
  emp <- as.numeric(1-m$EMPC)
  cutoff <- as.numeric(m$EMPCfrac)
  nfeat <- as.numeric(pred$task.desc$n.feat[1])
  
  nfeat <- as.numeric((nfeat / 35))
  
  obj <- sum(emp,cutoff,nfeat)
  return(obj)
}

COMB <- makeMeasure(id = "COMB", minimize = TRUE, properties = "classif", 
                     fun = comb)

#######################################################################
empcs <- function(task, model, pred, feats, extra.args){
  m <- EMP::empCreditScoring(pred$data$prob.1, pred$data$truth)
  emp <- m$EMPC
  return(as.numeric(1-emp))
}

EMPcs <- makeMeasure(id = "emp", minimize = TRUE, properties = "classif", 
                     fun = empcs)


#d = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
#plotROCCurves(d)
#mlr::performance(pred$pred, auc)
#plotThreshVsPerf(d)
