library(IsolationForest)
outlier.isf<-function(x,cutoff=0.95){
set.seed(10086)  
if_trees <- IsolationTrees(x[,-1])
if_anom_score <- AnomalyScore(x[,-1], if_trees)
# adding anomaly score
df_ses_if<-x
df_ses_if$anom_score <- round(if_anom_score$outF, 4)
df_ses_if <- df_ses_if %>%
  dplyr::mutate(is_anomaly = ifelse(ecdf(anom_score)(anom_score) >= cutoff, TRUE, FALSE))

return(df_ses_if)
}
##############
normalization<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
##############
MCC <- function(sensitivity,specificity,accuracy){
  x <- sensitivity
  y <- specificity
  z <- accuracy
  m <- sqrt(((x-z)*(z-y)*(x+y-1)^2)/(((x+y-1)*z+x*(1-2*y))*((1-x-y)*z+y*(2*x-1)))) 
  return(m)
}
###
tf_1Dto2D <- function(data, ncol){
  # ls <- list()
  df <- data.frame(matrix(NA, nrow = length(data)/ncol, ncol = ncol))
  for(i in 1:(length(data)/ncol)){
    df[i,] <- data[(ncol*(i-1)+1):(ncol*i)]
  }
  return(df)
}
###
Kappa <- function(TN, TP, FN, FP){
  acc = (TN+TP)/(TN+TP+FN+FP)
  Pe = ((TN+FN)*(TN+FP)+(FN+TP)*(FP+TP))/(TN+TP+FN+FP)^2
  kappa = (acc-Pe)/(1-Pe)
  return(kappa)
}
###
S_to_P <- function(recall, precision, specificity, Sum){
  P = Sum*precision*(1-specificity)/(recall*(1-precision)+precision*(1-specificity))
  return(P)
}
###
confM_cutoff <- function(data, label, from, to, by, smooth=T){
  cut_i <-  seq(from=from, to=to, by=by)
  cut.df <- data.frame(cut_i)
  cut.df$acc <- 0
  cut.df$precision <- 0 
  cut.df$sens <- 0
  cut.df$spec <- 0
  cut.df$mcc <- 0
  cut.df$AUC <-0
  for(i in cut_i){
    cut_FEP <- data
    if (colnames(cut_FEP)[1]!="N") {colnames(cut_FEP) <- c("P","N")}
    cut_FEP$dig <- "N"
    cut_FEP[cut_FEP$P>i,]$dig <- "P"
    cut_FEP$dig_real <- label
    tmp <- confusionMatrix(data = as.factor(cut_FEP$dig),reference=as.factor(cut_FEP$dig_real),positive = 'P')
    cut.df[cut.df$cut_i==i,]$acc <- tmp$overall[["Accuracy"]]
    cut.df[cut.df$cut_i==i,]$precision <- tmp$byClass[["Precision"]]
    cut.df[cut.df$cut_i==i,]$sens <- tmp$byClass[["Sensitivity"]]
    cut.df[cut.df$cut_i==i,]$spec <- tmp$byClass[["Specificity"]]
  }
  cut.df$mcc <- MCC(cut.df$sens, cut.df$spec, cut.df$acc)
  cut.df$AUC <- auc(predictor=cut_FEP$N, response=cut_FEP$dig_real,smooth=smooth)
  cut.df$index_merge <- rowMeans(cut.df[,2:6])
  return(cut.df)
}
### ROC model required
caret_cv2tab <- function(model){
  model_cv <- model[["resampledCM"]]
  ### handle svm
  ind_keep = which(model_cv$cell1!=0&model_cv$cell2!=0)
  model_cv <- model_cv[ind_keep,]
  colnames(model_cv)[1:4] <- c("TN","FP","FN","TP")
  model_cv$sum <- rowSums(model_cv[,1:4])
  model_cv$acc <- (model_cv$TN+model_cv$TP)/model_cv$sum
  model_cv$precision <- model_cv$TP/(model_cv$TP+model_cv$FP)
  model_cv$MCC <- mltools::mcc(TP=model_cv$TP,TN=model_cv$TN,FP=model_cv$FP,FN=model_cv$FN)

  cv_tab <- data.frame(Acc=mean(model_cv$acc),AccSD=sd(model_cv$acc),Prec=mean(model_cv$precision),PrecSD=sd(model_cv$precision),
                       model[["results"]],MCC=mean(model_cv$MCC),MCCSD=sd(model_cv$MCC))
  cv_tab <- cv_tab[,c("Acc","AccSD","Prec","PrecSD","Sens","SensSD","Spec","SpecSD","MCC","MCCSD","ROC","ROCSD")]
  colnames(cv_tab)[which(colnames(cv_tab)=="ROC")] <- "AUC"
  colnames(cv_tab)[which(colnames(cv_tab)=="ROCSD")] <- "AUCSD"
  cv_tab <- data.frame(t(cv_tab))
  colnames(cv_tab) <- model[["method"]]
  return(cv_tab)
}

caret_cv2tab_95ci <- function(model, R = 1000) {  # R = bootstrap 
  model_cv <- model[["resampledCM"]]
  
  ind_keep <- which(model_cv$cell1 != 0 & model_cv$cell2 != 0)
  model_cv <- model_cv[ind_keep, ]
  
  colnames(model_cv)[1:4] <- c("TN", "FP", "FN", "TP")
  
  model_cv$sum <- rowSums(model_cv[, 1:4])
  model_cv$acc <- (model_cv$TN + model_cv$TP) / model_cv$sum
  model_cv$precision <- model_cv$TP / (model_cv$TP + model_cv$FP)
  model_cv$MCC <- mltools::mcc(TP = model_cv$TP, TN = model_cv$TN, FP = model_cv$FP, FN = model_cv$FN)
  model_cv$Sensitivity <- model_cv$TP / (model_cv$TP + model_cv$FN)
  model_cv$Specificity <- model_cv$TN / (model_cv$TN + model_cv$FP)
  model_cv$ROC <- model[["resample"]]$ROC[ind_keep]
  
  boot_ci <- function(x, R = 1000) {
    x <- na.omit(x) 
    if (length(x) == 0) return(c(NA, NA))
    
    boot_res <- boot(x, function(data, idx) mean(data[idx]), R = R)
    ci <- tryCatch(
      boot.ci(boot_res, type = "bca")$bca[4:5],  
      error = function(e) quantile(boot_res$t, c(0.025, 0.975), na.rm = TRUE)  
    )
    return(ci)
  }
  
  acc_ci <- boot_ci(model_cv$acc, R = R)
  prec_ci <- boot_ci(model_cv$precision, R = R)
  sens_ci <- boot_ci(model_cv$Sensitivity, R = R)
  spec_ci <- boot_ci(model_cv$Specificity, R = R)
  mcc_ci <- boot_ci(model_cv$MCC, R = R)
  roc_ci <- boot_ci(model_cv$ROC, R = R)
  
  cv_tab <- data.frame(
    Acc = sprintf("%.3f", mean(model_cv$acc, na.rm = TRUE)),
    AccLow = sprintf("%.3f", acc_ci[1]),
    AccHigh = sprintf("%.3f", acc_ci[2]),
    Prec = sprintf("%.3f", mean(model_cv$precision, na.rm = TRUE)),
    PrecLow = sprintf("%.3f", prec_ci[1]),
    PrecHigh = sprintf("%.3f", prec_ci[2]),
    Sens = sprintf("%.3f", mean(model_cv$Sensitivity, na.rm = TRUE)),
    SensLow = sprintf("%.3f", sens_ci[1]),
    SensHigh = sprintf("%.3f", sens_ci[2]),
    Spec = sprintf("%.3f", mean(model_cv$Specificity, na.rm = TRUE)),
    SpecLow = sprintf("%.3f", spec_ci[1]),
    SpecHigh = sprintf("%.3f", spec_ci[2]),
    MCC = sprintf("%.3f", mean(model_cv$MCC, na.rm = TRUE)),
    MCCLow = sprintf("%.3f", mcc_ci[1]),
    MCCHigh = sprintf("%.3f", mcc_ci[2]),
    AUC = sprintf("%.3f", mean(model_cv$ROC, na.rm = TRUE)),
    AUCLow = sprintf("%.3f", roc_ci[1]),
    AUCHigh = sprintf("%.3f", roc_ci[2])
  )
  
  cv_tab[is.na(cv_tab)] <- "NA"
  
  cv_tab <- data.frame(t(cv_tab))
  colnames(cv_tab) <- model[["method"]]
  
  return(cv_tab)
}

h2o_cv2tab <- function(model_h2o){
  model <- model_h2o@model[["cross_validation_metrics_summary"]]
  model <- as.data.frame(t(model))
  model <- model[,c("accuracy","auc","mcc","recall","specificity","precision")]
  model <- data.frame(apply(model,2,as.numeric))
  colnames(model) <- c("Acc","AUC","MCC","Sens","Spec","Prec")
  tmp1 <- data.frame(model[1,])
  tmp2 <- data.frame(model[2,])
  colnames(tmp2) <- paste(colnames(tmp2),"SD",sep="")
  cv_tab <- cbind(tmp1,tmp2)
  cv_tab <- cv_tab[,c("Acc","AccSD","Prec","PrecSD","Sens","SensSD","Spec","SpecSD","MCC","MCCSD","AUC","AUCSD")]
  cv_tab <- data.frame(t(cv_tab))
  colnames(cv_tab) <- "DNN"
  return(cv_tab)
}
### 
C_pred2tab <- function(model, data){
  pred <- predict(model,newdata=data)
  label <- as.factor(data$dig)
  confM <- confusionMatrix(data = pred,reference=label,positive = 'P')
  mcc <- mcc(preds = pred, actuals = label)
  pred_poss <- predict(model,newdata=data,type="prob")
  auc <- auc(predictor=pred_poss$N, response=label,smooth=F)
  pred_tab <- data.frame(confM$overall[["Accuracy"]],confM$byClass[["Precision"]],confM[["byClass"]][["Sensitivity"]],
                         confM[["byClass"]][["Specificity"]],mcc,auc)
  colnames(pred_tab) <- c("Acc","Prec","Sens","Spec","MCC","AUC")
  pred_tab <- as.data.frame(t(pred_tab))
  colnames(pred_tab) <- model$method
  return(pred_tab)
}
### require h2o environment to be initiated already(always) 
H_pred2tab <- function(model, data){
  data_h2o <- as.h2o(data,seed=1)
  pred <- h2o.predict(model, data_h2o,seed=1)
  pred <- as.data.frame(pred)  
  pred1 <- pred[,1]
  label <- as.factor(data$dig)
  confM<- confusionMatrix(data = pred1,reference=label,positive = 'P')
  mcc <- MCC(confM$byClass[["Sensitivity"]], confM$byClass[["Specificity"]],confM$overall[["Accuracy"]])
  auc <- auc(predictor=pred$N, response=label,smooth=F)
  pred_tab <- data.frame(confM$overall[["Accuracy"]],confM$byClass[["Precision"]],confM[["byClass"]][["Sensitivity"]],
                         confM[["byClass"]][["Specificity"]],mcc,auc)
  colnames(pred_tab) <- c("Acc","Prec","Sens","Spec","MCC","AUC")
  pred_tab <- as.data.frame(t(pred_tab))
  colnames(pred_tab) <- "DNN"
  return(pred_tab)
}
###
P_pred2tab <- function(prob_p, data){
  pred <- ifelse(prob_p$V1>0.5,"P","N") %>% factor(levels = c("N","P"))
  label <- factor(data$dig,levels = c("N","P"))
  confM <- confusionMatrix(data = pred,reference=label,positive = 'P')
  mcc <- mcc(preds = pred, actuals = label)
  auc <- auc(predictor=prob_p$V1, response=label,smooth=T)
  pred_tab <- data.frame(confM$overall[["Accuracy"]],confM$byClass[["Precision"]],confM[["byClass"]][["Sensitivity"]],
                         confM[["byClass"]][["Specificity"]],mcc,auc)
  colnames(pred_tab) <- c("Acc","Prec","Sens","Spec","MCC","AUC")
  pred_tab <- as.data.frame(t(pred_tab))
  colnames(pred_tab) <- "Densenet"
  return(pred_tab)
}
