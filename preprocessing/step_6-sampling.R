library(caret)

setwd("~/psychosis/v1_20241016/")

### Unify Gender & reorder columns ################################################
data[data$Gender=="男","Gender"] <- "M"
data[data$Gender=="女","Gender"] <- "F"

Valida_out_tot[Valida_out_tot$Gender=="男","Gender"] <- "M"
Valida_out_tot[Valida_out_tot$Gender=="女","Gender"] <- "F"

colnms <- c("A/G","ALB","ALP","ALT","apoA","apoB","AST","AST/ALT", "Ca","CHO","CK","CKMB","Cl","CO2",  
            "CREA","DBIL","GGT","GLO","GLU","HDL-C","IBIL","K","LDH","LDL-C","Mg","Na","P","TBIL","TG",   
            "TP","UA","Urea","BASO%","EO%","HCT","HGB","LYM%","MCH","MCHC","MCV","MON%","MPV","NEU%","PCT",   
            "PDW","PLT","RBC","RDW-CV","WBC","ID","Gender","Age","dig","Chap")

data <- data[,colnms]
FEP_data <- FEP_data[,colnms]
Valida_out_tot <- Valida_out_tot[,colnms]
Valida_na_tot <- Valida_na_tot[,colnms]

### data distribution ###########################################
sum_data <- as.data.frame(matrix(rep(0,49*4),ncol = 4))
colnames(sum_data) <- c('mean','sd','mean-2*sd','mean+2*sd')
rownames(sum_data) <- colnames(data)[1:49]

for(i in 1:49){
  sum_data[i,1] <- mean(data[,i])
  sum_data[i,2] <- sd(data[,i])
}
sum_data$`mean-2*sd` <- sum_data$mean-2*sum_data$sd
sum_data$`mean+2*sd` <- sum_data$mean+2*sum_data$sd

## clipping
for (i in 1:nrow(data)){
  for (j in 1:49){
    if(data[i,j] < sum_data[j,3]){data[i,j] <- sum_data[j,3]}
    if(data[i,j] > sum_data[j,4]){data[i,j] <- sum_data[j,4]}
  }
}

data_m1<- data
sum_data_1 <- as.data.frame(matrix(rep(0,49*4),ncol = 4))
colnames(sum_data_1) <- c('mean','sd','mean-2*sd','mean+2*sd')
rownames(sum_data_1) <- colnames(data_m1)[1:49]

for(i in 1:49){
  sum_data_1[i,1] <- mean(data_m1[,i])
  sum_data_1[i,2] <- sd(data_m1[,i])
}
sum_data_1$`mean-2*sd` <- sum_data_1$mean-2*sum_data_1$sd
sum_data_1$`mean+2*sd` <- sum_data_1$mean+2*sum_data_1$sd

data_m <- data_m1

### select one measure for normalization

data_m[,1:49] <- as.data.frame(lapply(data_m [,1:49], 
                                      function(x){preProcess(as.data.frame(x), method=c("range")) %>% predict(as.data.frame(x))})) ### min-max normalize

write.csv(data_m,"./data/train.csv")

train_data <- data_m
train_data$dig <- as.factor(train_data$dig)

### pre-processing of 3 validation datasets ######################################
### FEP ##########################################################################
###
FEP_a1 <- FEP_data

######  clipping
for (i in 1:nrow(FEP_a1)){
  for (j in 1:49){
    if(FEP_a1[i,j] < sum_data[j,3]){FEP_a1[i,j] <- sum_data[j,3]}
    if(FEP_a1[i,j] > sum_data[j,4]){FEP_a1[i,j] <- sum_data[j,4]}
  }
}
FEP_mid <- FEP_a1

#####  scale by (x-min)/(max-min)
for (i in 1:nrow(FEP_a1)){
  for (j in 1:49){
    FEP_a1[i,j] <- (FEP_a1[i,j]-min(data_m1[,j]))/(max(data_m1[,j])-min(data_m1[,j]))
  }
}

FEP_data <- FEP_a1

write.csv(FEP_data,"./data/FEP.csv")

### outliers ##########################################################################

######  clipping
for (i in 1:nrow(Valida_out_tot)){
  for (j in 1:49){
    if(Valida_out_tot[i,j] < sum_data[j,3]){Valida_out_tot[i,j] <- sum_data[j,3]}
    if(Valida_out_tot[i,j] > sum_data[j,4]){Valida_out_tot[i,j] <- sum_data[j,4]}
  }
}
Valida_out_mid <- Valida_out_tot

#####  scale by (x-min)/(max-min)
for (i in 1:nrow(Valida_out_tot)){
  for (j in 1:49){
    Valida_out_tot[i,j] <- (Valida_out_tot[i,j]-min(data_m1[,j]))/(max(data_m1[,j])-min(data_m1[,j]))
  }
}

Valida_out <- Valida_out_tot
write.csv(Valida_out,"./data/outliers.csv")


### Incompleteness ##########################################################################

######  clipping
for (i in 1:nrow(Valida_na_tot)){
  for (j in 1:49){
    if(!is.na(Valida_na_tot[i,j])){
      if(Valida_na_tot[i,j] < sum_data[j,3]){Valida_na_tot[i,j] <- sum_data[j,3]}
      if(Valida_na_tot[i,j] > sum_data[j,4]){Valida_na_tot[i,j] <- sum_data[j,4]}
    }
  }
}

Valida_na_mid <- Valida_na_tot

#####  scale by (x-min)/(max-min)
for (i in 1:nrow(Valida_na_tot)){
  for (j in 1:49){
    if(!is.na(Valida_na_tot[i,j])){
      Valida_na_tot[i,j] <- (Valida_na_tot[i,j]-min(data_m1[,j]))/(max(data_m1[,j])-min(data_m1[,j]))
    }
  }
}


### impute NAs with caret knn
train_sex_bin <- train_data
train_sex_bin$Gender[train_sex_bin$Gender%in%c("F","女")] <- 0
train_sex_bin$Gender[train_sex_bin$Gender%in%c("M","男")] <- 1
train_sex_bin$Gender <- as.numeric(train_sex_bin$Gender)
train_sex_bin <- train_sex_bin[,colnames(Valida_na_tot)]

Valida_na_unimputed <- rbind(train_sex_bin, Valida_na_tot)

Valida_na_unimputed[,c(1:49,51:52)] <- knnImputation(Valida_na_unimputed[,c(1:49,51:52)], k = 5, meth = "weighAvg", scale = F)
Valida_na_imputed <- Valida_na_unimputed
Valida_na_imputed$Gender <- ifelse(Valida_na_imputed$Gender<0.5, 0, 1)

colnames(Valida_na_imputed) <- colnames(Valida_na_unimputed)
Valida_na_imputed <- Valida_na_imputed[(nrow(train_sex_bin)+1):nrow(Valida_na_imputed),]
Valida_na_imputed$Gender <- ifelse(Valida_na_imputed$Gender==0, 'F', 'M')

Valida_na_tot <- Valida_na_imputed
Valida_na <- Valida_na_tot

### save normalized NA data
write.csv(Valida_na,"./data/incompleteness.csv")
