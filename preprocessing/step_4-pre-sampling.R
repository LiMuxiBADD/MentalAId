### split FEP and training set
set.seed(111)
index <- sample(1:nrow(data_H_all),1000)
FEP_data_N <- as.data.frame(data_H_all[index,])
data_H_all <- as.data.frame(data_H_all[-index,])

index <- sample(1:nrow(FEP_p_unout),1000)
FEP_data_P <- as.data.frame(FEP_p_unout[index,])
FEP_train <- as.data.frame(FEP_p_unout[-index,])


FEP_data <- rbind(FEP_data_N, FEP_data_P)

data <- rbind(data_H_all,FEP_train, p_all_unout) %>% as.data.frame()
