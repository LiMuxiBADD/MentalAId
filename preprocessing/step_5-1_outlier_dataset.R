######N
### outliers for validation
temp <- which(data_H_CNN$Chap==1)
data_H_chap1 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap1[,2:50])[,50:51]
data_H_chap1<-data_H_chap1 %>% cbind(out_infor)
data_H_chap1 <-data_H_chap1[which(data_H_chap1$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==2)
data_H_chap2 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap2[,2:50])[,50:51]
data_H_chap2<-data_H_chap2 %>% cbind(out_infor)
data_H_chap2 <-data_H_chap2[which(data_H_chap2$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==3)
data_H_chap3 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap3[,2:50])[,50:51]
data_H_chap3<-data_H_chap3 %>% cbind(out_infor)
data_H_chap3 <-data_H_chap3[which(data_H_chap3$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==7)
data_H_chap7 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap7[,2:50])[,50:51]
data_H_chap7<-data_H_chap7 %>% cbind(out_infor)
data_H_chap7 <-data_H_chap7[which(data_H_chap7$is_anomaly),]
#
#
temp <- which(data_H_CNN$Chap==8)
data_H_chap8 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap8[,2:50])[,50:51]
data_H_chap8<-data_H_chap8 %>% cbind(out_infor)
data_H_chap8 <-data_H_chap8[which(data_H_chap8$is_anomaly),]
#
#
temp <- which(data_H_CNN$Chap==9)
data_H_chap9 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap9[,2:50])[,50:51]
data_H_chap9<-data_H_chap9 %>% cbind(out_infor)
data_H_chap9 <-data_H_chap9[which(data_H_chap9$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==10)
data_H_chap10 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap10[,2:50])[,50:51]
data_H_chap10<-data_H_chap10 %>% cbind(out_infor)
data_H_chap10 <-data_H_chap10[which(data_H_chap10$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==11)
data_H_chap11 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap11[,2:50])[,50:51]
data_H_chap11<-data_H_chap11 %>% cbind(out_infor)
data_H_chap11 <-data_H_chap11[which(data_H_chap11$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==12)
data_H_chap12 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap12[,2:50])[,50:51]
data_H_chap12<-data_H_chap12 %>% cbind(out_infor)
data_H_chap12 <-data_H_chap12[which(data_H_chap12$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==13)
data_H_chap13 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap13[,2:50])[,50:51]
data_H_chap13<-data_H_chap13 %>% cbind(out_infor)
data_H_chap13 <-data_H_chap13[which(data_H_chap13$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==14)
data_H_chap14 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap14[,2:50])[,50:51]
data_H_chap14<-data_H_chap14 %>% cbind(out_infor)
data_H_chap14 <-data_H_chap14[which(data_H_chap14$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==15)
data_H_chap15 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap15[,2:50])[,50:51]
data_H_chap15<-data_H_chap15 %>% cbind(out_infor)
data_H_chap15 <-data_H_chap15[which(data_H_chap15$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==17)
data_H_chap17 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap17[,2:50])[,50:51]
data_H_chap17<-data_H_chap17 %>% cbind(out_infor)
data_H_chap17 <-data_H_chap17[which(data_H_chap17$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==18)
data_H_chap18 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap18[,2:50])[,50:51]
data_H_chap18<-data_H_chap18 %>% cbind(out_infor)
data_H_chap18 <-data_H_chap18[which(data_H_chap18$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==19)
data_H_chap19 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap19[,2:50])[,50:51]
data_H_chap19<-data_H_chap19 %>% cbind(out_infor)
data_H_chap19 <-data_H_chap19[which(data_H_chap19$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==21)
data_H_chap21 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap21[,2:50])[,50:51]
data_H_chap21<-data_H_chap21 %>% cbind(out_infor)
data_H_chap21 <-data_H_chap21[which(data_H_chap21$is_anomaly),]
#
data_H_CNN_out <- rbind(data_H_chap1,data_H_chap2,data_H_chap3,data_H_chap7,data_H_chap8,data_H_chap9,data_H_chap10,data_H_chap11,data_H_chap12,data_H_chap13,data_H_chap14,data_H_chap15,data_H_chap17,data_H_chap18,data_H_chap19,data_H_chap21)
data_H_CNN_out$dig <- "N"
data_H_CNN_out <- data_H_CNN_out[,c(1:52,58,56)]
data_H_CNN_out <- na.omit(data_H_CNN_out)
data_H_CNN_out <- data_H_CNN_out[,-c(55:56)]
#####
## chapter IV-VI
index <- which(data_H$Chap==4|data_H$Chap==5|data_H$Chap==6)
data_H_456 <- data_H[index,]
data_H_456 <- data_H_456[,c(2:58,1)]
data_H_456[,1:51] <- lapply(data_H_456[,c(1:51)],as.numeric)
data_H_456 <- na.omit(data_H_456)
data_H_456 <- subset(data_H_456, !duplicated(data_H_456,select = "ID"))
data_H_456$dig <- "N"
data_H_456[which(data_H_456$Chap==5),ncol(data_H_456)] <- "P"
data_H_456 <- data_H_456[,c(1:52,58,56)]

###
p_all_out <-p_icd2
p_all_out$dig <- "P"
out_infor<-outlier.isf(p_all_out[,2:50])[,50:51]
p_out <-p_all_out%>% cbind(out_infor)
p_out  <-p_out [which(p_out $is_anomaly),]
p_out  <- p_out [,-c(55:56)]

###
FEP_p_out <- FEP_a %>% filter(dig=="P")
out_infor<-outlier.isf(FEP_p_out[,2:50])[,50:51]
FEP_p_out <- FEP_p_out%>% cbind(out_infor)
FEP_p_out  <- FEP_p_out [which(FEP_p_out$is_anomaly),]
FEP_p_out  <- FEP_p_out [,-c(55:56)]

###
Valida_out_tot <- as.data.frame(rbind(data_H_CNN_out, data_H_456, p_out, FEP_p_out))
Valida_out_tot <- Valida_out_tot[,c(2:50,1,51:54)]
ind_dup <- which(Valida_out_tot$ID%in%data$ID|Valida_out_tot$ID%in%FEP_data$ID)

Valida_out_unprep <- Valida_out_tot
