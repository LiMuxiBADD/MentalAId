### merge all data (HC & MI & P & FEP)
h1_c_na <- mutate(h1_c_na, Chap="21")
data_H_CNN <- FEP_a %>% filter(dig=="N") %>% rbind(data_H_CNN) %>% rbind(h1_c_na)

###### empty value remove
data_H_CNN[data_H_CNN==0] <- NA
data_H_CNN <- na.omit(data_H_CNN)
p_icd2[p_icd2==0] <- NA
p_icd2 <- na.omit(p_icd2)
### 
temp <- which(data_H_CNN$Chap==1)
data_H_chap1 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap1[,2:50])[,50:51]
data_H_chap1<-data_H_chap1 %>% cbind(out_infor)
data_H_chap1 <-data_H_chap1[-which(data_H_chap1$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==2)
data_H_chap2 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap2[,2:50])[,50:51]
data_H_chap2<-data_H_chap2 %>% cbind(out_infor)
data_H_chap2 <-data_H_chap2[-which(data_H_chap2$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==3)
data_H_chap3 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap3[,2:50])[,50:51]
data_H_chap3<-data_H_chap3 %>% cbind(out_infor)
data_H_chap3 <-data_H_chap3[-which(data_H_chap3$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==7)
data_H_chap7 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap7[,2:50])[,50:51]
data_H_chap7<-data_H_chap7 %>% cbind(out_infor)
data_H_chap7 <-data_H_chap7[-which(data_H_chap7$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==8)
data_H_chap8 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap8[,2:50])[,50:51]
data_H_chap8<-data_H_chap8 %>% cbind(out_infor)
data_H_chap8 <-data_H_chap8[-which(data_H_chap8$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==9)
data_H_chap9 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap9[,2:50])[,50:51]
data_H_chap9<-data_H_chap9 %>% cbind(out_infor)
data_H_chap9 <-data_H_chap9[-which(data_H_chap9$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==10)
data_H_chap10 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap10[,2:50])[,50:51]
data_H_chap10<-data_H_chap10 %>% cbind(out_infor)
data_H_chap10 <-data_H_chap10[-which(data_H_chap10$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==11)
data_H_chap11 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap11[,2:50])[,50:51]
data_H_chap11<-data_H_chap11 %>% cbind(out_infor)
data_H_chap11 <-data_H_chap11[-which(data_H_chap11$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==12)
data_H_chap12 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap12[,2:50])[,50:51]
data_H_chap12<-data_H_chap12 %>% cbind(out_infor)
data_H_chap12 <-data_H_chap12[-which(data_H_chap12$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==13)
data_H_chap13 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap13[,2:50])[,50:51]
data_H_chap13<-data_H_chap13 %>% cbind(out_infor)
data_H_chap13 <-data_H_chap13[-which(data_H_chap13$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==14)
data_H_chap14 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap14[,2:50])[,50:51]
data_H_chap14<-data_H_chap14 %>% cbind(out_infor)
data_H_chap14 <-data_H_chap14[-which(data_H_chap14$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==15)
data_H_chap15 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap15[,2:50])[,50:51]
data_H_chap15<-data_H_chap15 %>% cbind(out_infor)
data_H_chap15 <-data_H_chap15[-which(data_H_chap15$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==17)
data_H_chap17 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap17[,2:50])[,50:51]
data_H_chap17<-data_H_chap17 %>% cbind(out_infor)
data_H_chap17 <-data_H_chap17[-which(data_H_chap17$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==18)
data_H_chap18 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap18[,2:50])[,50:51]
data_H_chap18<-data_H_chap18 %>% cbind(out_infor)
data_H_chap18 <-data_H_chap18[-which(data_H_chap18$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==19)
data_H_chap19 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap19[,2:50])[,50:51]
data_H_chap19<-data_H_chap19 %>% cbind(out_infor)
data_H_chap19 <-data_H_chap19[-which(data_H_chap19$is_anomaly),]
#
temp <- which(data_H_CNN$Chap==21)
data_H_chap21 <- data_H_CNN[temp,] 
out_infor<-outlier.isf(data_H_chap21[,2:50])[,50:51]
data_H_chap21<-data_H_chap21 %>% cbind(out_infor)
data_H_chap21 <-data_H_chap21[-which(data_H_chap21$is_anomaly),]
#
data_H_CNN_unout <- rbind(data_H_chap1,data_H_chap2,data_H_chap3,data_H_chap7,data_H_chap8,data_H_chap9,data_H_chap10,data_H_chap11,data_H_chap12,data_H_chap13,data_H_chap14,data_H_chap15,data_H_chap17,data_H_chap18,data_H_chap19,data_H_chap21)
#####
data_H_CNN_unout$dig <- "N"

#######
data_H_all <- data_H_CNN_unout[,-c(55:56)]
data_H_all$dig <-"N"
###################################################### P
### remove outliers
p_all_out <-p_icd2
p_all_out$dig <- "P"
out_infor<-outlier.isf(p_all_out[,2:50])[,50:51]
p_all_unout <- p_all_out%>% cbind(out_infor)
p_all_unout  <- p_all_unout [-which(p_all_unout$is_anomaly),]
p_all_unout  <- p_all_unout [,-c(55:56)]

############# remove outliers FEP_P
FEP_p_out <- FEP_a %>% filter(dig=="P")
out_infor<-outlier.isf(FEP_p_out[,2:50])[,50:51]
FEP_p_unout <- FEP_p_out%>% cbind(out_infor)
FEP_p_unout  <- FEP_p_unout [-which(FEP_p_unout$is_anomaly),]
FEP_p_unout  <- FEP_p_unout [,-c(55:56)]

