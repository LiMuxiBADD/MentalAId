########## FEP数据补充
t2019_m1 <-read.xlsx("2019_FEP.xlsx",sheet="1月-3月")
t2019_m2 <-read.xlsx("2019_FEP.xlsx",sheet="4月-6月")
t2019_m3 <-read.xlsx("2019_FEP.xlsx",sheet="7月-9月")
t2019_m4 <-read.xlsx("2019_FEP.xlsx",sheet="10月-12月")

t2019_b1 <-read.xlsx("2019_bc.xlsx",sheet="1月-3月")
t2019_b2 <-read.xlsx("2019_bc.xlsx",sheet="4月-6月")
t2019_b3 <-read.xlsx("2019_bc.xlsx",sheet="7月-9月")
t2019_b4 <-read.xlsx("2019_bc.xlsx",sheet="10月-12月")

t2020_1 <-read.xlsx("2020_FEP.xlsx",sheet="1月-3月")
t2020_2 <-read.xlsx("2020_FEP.xlsx",sheet="4月-6月")
t2020_3 <-read.xlsx("2020_FEP.xlsx",sheet="7月-9月")
t2020_4 <-read.xlsx("2020_FEP.xlsx",sheet="10月-12月")

setDT(t2019_m1)
setDT(t2019_m2)
setDT(t2019_m3)
setDT(t2019_m4)
t2019_m1 <- t2019_m1[,-1]
t2019_m2 <- t2019_m2[,-1]
t2019_m4 <- t2019_m4[,-1]

colnames(t2019_m1) <-  paste("V", 1:10, sep = "")
colnames(t2019_m2) <-  paste("V", 1:10, sep = "")
colnames(t2019_m3) <-  paste("V", 1:10, sep = "")
colnames(t2019_m4) <-  paste("V", 1:10, sep = "")

colnames(t2019_m1)[c(6,7)]<- c("variable","value")
colnames(t2019_m2)[c(6,7)]<- c("variable","value")
colnames(t2019_m3)[c(6,7)]<- c("variable","value")
colnames(t2019_m4)[c(6,7)]<- c("variable","value")

t2019_m <- rbind(t2019_m1,t2019_m2,t2019_m3,t2019_m4)

setDT(t2019_b1)
setDT(t2019_b2)
setDT(t2019_b3)
setDT(t2019_b4)
t2019_b1 <- t2019_b1[,-1]
t2019_b2 <- t2019_b2[,-1]
t2019_b3 <- t2019_b3[,-1]

colnames(t2019_b1) <-  paste("V", 1:10, sep = "")
colnames(t2019_b2) <-  paste("V", 1:10, sep = "")
colnames(t2019_b3) <-  paste("V", 1:10, sep = "")
colnames(t2019_b4) <-  paste("V", 1:10, sep = "")

colnames(t2019_b1)[c(6,7)]<- c("variable","value")
colnames(t2019_b2)[c(6,7)]<- c("variable","value")
colnames(t2019_b3)[c(6,7)]<- c("variable","value")
colnames(t2019_b4)[c(6,7)]<- c("variable","value")

t2019_b <- rbind(t2019_b1,t2019_b2,t2019_b3,t2019_b4)

setDT(t2020_1)
setDT(t2020_2)
setDT(t2020_3)
setDT(t2020_4)
t2020_1 <- t2020_1[,-1]
t2020_3 <- t2020_3[,-1]
t2020_4 <- t2020_4[,-1]

colnames(t2020_1) <-  paste("V", 1:10, sep = "")
colnames(t2020_2) <-  paste("V", 1:10, sep = "")
colnames(t2020_3) <-  paste("V", 1:10, sep = "")
colnames(t2020_4) <-  paste("V", 1:10, sep = "")

colnames(t2020_1)[c(6,7)]<- c("variable","value")
colnames(t2020_2)[c(6,7)]<- c("variable","value")
colnames(t2020_3)[c(6,7)]<- c("variable","value")
colnames(t2020_4)[c(6,7)]<- c("variable","value")

t2020 <- rbind(t2020_1,t2020_2,t2020_3,t2020_4)

t2019 <- rbind(t2019_m, t2019_b)
Test_sup <- rbind(t2019, t2020)

Test_sup <- data.table::dcast(Test_sup,V1+V2+V3+V4~variable,value.var="value",drop=TRUE,fill=0,function(x){x})
names(Test_sup)[1] <- "Gender"
names(Test_sup)[2] <- "Age"
names(Test_sup)[3] <- "ID"
names(Test_sup)[4] <- "dig"
Test_sup$Gender[which(Test_sup$Gender=="女")] <- "F"
Test_sup$Gender[which(Test_sup$Gender=="男")] <- "M"
names(Test_sup) <- str_extract(string =names(Test_sup), pattern = "[^[:blank:]]+")
names(Test_sup)[names(Test_sup)=='CK-MB'] <- 'CKMB'
Test_sup <- subset(Test_sup,select = setdiff(c(as.character(sel_features),"Age"),"ICD10"))
Test_sup_raw <- Test_sup
temp <- which(Test_sup$`CK`==0|Test_sup$`LDL-C`==0|Test_sup$PLT==0|Test_sup$`EO%`== 0)
Test_sup <- Test_sup[-c(temp),]

Test_sup[Test_sup==0] <- NA
Test_sup<-na.omit(Test_sup)
Test_sup <- subset(Test_sup,!duplicated(subset(Test_sup,select = "ID")))
id_pal <- p_all$ID
id_fep <- Test_c$ID
id_sup <- Test_sup$ID
temp <- which(id_sup%in%id_pal|id_sup%in%id_fep)
Test_sup <- Test_sup[-temp,]

### add age
pos <- function(P){str_replace(P, pattern = 'Y0M0D0H$', replacement = '')}
Test_sup$Age <- unlist(lapply(Test_sup$Age, pos))
pos <- function(P){str_replace(P, pattern = 'Y0M0D$', replacement = '')}
Test_sup$Age <- unlist(lapply(Test_sup$Age, pos))
Test_sup$Age <- unlist(lapply(Test_sup$Age, as.numeric))


###### ICD-10 standardization (psychosis)
t_sup <-merge(Test_sup,ICD_code[1:21640,c(1,3)],by = "dig",all.x = T)
list <- as.data.frame(table(t_sup$dig[which(is.na(t_sup$主要编码))]))
list <- list[order(list[,2],decreasing = T),]
##
b <- function(P){str_replace_all(string = P,pattern = "(^抑郁症$)",replacement = "F32.901")}
temp  <- unlist(lapply(t_sup$dig,b))
b <- function(P){str_replace_all(string = P,pattern = "(^心境障碍$)",replacement = "F71.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^心境\\[情感\\]障碍$)",replacement = "F71.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^强迫综合征$)",replacement = "F42.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^焦虑抑郁状态$)",replacement = "F41.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^妇科检查$)",replacement = "Z00.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^酒精依赖综合征$)",replacement = "F10.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指一般性检查$)",replacement = "Z00.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指精神分裂症$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^躁狂状态$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^惊恐障碍\\[间歇发作性焦虑\\]$)",replacement = "F41.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指心境障碍$)",replacement = "F71.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^重度抑郁发作$)",replacement = "F32.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指双相情感障碍$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^伴有精神病性症状重度抑郁发作$)",replacement = "F32.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍,目前为缓解状态$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^癫痫综合征$)",replacement = "G40.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^妄想状态$)",replacement = "F22.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^痛风病$)",replacement = "M10.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^酒精性精神和行为障碍$)",replacement = "F10.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^不伴有精神病性症状躁狂$)",replacement = "F30.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^癫病$)",replacement = "G40.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^月经病类$)",replacement = "N94.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指持久心境障碍$)",replacement = "F34.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^皮肤感染$)",replacement = "L08.902")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^精神发育迟缓$)",replacement = "F73.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^不寐病$)",replacement = "F51.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^复发性抑郁障碍,目前为中度发作$)",replacement = "F33.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^复发性抑郁障碍,目前为伴有精神病性症状的重度发作$)",replacement = "F33.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^轻度精神发育迟滞$)",replacement = "F70.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^器质性妄想性精神分裂症样障碍$)",replacement = "F06.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指幻觉$)",replacement = "F28.x01")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^精神发育迟滞$)",replacement = "F73.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^酒精中毒性精神障碍$)",replacement = "F10.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乙型肝炎.*$)",replacement = "B18.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乙肝.*$)",replacement = "B18.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*恶性肿瘤.*$)",replacement = "C80.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*恶性.*瘤.*$)",replacement = "C80.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^可疑疾病.*$)",replacement = "Unknown")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^可疑精神.*$)",replacement = "Unknown")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^精神活性物质滥用个人史.*$)",replacement = "Unknown")}
temp  <- unlist(lapply(temp,b))


###
t_sup$主要编码[which(!temp==t_sup$dig)] <- temp[which(!temp==t_sup$dig)]
t_sup <- t_sup %>% filter(主要编码!="Unknown")
names(t_sup)[54] <- "ICD10"
t_sup <- na.omit(t_sup)
p <- function(x){str_sub(x,1,1)}
fir1_ICD <- unlist(lapply(t_sup$ICD10,p))
index_n <- which(fir1_ICD!='F')
index_p <- which(fir1_ICD=='F')
##### non-psychosis chapter
t_sup_n <- t_sup[index_n,]
p <- function(x){str_sub(x,1,3)}
fir3_ICD <- unlist(lapply(t_sup_n$ICD10,p))
p <- function(x){str_sub(x,1,1)}
fir1_ICD <- unlist(lapply(t_sup_n$ICD10,p))
p <- function(x){str_sub(x,2,3)}
num_ICD <- unlist(lapply(t_sup_n$ICD10,p))
##### chapter
t_sup_n$"fir3" <- fir3_ICD 
t_sup_n$"fir1" <- fir1_ICD
t_sup_n <- as.data.frame(t_sup_n)
tt <- ncol(t_sup_n)+1
t_sup_n[which(t_sup_n$fir1=="A"|t_sup_n$fir1=="B"),tt] <- 1
t_sup_n[which(t_sup_n$fir1=="C"),tt] <- 2
t_sup_n$"ICDnum" <- num_ICD 
t_sup_n[which(t_sup_n$fir1=="D"&t_sup_n$ICDnum>=0&t_sup_n$ICDnum<=48),tt] <- 2
t_sup_n[which(t_sup_n$fir1=="D"&t_sup_n$ICDnum>=50&t_sup_n$ICDnum<=89),tt] <- 3
t_sup_n[which(t_sup_n$fir1=="E"),tt] <- 4
t_sup_n[which(t_sup_n$fir1=="F"),tt] <- 5
t_sup_n[which(t_sup_n$fir1=="G"),tt] <- 6
t_sup_n[which(t_sup_n$fir1=="H"&t_sup_n$ICDnum>=00&t_sup_n$ICDnum<=59),tt] <- 7
t_sup_n[which(t_sup_n$fir1=="H"&t_sup_n$ICDnum>=60&t_sup_n$ICDnum<=95),tt] <- 8
t_sup_n[which(t_sup_n$fir1=="I"),tt] <- 9
t_sup_n[which(t_sup_n$fir1=="J"),tt] <- 10
t_sup_n[which(t_sup_n$fir1=="K"),tt] <- 11
t_sup_n[which(t_sup_n$fir1=="L"),tt] <- 12
t_sup_n[which(t_sup_n$fir1=="M"),tt] <- 13
t_sup_n[which(t_sup_n$fir1=="N"),tt] <- 14
t_sup_n[which(t_sup_n$fir1=="O"),tt] <- 15
t_sup_n[which(t_sup_n$fir1=="P"),tt] <- 16
t_sup_n[which(t_sup_n$fir1=="Q"),tt] <- 17
t_sup_n[which(t_sup_n$fir1=="R"),tt] <- 18
t_sup_n[which(t_sup_n$fir1=="S"|t_sup_n$fir1=="T"),tt] <- 19
t_sup_n[which(t_sup_n$fir1=="Z"),tt] <- 21
names(t_sup_n)[57] <- "Chap"
t_sup_n$dig <- 'N'
################# rm chapter IV-VI
index <- which(t_sup_n$Chap==4|t_sup_n$Chap==5|t_sup_n$Chap==6)
t_sup_456 <- t_sup_n[index,]
t_sup_n <- t_sup_n[-index,]

##### psychosis chapter
t_sup_p <-t_sup[index_p,]
p <- function(x){str_sub(x,2,3)}
num_ICD <- unlist(lapply(t_sup_p$ICD10,p))
t_sup_p$num <- num_ICD
t_sup_p$icdF[which(t_sup_p$num<=9)] <- "p1"
t_sup_p$icdF[which(t_sup_p$num>=10&t_sup_p$num<=19)] <- "p2"
t_sup_p$icdF[which(t_sup_p$num>=20&t_sup_p$num<=29)] <- "p3"
t_sup_p$icdF[which(t_sup_p$num>=30&t_sup_p$num<=39)] <- "p4"
t_sup_p$icdF[which(t_sup_p$num>=40&t_sup_p$num<=49)] <- "p5"
t_sup_p$icdF[which(t_sup_p$num>=50&t_sup_p$num<=59)] <- "p6"
t_sup_p$icdF[which(t_sup_p$num>=60&t_sup_p$num<=69)] <- "p7"
t_sup_p$icdF[which(t_sup_p$num>=70&t_sup_p$num<=79)] <- "p8"
t_sup_p$icdF[which(t_sup_p$num>=80&t_sup_p$num<=89)] <- "p9"
t_sup_p$icdF[which(t_sup_p$num>=90&t_sup_p$num<=98)] <- "p10"
t_sup_p$icdF[which(t_sup_p$num==99)] <- "p11"
t_sup_p$icdF[which(is.na(t_sup_p$num))] <- "p_other"
t_sup_p<-t_sup_p[,c(2:53,1,56)]
t_sup_p$dig<- "P"
names(t_sup_p)[ncol(t_sup_p)] <- "Chap"


t_sup2 <- rbind(t_sup_n[,c(2:53,1,57)], t_sup_p)
pos <- function(P){str_replace(P, pattern = 'Y0M0D0H$', replacement = '')}
t_sup2$Age <- unlist(lapply(t_sup2$Age, pos))
pos <- function(P){str_replace(P, pattern = 'Y0M0D$', replacement = '')}
t_sup2$Age <- unlist(lapply(t_sup2$Age, pos))
t_sup2$Age <- unlist(lapply(t_sup2$Age, as.numeric))
summary(t_sup2$Age)
summary(t_sup2[t_sup2$dig=='P',]$Age)

########################
test<-read.xlsx("2017_FEP.xlsx",sheet="Sheet1")
setDT(test)
colnames(test) <- paste("V",1:13,sep = "")
colnames(test)[c(9,10)] <- c("variable","value1")
test[which(is.na(as.numeric(test$value1))),"value1"] <- "0"
test$value1 <- as.numeric(test$value1)
test <- test[,-"V11"]
test <- test[,-"V12"]
test <- test[,-"V13"]
t <- data.table::dcast(test,V2+V3+V4+V6~variable,value.var="value1",drop=TRUE,fill=0,function(x){x})
names(t)[1]<-'Gender'
names(t)[2]<-"Age"
names(t)[3]<-"ID" 
names(t)[4]<-"dig"
names(t)[which(names(t)=='CK-MB')] <- 'CKMB'
sel_tt <- subset(t,select = unlist(as.data.frame(sel_features[-53])))
temp <- which(sel_tt$`CK`==0|sel_tt$`LDL-C`==0|sel_tt$PLT==0|sel_tt$`EO%`== 0)
sel_tt_1 <- sel_tt[-c(temp),]
sel_tt_1[sel_tt_1==0] <- NA
Test<-na.omit(sel_tt_1)
Test_c <- subset(Test, !duplicated(Test))

################################
id_pal <- p_all$ID
id_fep <- Test_c$ID
temp <- which(id_fep%in%id_pal)
Test_c1 <- Test_c[-temp,]
Test_icd <-merge(Test_c1,ICD_code[,c(1,3)],by = "dig",all.x = T)
b <- function(P){str_replace_all(string = P,pattern = "(^抑郁症  $)",replacement = "F32.901")}
temp  <- unlist(lapply(Test_icd$dig,b))
b <- function(P){str_replace_all(string = P,pattern = "(^抑郁症$)",replacement = "F32.901")}
temp  <- unlist(lapply(Test_icd$dig,b))
b <- function(P){str_replace_all(string = P,pattern = "(^抑郁症  $)",replacement = "F32.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^躁狂症$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^情绪障碍$)",replacement = "F93.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^躁狂状态$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^伴有精神病性症状躁狂$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^重度抑郁发作$)",replacement = "F32.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急促而短暂的精神病性障碍$)",replacement = "F23.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^妄想状态$)",replacement = "F23.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍，目前为轻躁狂发作$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍，躁狂相$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍，目前为轻度抑郁发作$)",replacement = "F31.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^分裂症$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^不伴有精神病性症状的躁狂发作$)",replacement = "F30.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^伴有精神病性症状重度抑郁发作$)",replacement = "F32.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性而短暂的精神障碍$)",replacement = "F23.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性而短暂性的精神障碍$)",replacement = "F23.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍，目前为轻躁狂发作  $)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^轻度精神发育迟缓$)",replacement = "F70.000")}
temp  <- unlist(lapply(temp,b))

Test_icd$主要编码[which(!temp==Test_icd$dig)] <- temp[which(!temp==Test_icd$dig)]
names(Test_icd)[54] <- "ICD10"
p <- function(x){str_sub(x,1,3)}
fir3_ICD_Test <- unlist(lapply(Test_icd$ICD10,p))
p <- function(x){str_sub(x,1,1)}
fir1_ICD_Test <- unlist(lapply(Test_icd$ICD10,p))
p <- function(x){str_sub(x,2,3)}
num_ICD_Test <- unlist(lapply(Test_icd$ICD10,p))
Test_icd$num<- as.numeric(num_ICD_Test)

Test_icd2 <-Test_icd
Test_icd2$icdF <- Test_icd$num
Test_icd2$icdF[which(Test_icd2$num<=9)] <- "p1"
Test_icd2$icdF[which(Test_icd2$num>=10&Test_icd2$num<=19)] <- "p2"
Test_icd2$icdF[which(Test_icd2$num>=20&Test_icd2$num<=29)] <- "p3"
Test_icd2$icdF[which(Test_icd2$num>=30&Test_icd2$num<=39)] <- "p4"
Test_icd2$icdF[which(Test_icd2$num>=40&Test_icd2$num<=49)] <- "p5"
Test_icd2$icdF[which(Test_icd2$num>=50&Test_icd2$num<=59)] <- "p6"
Test_icd2$icdF[which(Test_icd2$num>=60&Test_icd2$num<=69)] <- "p7"
Test_icd2$icdF[which(Test_icd2$num>=70&Test_icd2$num<=79)] <- "p8"
Test_icd2$icdF[which(Test_icd2$num>=80&Test_icd2$num<=89)] <- "p9"
Test_icd2$icdF[which(Test_icd2$num>=90&Test_icd2$num<=98)] <- "p10"
Test_icd2$icdF[which(Test_icd2$num==99)] <- "p11"
Test_icd2$icdF[which(is.na(Test_icd2$num))] <- "p_other"#others
Test_icd2<-Test_icd2[,c(2:53,1,56)]
Test_icd2$dig<- "P"
names(Test_icd2)[ncol(Test_icd2)] <- "Chap"
####
FEP_sum <- rbind(t_sup2, Test_icd2)
summary(FEP_sum$Age)

FEP_sum[,2:50] <- as.data.frame(lapply(FEP_sum[,2:50], as.numeric))
FEP_sum <- na.omit(FEP_sum)
FEP_a <- as.data.frame(FEP_sum)

FEP_na <- filter(FEP_a, !Gender%in%c('F','M'))
FEP_a <- filter(FEP_a, Gender%in%c('F','M'))
