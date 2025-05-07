sel_features <- read.table("features.txt")
sel_features <- apply(sel_features, 1, as.character)
sel_features[54] <- "dig"

##################
pp <- read.xlsx("181001_psychosis.xlsx", sheet="住院")
setDT(pp)
colnames(pp) <- paste("V", 1:12, sep = "")
colnames(pp)[c(9,10)] <- c("variable", "value")
p1 <- data.table::dcast(pp, V2+V3+V4+V6~variable,value.var="value",drop=TRUE,fill=0,function(x){x})
names(p1)[1] <- "Gender"
names(p1)[2] <- "Age"
names(p1)[3] <- "ID"
names(p1)[4]<-"dig"
names(p1)[which(names(p1)=="CK-MB")] <- "CKMB"
#
p1 <- subset(p1,select = unlist(as.data.frame(sel_features[-c(53)])))
temp <- which(p1$`CK`==0|p1$`LDL-C`==0|p1$PLT==0|p1$`EO%`== 0)
p1_1 <- p1[-c(temp),]
#p1_1$dig <- "P"
p1_1 <-subset(p1_1,!duplicated(subset(p1_1,select = "ID")))
#
p1_na <-na.omit(p1_1)
############################181001之后数据
pa1 <- read.xlsx("1106_psychosis.xlsx",sheet = "2011")
pa2 <- read.xlsx("1106_psychosis.xlsx",sheet = "2012")
pa3 <- read.xlsx("1106_psychosis.xlsx",sheet = "2013")
pa4 <- read.xlsx("1106_psychosis.xlsx",sheet = "2014")
pa5 <- read.xlsx("1106_psychosis.xlsx",sheet = "2015")
#
pb1 <- read.xlsx("1107_psychosis.xlsx",sheet = "2011")
pb2 <- read.xlsx("1107_psychosis.xlsx",sheet = "2012")
pb3 <- read.xlsx("1107_psychosis.xlsx",sheet = "2013")
pb4 <- read.xlsx("1107_psychosis.xlsx",sheet = "2014")
pb5 <- read.xlsx("1107_psychosis.xlsx",sheet = "2015")
#
pc1 <- read.xlsx("1108_psychosis.xlsx",sheet = "2011")
pc2 <- read.xlsx("1108_psychosis.xlsx",sheet = "2012")
pc3 <- read.xlsx("1108_psychosis.xlsx",sheet = "2013")
pc4 <- read.xlsx("1108_psychosis.xlsx",sheet = "2014")
pc5 <- read.xlsx("1108_psychosis.xlsx",sheet = "2015")
###################
setDT(pa1)
setDT(pa2)
setDT(pa3)
setDT(pa4)
setDT(pa5)
colnames(pa1) <-  paste("V", 1:14, sep = "")
colnames(pa2) <-  paste("V", 1:14, sep = "")
colnames(pa3) <-  paste("V", 1:14, sep = "")
colnames(pa4) <-  paste("V", 1:14, sep = "")
colnames(pa5) <-  paste("V", 1:14, sep = "")
colnames(pa1)[c(9,10)]<- c("variable","value")
colnames(pa2)[c(9,10)]<- c("variable","value")
colnames(pa3)[c(9,10)]<- c("variable","value")
colnames(pa4)[c(9,10)]<- c("variable","value")
colnames(pa5)[c(9,10)]<- c("variable","value")
pa <- rbind(pa1,pa2,pa3,pa4,pa5)
pa <- data.table::dcast(pa,V2+V3+V4+V6~variable,value.var="value",drop=TRUE,fill=0,function(x){x})
names(pa)[1] <- "Gender"
names(pa)[2] <- "Age"
names(pa)[3] <- "ID"
names(pa)[4] <- "dig"
pa$Gender[which(pa$Gender=="女")] <- "F"
pa$Gender[which(pa$Gender=="男")] <- "M"
names(pa) <- str_extract(string =names(pa), pattern = "[^[:blank:]]+")
pa <- subset(pa,select = unlist(as.data.frame(sel_features[-c(53)])))
temp <- which(pa$`CK`==0|pa$`LDL-C`==0|pa$PLT==0|pa$`EO%`== 0)
pa_1 <- pa[-c(temp),]
###
setDT(pb1)
setDT(pb2)
setDT(pb3)
setDT(pb4)
setDT(pb5)
colnames(pb1) <-  paste("V", 1:13, sep = "")
colnames(pb2) <-  paste("V", 1:13, sep = "")
pb3<-pb3[,-14]
colnames(pb3) <-  paste("V", 1:13, sep = "")
pb4<-pb4[,-14]
colnames(pb4) <-  paste("V", 1:13, sep = "")
pb5<-pb5[,-14]
colnames(pb5) <-  paste("V", 1:13, sep = "")
colnames(pb1)[c(9,10)]<- c("variable","value")
colnames(pb2)[c(9,10)]<- c("variable","value")
colnames(pb3)[c(9,10)]<- c("variable","value")
colnames(pb4)[c(9,10)]<- c("variable","value")
colnames(pb5)[c(9,10)]<- c("variable","value")
pb <- rbind(pb1,pb2,pb3,pb4,pb5)
pb <- data.table::dcast(pb,V2+V3+V4+V6~variable,value.var="value",drop=TRUE,fill=0,function(x){x})
names(pb)[1] <- "Gender"
names(pb)[2] <- "Age"
names(pb)[3] <- "ID"
names(pb)[4] <- "dig"
pb$Gender[which(pb$Gender=="女")] <- "F"
pb$Gender[which(pb$Gender=="男")] <- "M"
names(pb) <- str_extract(string =names(pb), pattern = "[^[:blank:]]+")

pb <- subset(pb,select = unlist(as.data.frame(sel_features[-c(53)])))
temp <- which(pb$`CK`==0|pb$`LDL-C`==0|pb$PLT==0|pb$`EO%`== 0)
pb_1 <- pb[-c(temp),]

###
setDT(pc1)
setDT(pc2)
setDT(pc3)
setDT(pc4)
setDT(pc5)
pc <- rbind(pc1,pc2,pc3,pc4,pc5)
colnames(pc) <-  paste("V", 1:14, sep = "")
colnames(pc)[c(9,10)]<- c("variable","value")
pc <- data.table::dcast(pc,V2+V3+V4+V6~variable,value.var="value",drop=TRUE,fill=0,function(x){x})
names(pc)[1] <- "Gender"
names(pc)[2] <- "Age"
names(pc)[3] <- "ID"
names(pc)[4] <- "dig"
pc$Gender[which(pc$Gender=="女")] <- "F"
pc$Gender[which(pc$Gender=="男")] <- "M"
names(pc) <- str_extract(string =names(pc), pattern = "[^[:blank:]]+")
pc <- subset(pc,select = unlist(as.data.frame(sel_features[-c(53)])))
temp <- which(pc$`CK`==0|pc$`LDL-C`==0|pc$PLT==0|pc$`EO%`== 0)
pc_1 <- pc[-c(temp),]

###
p_al <- rbind(pa_1,pb_1,pc_1)

p_al <- as.data.frame(p_al)
p_al[,-c(52:53)] <- apply(p_al[,-c(52:53)],2,as.numeric)
p_all <- rbind(p_al,p1_1)
p_all <- subset(p_all,!duplicated(subset(p_all,select = "ID")))
p_all[,1:51] <- as.data.frame(lapply(p_all[,1:51],as.numeric))
p_all <- na.omit(p_all)
