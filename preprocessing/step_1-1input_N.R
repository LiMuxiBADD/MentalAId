library(openxlsx)
library(data.table)
library(reshape2)
library(cluster)
library(ggplot2)
library(stringr)
#
sel_features <- read.table("features.txt")
sel_features <- apply(sel_features, 1, as.character)
sel_features[54] <- "dig"
##################################################
h1 <- read.xlsx("181001_reg.xlsx", sheet="体检")
setDT(h1)
colnames(h1) <- paste("v", 1:9, sep =  "")
colnames(h1)[c(7,8)] <- c("variable", "value")
h1 <- data.table::dcast(h1, v2+v3+v4~variable,value.var="value",
                        drop=TRUE,fill=0,function(x){x})
names(h1)[1] <- "Gender"
names(h1)[2] <- "Age"
names(h1)[3] <- "ID"
names(h1)[which(names(h1)=="CK-MB")] <- "CKMB"
h1$Gender[which(h1$Gender=="女")] <- "F"
h1$Gender[which(h1$Gender=="男")] <- "M"
############################
h1 <- subset(h1,select = unlist(as.data.frame(sel_features[-c(53,54)])))
temp <- which(h1$`CK`==0|h1$`LDL-C`==0|h1$PLT==0|h1$`EO%`== 0)
h1_1 <- h1[-c(temp),]
h1_1[,1:51] <- as.data.frame(apply(h1_1[,1:51], 2, as.numeric))
############################
h1_c <-subset(h1_1,!duplicated(subset(h1_1,select = "ID")))
h1_c_na <-na.omit(h1_c)
h1_c_na$dig <- "N"

##################################
# data
a <- read.xlsx("2017_1.xlsx",sheet="首次入院（2017上半年）")
b <- read.xlsx("2017_2.xlsx", sheet="1")
c <- read.xlsx("2017_3.xlsx", sheet="2")

names(b) <- names(a)
names(c) <- names(a)
a[which(a[,5]=="HDL"),5] <-"HDL-C"
b[which(b[,5]=="HDL"),5] <-"HDL-C"
c[which(c[,5]=="HDL"),5] <-"HDL-C"
aa <- rbind(a,b,c)
#
setDT(aa)
colnames(aa) <- paste("V", 1:11, sep = "")
aa <- aa[-c(which(is.na(aa$V1))),]
colnames(aa)[c(5,6)] <- c("variable", "value")
aa <- data.table::dcast(aa,V1+V2+V3+V8~variable, value.var="value", drop=TRUE, fill=0, function(x){x})
names(aa)[1] <- "ID"
names(aa)[2] <- "Gender"
names(aa)[3] <- "Age"
names(aa)[4] <- "dig"
aa$Gender[which(aa$Gender=="女")] <- "F"
aa$Gender[which(aa$Gender=="男")] <- "M"
names(aa)[which(names(aa)=="NBIL")] <- "IBIL" 
names(aa)[which(names(aa)=="UREA")] <- "Urea"
names(aa)[which(names(aa)=="CRE")] <- "CREA"
names(aa)[which(names(aa)=="APOA1")] <- "apoA"
names(aa)[which(names(aa)=="APOB")] <- "apoB"
names(aa)[which(names(aa)=="CL")] <- "Cl"
names(aa)[which(names(aa)=="RDW")] <- "RDW-SD"
names(aa)[which(names(aa)=="TSH3")] <- "TSH"
names(aa)[which(names(aa)=="CK-MB")] <- "CKMB"
names(aa)[which(names(aa)=="IP")] <- "P"
names(aa)[which(names(aa)=="HDL")] <- "HDL-C"
############################
aa <- subset(aa,select = unlist(as.data.frame(sel_features[-53])))
aa <-subset(aa,!duplicated(subset(aa,select ="ID")))
aa_1 <-aa[-c(which(aa$`CK`==0|aa$`MCV`==0)),]
############################
aa_1 <-subset(aa_1,!duplicated(subset(aa_1,select = "ID")))
aa_1 <-na.omit(aa_1)
aa_1$HCT <- as.numeric(aa_1$HCT)*100

############################
aa_1$Age <- as.numeric(aa_1$Age)
aa_1 <-na.omit(aa_1)
