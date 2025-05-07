ICD_code <- read.xlsx("ICD-10疾病分类与代码国标版.xlsx", sheet="Sheet1")
names(ICD_code)[3] <- "dig"

p_icd <-merge(p_all,ICD_code[,c(1,3)],by = "dig",all.x = T)
b <- function(P){str_replace_all(string = P,pattern = "(^抑郁症$)",replacement = "F32.901")}
temp  <- unlist(lapply(p_icd$dig,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相障碍$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))#无法明确归类，暂归为F31
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍，目前为$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))#无法明确归类，暂归为F31
b <- function(P){str_replace_all(string = P,pattern = "(^躁狂症$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))#躁狂发作 F30.900
b <- function(P){str_replace_all(string = P,pattern = "(^躁狂状态$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))#躁狂发作 F30.900
b <- function(P){str_replace_all(string = P,pattern = "(^焦虑症$)",replacement = "F41.101")}
temp  <- unlist(lapply(temp,b))#焦虑状态 F41.101
b <- function(P){str_replace_all(string = P,pattern = "(^重度抑郁发作$)",replacement = "F32.900")}
temp  <- unlist(lapply(temp,b))#抑郁发作 F32.900
b <- function(P){str_replace_all(string = P,pattern = "(^酒精依赖综合征$)",replacement = "F10.200")}
temp  <- unlist(lapply(temp,b))#使用酒精引起的依赖综合征 F10.200
b <- function(P){str_replace_all(string = P,pattern = "(^酒精所致精神障碍$)",replacement = "F10.000")}
temp  <- unlist(lapply(temp,b))#急性酒精中毒引起的精神和行为障碍 F10.000
b <- function(P){str_replace_all(string = P,pattern = "(^酒精性精神和行为障碍$)",replacement = "F10.000")}
temp  <- unlist(lapply(temp,b))#急性酒精中毒引起的精神和行为障碍 F10.000
b <- function(P){str_replace_all(string = P,pattern = "(^酒依赖$)",replacement = "F10.000")}
temp  <- unlist(lapply(temp,b))#急性酒精中毒引起的精神和行为障碍 F10.000
b <- function(P){str_replace_all(string = P,pattern = "(^失眠症$)",replacement = "G47.000")}
temp  <- unlist(lapply(temp,b))#初发性或维持性睡眠障碍［失眠症］G47.000
b <- function(P){str_replace_all(string = P,pattern = "(^精神发育迟滞$)",replacement = "F73.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^酒精中毒性精神障碍$)",replacement = "F10.900")}
temp  <- unlist(lapply(temp,b))#使用酒精引起的精神和行为障碍 F10.900
b <- function(P){str_replace_all(string = P,pattern = "(^双相情感障碍$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^精神发育迟缓$)",replacement = "F73.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^强迫症$)",replacement = "F42.003")}
temp  <- unlist(lapply(temp,b))#强迫状态 F42.003
b <- function(P){str_replace_all(string = P,pattern = "(^强迫症$)",replacement = "F42.003")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双相性情感障碍$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性而短暂的精神病性$)",replacement = "F23.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^癫痫所致精神障碍$)",replacement = "F06.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^伴有精神病性症状躁狂$)",replacement = "F30.200")}
temp  <- unlist(lapply(temp,b))#伴有精神病性症状的躁狂 F30.200
b <- function(P){str_replace_all(string = P,pattern = "(^伴有精神病性症状躁狂$)",replacement = "F30.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^复发性抑郁障碍，目前$)",replacement = "F33.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^轻度精神发育迟缓$)",replacement = "F70.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^伴有精神病性症状重度抑郁发作$)",replacement = "F32.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^SCH$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^复发性抑郁症$)",replacement = "F32.902")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^精神分裂症   $)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^惊恐障碍$)",replacement = "F41.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^中度精神发育迟缓$)",replacement = "F71.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^心境障碍$)",replacement = "F71.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性短暂精神病性障碍$)",replacement = "F23.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^惊恐障碍[间歇发作性焦虑] $)",replacement = "F41.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^不伴有精神病性症状躁狂$)",replacement = "F30.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^sch$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
### advanced
b <- function(P){str_replace_all(string = P,pattern = "(^.*精神分裂.*$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*分裂样.*$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*分裂.*$)",replacement = "F20.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*双相.*$)",replacement = "F31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*酒精.*$)",replacement = "F10.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*抑郁.*$)",replacement = "F32.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*躁狂.*$)",replacement = "F30.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*兴奋剂.*$)",replacement = "F15.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*发育.*$)",replacement = "F89.000")}
temp  <- unlist(lapply(temp,b))


###
p_icd$主要编码[which(!temp==p_icd$dig)] <- temp[which(!temp==p_icd$dig)]
names(p_icd)[54] <- "ICD10"
p <- function(x){str_sub(x,1,3)}
fir3_ICD_p <- unlist(lapply(p_icd$ICD10,p))
p <- function(x){str_sub(x,1,1)}
fir1_ICD_p <- unlist(lapply(p_icd$ICD10,p))
p <- function(x){str_sub(x,2,3)}
num_ICD_p <- unlist(lapply(p_icd$ICD10,p))
p_icd$num<- as.numeric(num_ICD_p)

tmp1 <- p_icd[is.na(p_icd$ICD10),"dig"] %>% table()

p_icd2 <-p_icd
p_icd2$icdF <- p_icd$num
p_icd2$icdF[which(p_icd2$num<=9)] <- "p1"
p_icd2$icdF[which(p_icd2$num>=10&p_icd2$num<=19)] <- "p2"
p_icd2$icdF[which(p_icd2$num>=20&p_icd2$num<=29)] <- "p3"
p_icd2$icdF[which(p_icd2$num>=30&p_icd2$num<=39)] <- "p4"
p_icd2$icdF[which(p_icd2$num>=40&p_icd2$num<=49)] <- "p5"
p_icd2$icdF[which(p_icd2$num>=50&p_icd2$num<=59)] <- "p6"
p_icd2$icdF[which(p_icd2$num>=60&p_icd2$num<=69)] <- "p7"
p_icd2$icdF[which(p_icd2$num>=70&p_icd2$num<=79)] <- "p8"
p_icd2$icdF[which(p_icd2$num>=80&p_icd2$num<=89)] <- "p9"
p_icd2$icdF[which(p_icd2$num>=90&p_icd2$num<=98)] <- "p10"
p_icd2$icdF[which(p_icd2$num==99)] <- "p11"
p_icd2$icdF[which(is.na(p_icd2$num))] <- "p_other"
p_icd2<-p_icd2[,c(2:53,1,56)]
p_icd2$dig<- "P"
names(p_icd2)[ncol(p_icd2)] <- "Chap"

