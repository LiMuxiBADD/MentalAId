
# Negative set selection for incomplete records--------------------------------------------------

## health sample
h_na <- as.data.frame(rep(0,nrow(h1)),ncol=1)
colnames(h_na)[1] <- 'sumNa'
for(i in 1:nrow(h1)){
  h_na[i,] <- length(which(h1[i,]==0))
}
table(h_na[h_na!=0,])
table(h_na$sumNa)
index_h <- which(h_na$sumNa==1|h_na$sumNa==2|h_na$sumNa==3)

h_na <-h1[index_h,]
h_na$dig <- 'N'
h_na <- h_na[,c(2:50,1,51:53)]
h_na$Chap <- "health"

### multiple illness
a_na <- as.data.frame(aa)
a_na[,2:50] <- as.data.frame(apply(a_na[,2:50], 2, as.numeric))
a_na[is.na(a_na)] <- 0

a_na$sumNA <- rep(1,nrow(a_na))

for(i in 1:nrow(a_na)){
  a_na[i,'sumNA'] <- length(which(a_na[i,2:50]==0))
}
table(a_na$sumNA)
index_h <- which(a_na$sumNA>=1&a_na$sumNA<=3)

a_na <- a_na[index_h,]

table(a_na$sumNA)
a_na <- a_na[,-54]

# negative set ICD normalization -------------------------------------------
############################################################################
a_na_icd <-merge(a_na,ICD_code[1:21640,c(1,3)],by = "dig",all.x = T)
##
list <- as.data.frame(table(a_na_icd$dig[which(is.na(a_na_icd$主要编码))]))
list <- list[order(list[,2],decreasing = T),]
b <- function(P){str_replace_all(string = P,pattern = "(^孕[0-9]{1,3}周宫内妊娠$)",replacement = "Z32.100")}
temp  <- unlist(lapply(a_na_icd$dig,b))
b <- function(P){str_replace_all(string = P,pattern = "(^腰椎间盘突出症$)",replacement = "M51.202")}               
temp  <- unlist(lapply(temp,b)) 
b <- function(P){str_replace_all(string = P,pattern = "(^腹股沟疝$)",replacement = "K40.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺部阴影$)",replacement = "R91.x00")}               
temp  <- unlist(lapply(temp,b)) 
b <- function(P){str_replace_all(string = P,pattern = "(^直肠癌$)",replacement = "C20.x00")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺恶性肿瘤$)",replacement = "C34.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺癌$)",replacement = "C50.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^头部外伤$)",replacement = "S09.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃癌$)",replacement = "C16.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颈椎病$)",replacement = "M99.303")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^腰椎管狭窄症$)",replacement = "M99.403")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肝癌$)",replacement = "C22.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胆囊结石$)",replacement = "K80.200")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑出血$)",replacement = "I61.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^高血压病$)",replacement = "I10.x00")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^先天性心脏病$)",replacement = "Q24.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乙状结肠癌$)",replacement = "C18.700")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳房诊断性影像检查异常所见$)",replacement = "R92.x00")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾炎综合症$)",replacement = "N05.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胆囊结石伴急性胆囊炎$)",replacement = "K80.000")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^慢性阻塞性肺病伴急性加重$)",replacement = "J44.100")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^子宫肌瘤$)",replacement = "N85.806")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肝功能异常$)",replacement = "R94.500")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^贲门癌$)",replacement = "C16.000")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^膝骨性关节炎$)",replacement = "M13.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^蛛网膜下腔出血$)",replacement = "I60.900")}               
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^糖尿病$)",replacement = "E14.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾功能异常$)",replacement = "R94.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^新生儿肺炎$)",replacement = "P23.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脊柱压缩性骨折$)",replacement = "M48.305")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾盂积水伴肾输尿管结石$)",replacement = "N13.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^面神经麻痹$)",replacement = "G51.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^膝关节半月板损伤$)",replacement = "M23.308")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^甲状腺癌$)",replacement = "C73.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^结肠癌$)",replacement = "C18.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^血小板减少症$)",replacement = "D69.600")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺癌术后化疗$)",replacement = "C50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^干燥综合征$)",replacement = "M35.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺恶性肿瘤$)",replacement = "C50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^突发性耳聋$)",replacement = "H91.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^腰椎滑脱症$)",replacement = "M43.006")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^可疑疾病观察$)",replacement = "Z03.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^宫颈癌$)",replacement = "C53.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^甲状腺机能亢进症$)",replacement = "E05.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^子宫瘢痕妊娠$)",replacement = "O00.807")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^升结肠癌$)",replacement = "C18.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脂肪瘤$)",replacement = "D17.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^血管瘤$)",replacement = "D18.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^前列腺动态未定肿瘤$)",replacement = "D40.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^短暂性脑缺血发作$)",replacement = "G45.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃窦癌$)",replacement = "C16.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^早产儿$)",replacement = "P07.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肿瘤$)",replacement = "D48.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^带状疱疹$)",replacement = "B02.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性髓系白血病$)",replacement = "C92.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^特指慢性阻塞性肺病$)",replacement = "J44.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^子宫脱垂$)",replacement = "N81.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^心功能不全$)",replacement = "I50.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃溃疡$)",replacement = "K25.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^幼年型类风湿关节炎$)",replacement = "M08.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑出血后遗症$)",replacement = "I69.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^主动脉夹层$)",replacement = "I71.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳房肿物$)",replacement = "N64.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^身体体表小于10％烧伤$)",replacement = "T31.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨性关节炎$)",replacement = "M13.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^多指[趾]畸形$)",replacement = "Q69.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^静脉畸形$)",replacement = "Q26.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^不稳定性心绞痛$)",replacement = "I20.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^子宫腺肌症$)",replacement = "N80.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^弥漫大B细胞非霍奇金淋巴瘤$)",replacement = "C83.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^恶性淋巴瘤$)",replacement = "Z85.701")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^多器官功能障碍综合征(MODS)$)",replacement = "R65.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^慢性髓系白血病$)",replacement = "C92.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^早孕、妊娠$)",replacement = "Z32.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃体癌$)",replacement = "C16.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺炎性疾患$)",replacement = "N61.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^前列腺癌内分泌治疗$)",replacement = "C61.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^大隐静脉曲张伴溃疡$)",replacement = "I83.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^粘膜皮肤淋巴结综合征[川崎病]$)",replacement = "M30.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^腓骨骨折$)",replacement = "S82.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^贲门失弛缓症$)",replacement = "K22.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨折$)",replacement = "T14.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^横结肠癌$)",replacement = "C18.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^白细胞减少症$)",replacement = "R72.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^第三[动眼]神经麻痹$)",replacement = "H49.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^进行性系统性硬化症$)",replacement = "M34.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^一氧化碳毒性效应$)",replacement = "T58.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^多形红斑$)",replacement = "L51.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^尺骨桡骨远端骨折$)",replacement = "S52.600")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^格雷夫斯病$)",replacement = "E05.003")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^漏出性中耳炎$)",replacement = "H65.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肝癌术后$)",replacement = "C22.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胰头癌$)",replacement = "C25.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颅内感染$)",replacement = "G06.006")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳房假体和植入物机械性并发症$)",replacement = "T85.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^多处烧伤$)",replacement = "T29.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^扁桃体腺样体肥大$)",replacement = "J35.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^支气管扩张$)",replacement = "J47.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃癌术后$)",replacement = "C16.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^身体体表10～19％烧伤$)",replacement = "T31.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食道中下段恶性肿瘤$)",replacement = "C15.802")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^Ⅱ度腭裂$)",replacement = "Q35.905")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^上颌骨骨折$)",replacement = "S02.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^前列腺增生(PSA升高)$)",replacement = "N40.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^卵巢子宫内膜异位囊肿$)",replacement = "N80.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^发热(待查)$)",replacement = "R50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^直肠乙状结肠交接处癌$)",replacement = "D01.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^糖尿病性肾病$)",replacement = "E10.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^结肠肝曲癌$)",replacement = "C18.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺脓肿$)",replacement = "J85.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胰腺恶性肿瘤$)",replacement = "C25.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^腘窝囊肿$)",replacement = "M71.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^降结肠癌$)",replacement = "C18.600")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨质疏松伴病理性骨折$)",replacement = "M80.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^2型糖尿病伴多并发症$)",replacement = "E11.700")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^下肢静脉曲张$)",replacement = "I83.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺癌术前化疗$)",replacement = "C50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺癌术后$)",replacement = "C50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^多发性肋骨骨折$)",replacement = "S22.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^多处二度烧伤$)",replacement = "T29.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^大脑动脉栓塞脑梗死$)",replacement = "I63.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性早幼粒细胞性白血病$)",replacement = "C92.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^皮肤慢性溃疡$)",replacement = "L98.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^直肠癌术后$)",replacement = "C20.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胆管结石$)",replacement = "K80.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑卒中$)",replacement = "I64.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^腹主动脉瘤$)",replacement = "I71.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^过敏性湿疹$)",replacement = "L20.802")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颈椎骨折$)",replacement = "S12.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食管癌化疗$)",replacement = "D00.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食管粘膜不典型增生$)",replacement = "K22.812")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨质疏松症$)",replacement = "M81.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乙状结肠癌术后$)",replacement = "C18.700")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺癌术后放疗$)",replacement = "C50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乳腺良性肿瘤$)",replacement = "D24.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^包茎$)",replacement = "N47.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^十二指肠溃疡伴出血$)",replacement = "K26.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双侧感音神经性耳聋$)",replacement = "H90.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^右乳房肿块$)",replacement = "N63.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^异常阴道出血$)",replacement = "N93.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^心包积液$)",replacement = "I31.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^恙虫病$)",replacement = "A75.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^手舟状骨骨折$)",replacement = "S62.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^皮肤感染$)",replacement = "L08.902")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^糖尿病性酮症酸中毒$)",replacement = "E10.101")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺动态未定肿瘤$)",replacement = "D38.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺大泡$)",replacement = "J43.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺癌术后$)",replacement = "C34.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾结石伴输尿管结石$)",replacement = "N20.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胆石症$)",replacement = "K80.800")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胆管结石伴胆管炎$)",replacement = "K80.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑动脉瘤$)",replacement = "I67.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑外伤后遗症$)",replacement = "F07.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑干出血$)",replacement = "I61.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脾大$)",replacement = "R16.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^贝赫切特综合征$)",replacement = "M35.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^鞍区病变$)",replacement = "R90.802")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食管下段恶性肿瘤$)",replacement = "C15.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨继发恶性肿瘤$)",replacement = "C79.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨转移癌$)",replacement = "C79.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^免疫性血小板减少性紫癜$)",replacement = "D69.404")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^十二指肠溃疡$)",replacement = "K26.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双乳房肿块$)",replacement = "N63.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^发热(待查？)$)",replacement = "R50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^口腔粘膜溃疡$)",replacement = "K12.109")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^吉兰-巴雷综合征$)",replacement = "G61.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^咽部异物$)",replacement = "T17.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^嗜酸细胞增多症$)",replacement = "D72.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^妊娠剧吐伴代谢紊乱$)",replacement = "O21.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^子宫粘膜下平滑肌瘤$)",replacement = "D25.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^宫颈上皮内瘤样变$)",replacement = "N87.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^小脑出血$)",replacement = "I61.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^尺骨桡骨骨干骨折$)",replacement = "S52.400")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^异位妊娠(可能)$)",replacement = "O00.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^弥漫性肺间质纤维化$)",replacement = "J84.106")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性肠炎$)",replacement = "K52.904")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^慢性非萎缩性胃炎$)",replacement = "K29.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^泌尿系统疾病$)",replacement = "N39.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^消化系统疾病$)",replacement = "K92.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^病窦综合征$)",replacement = "I49.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^股骨粗隆下骨折$)",replacement = "S72.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾盂积水伴输尿管狭窄$)",replacement = "N13.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃溃疡恶变$)",replacement = "K25.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃间质细胞瘤$)",replacement = "C16.901")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑继发恶性肿瘤$)",replacement = "C79.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^良性肿瘤$)",replacement = "D36.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^血尿(待查)$)",replacement = "R31.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^视神经脊髓炎$)",replacement = "G36.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颊粘膜恶性肿瘤$)",replacement = "C06.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食管疾患$)",replacement = "K22.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食管癌术后$)",replacement = "C15.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^骨折不连接$)",replacement = "M84.101")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^龟头型尿道下裂$)",replacement = "Q54.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^B细胞淋巴瘤$)",replacement = "C85.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^低血糖症$)",replacement = "E16.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^创伤性硬膜下血肿$)",replacement = "I62.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^创伤性鼓膜穿孔$)",replacement = "S09.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^原发性血小板增多症$)",replacement = "D75.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^双侧腹股沟疝$)",replacement = "K40.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^发热$)",replacement = "R50.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^古典生物型霍乱$)",replacement = "A00.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^右股骨颈骨折$)",replacement = "S72.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^妊娠伴糖尿病$)",replacement = "O24.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^妊娠合并宫颈机能不全$)",replacement = "O34.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^室性早搏$)",replacement = "I49.300")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^宫颈上皮内肿瘤Ⅱ级$)",replacement = "N87.101")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^对精神有影响药物中毒$)",replacement = "T43.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^左心衰竭$)",replacement = "I50.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^异位妊娠$)",replacement = "O00.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性上消化道出血$)",replacement = "K92.207")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^急性透壁心肌梗塞$)",replacement = "I21.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^成人斯蒂尔病$)",replacement = "M06.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^扁桃体周围蜂窝组织炎$)",replacement = "J36.x01")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^淋巴管瘤$)",replacement = "D18.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^淋巴结炎$)",replacement = "I88.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^溺水$)",replacement = "T75.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^甲状腺癌术后$)",replacement = "C73.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^甲状腺肿$)",replacement = "D44.001")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^痔$)",replacement = "I84.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^白细胞异常$)",replacement = "R72.x00")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^糖尿病性酮症$)",replacement = "E10.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肌腱囊肿$)",replacement = "M67.801")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肝癌术后复发$)",replacement = "C22.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺大泡破裂$)",replacement = "J43.902")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肺栓塞$)",replacement = "I26.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾病综合征，膜性肾病$)",replacement = "N04.200")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^肾积水伴肾结石(术后)$)",replacement = "N13.201")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃窦腺癌$)",replacement = "C16.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃腺癌$)",replacement = "C16.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^胃角恶性肿瘤$)",replacement = "C16.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^脑室出血$)",replacement = "I61.500")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^贲门癌术后$)",replacement = "C16.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^贲门腺癌$)",replacement = "C16.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^酒精性股骨头坏死$)",replacement = "M87.301")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^重度子痫前期$)",replacement = "O14.102")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^陈旧性颧弓颧骨骨折$)",replacement = "T90.203")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^非恶性神经纤维瘤病$)",replacement = "Q85.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颅骨骨折$)",replacement = "S02.900")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颈部淋巴结转移癌$)",replacement = "C77.002")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^颈部转移癌$)",replacement = "C79.834")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^食管癌化疗后$)",replacement = "D00.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^高热惊厥$)",replacement = "R56.001")}
temp  <- unlist(lapply(temp,b))
##################################################
a_na_icd$主要编码[which(!temp==a_na_icd$dig)] <- temp[which(!temp==a_na_icd$dig)]
a_na_icd2 <- a_na_icd[-c(which(is.na(a_na_icd$主要编码))),]
names(a_na_icd2)[54] <- "ICD10"
sel_features[51] <- "ICD10"
a_na_icd2 <- as.data.frame(a_na_icd2)
##############################################
p <- function(x){str_sub(x,1,3)}
fir3_ICD <- unlist(lapply(a_na_icd2$ICD10,p))
p <- function(x){str_sub(x,1,1)}
fir1_ICD <- unlist(lapply(a_na_icd2$ICD10,p))
p <- function(x){str_sub(x,2,3)}
num_ICD <- unlist(lapply(a_na_icd2$ICD10,p))
#####
a_na_icd2$"fir3" <- fir3_ICD
a_na_icd2$"fir1" <- fir1_ICD
a_na_icd2 <- as.data.frame(a_na_icd2)
tt <- ncol(a_na_icd2)+1
a_na_icd2[which(a_na_icd2$fir1=="A"|a_na_icd2$fir1=="B"),tt] <- 1
a_na_icd2[which(a_na_icd2$fir1=="C"),tt] <- 2
a_na_icd2$"ICDnum" <- num_ICD 
a_na_icd2[which(a_na_icd2$fir1=="D"&a_na_icd2$ICDnum>=0&a_na_icd2$ICDnum<=48),tt] <- 2
a_na_icd2[which(a_na_icd2$fir1=="D"&a_na_icd2$ICDnum>=50&a_na_icd2$ICDnum<=89),tt] <- 3
a_na_icd2[which(a_na_icd2$fir1=="E"),tt] <- 4
a_na_icd2[which(a_na_icd2$fir1=="F"),tt] <- 5
a_na_icd2[which(a_na_icd2$fir1=="G"),tt] <- 6
a_na_icd2[which(a_na_icd2$fir1=="H"&a_na_icd2$ICDnum>=00&a_na_icd2$ICDnum<=59),tt] <- 7
a_na_icd2[which(a_na_icd2$fir1=="H"&a_na_icd2$ICDnum>=60&a_na_icd2$ICDnum<=95),tt] <- 8
a_na_icd2[which(a_na_icd2$fir1=="I"),tt] <- 9
a_na_icd2[which(a_na_icd2$fir1=="J"),tt] <- 10
a_na_icd2[which(a_na_icd2$fir1=="K"),tt] <- 11
a_na_icd2[which(a_na_icd2$fir1=="L"),tt] <- 12
a_na_icd2[which(a_na_icd2$fir1=="M"),tt] <- 13
a_na_icd2[which(a_na_icd2$fir1=="N"),tt] <- 14
a_na_icd2[which(a_na_icd2$fir1=="O"),tt] <- 15
a_na_icd2[which(a_na_icd2$fir1=="P"),tt] <- 16
a_na_icd2[which(a_na_icd2$fir1=="Q"),tt] <- 17
a_na_icd2[which(a_na_icd2$fir1=="R"),tt] <- 18
a_na_icd2[which(a_na_icd2$fir1=="S"|a_na_icd2$fir1=="T"),tt] <- 19
a_na_icd2[which(a_na_icd2$fir1=="Z"),tt] <- 21
names(a_na_icd2)[57] <- "Chap"
####
a_na_icd2 <-na.omit(a_na_icd2)
#################
index <- which(a_na_icd2$Chap==5)
a_na_icd2$dig <- 'N'
a_na_icd2[index,'dig'] <- 'P'
a_na_icd2 <- a_na_icd2[,c(3:51,2,52:53,1,57)]


# positive set selection / p1 pa pb pc---------------------------------------------------
p_na <- as.data.frame(rbind(p1,pa,pb,pc))
p_na[,2:50] <- as.data.frame(apply(p_na[,2:50], 2, as.numeric))
p_na[is.na(p_na)] <- 0

p_na$sumNA <- rep(1,nrow(p_na))

for(i in 1:nrow(p_na)){
  p_na[i,'sumNA'] <- length(which(p_na[i,2:50]==0))
}
table(p_na$sumNA)

index_h <- which(p_na$sumNA>=1&p_na$sumNA<=3)
p_na <- p_na[index_h,]


table(p_na$sumNA)
p_na <- p_na[,-54]

# positive set ICD normalization -------------------------------------------
############################################################################
p_na_icd <-merge(p_na,ICD_code[,c(1,3)],by = "dig",all.x = T)
b <- function(P){str_replace_all(string = P,pattern = "(^抑郁症$)",replacement = "F32.901")}
temp  <- unlist(lapply(p_na_icd$dig,b))
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

p_na_icd$主要编码[which(!temp==p_na_icd$dig)] <- temp[which(!temp==p_na_icd$dig)]
names(p_na_icd)[54] <- "ICD10"
p <- function(x){str_sub(x,1,3)}
fir3_ICD_p <- unlist(lapply(p_na_icd$ICD10,p))
p <- function(x){str_sub(x,1,1)}
fir1_ICD_p <- unlist(lapply(p_na_icd$ICD10,p))
p <- function(x){str_sub(x,2,3)}
num_ICD_p <- unlist(lapply(p_na_icd$ICD10,p))
p_na_icd$num<- as.numeric(num_ICD_p)

p_na_icd2 <-p_na_icd
p_na_icd2$icdF <- p_na_icd$num
p_na_icd2$icdF[which(p_na_icd2$num<=9)] <- "p1"
p_na_icd2$icdF[which(p_na_icd2$num>=10&p_na_icd2$num<=19)] <- "p2"
p_na_icd2$icdF[which(p_na_icd2$num>=20&p_na_icd2$num<=29)] <- "p3"
p_na_icd2$icdF[which(p_na_icd2$num>=30&p_na_icd2$num<=39)] <- "p4"
p_na_icd2$icdF[which(p_na_icd2$num>=40&p_na_icd2$num<=49)] <- "p5"
p_na_icd2$icdF[which(p_na_icd2$num>=50&p_na_icd2$num<=59)] <- "p6"
p_na_icd2$icdF[which(p_na_icd2$num>=60&p_na_icd2$num<=69)] <- "p7"
p_na_icd2$icdF[which(p_na_icd2$num>=70&p_na_icd2$num<=79)] <- "p8"
p_na_icd2$icdF[which(p_na_icd2$num>=80&p_na_icd2$num<=89)] <- "p9"
p_na_icd2$icdF[which(p_na_icd2$num>=90&p_na_icd2$num<=98)] <- "p10"
p_na_icd2$icdF[which(p_na_icd2$num==99)] <- "p11"
p_na_icd2$icdF[which(is.na(p_na_icd2$num))] <- "p_other"#others
p_na_icd2<-p_na_icd2[,c(2:53,1,56)]
p_na_icd2$dig<- "P"
names(p_na_icd2)[ncol(p_na_icd2)] <- "Chap"
p_na_icd2 <- p_na_icd2[,c(2:50,1,51:53,54)]

# Integrate data -------------------------------------------
############################################################################

Valida_na_tot <- rbind(h_na, a_na_icd2, p_na_icd2, FEP_na)
Valida_na_tot[,1:49] <- as.data.frame(apply(Valida_na_tot[,1:49],2,as.numeric))
Valida_na_tot <- as.data.frame(Valida_na_tot)
Valida_na_tot$ID <- as.numeric(Valida_na_tot$ID)

ind_dup_na <- which(Valida_na_tot$ID%in%data$ID|Valida_na_tot$ID%in%FEP_data$ID|Valida_na_tot$ID%in%Valida_out_tot$ID)

Valida_na_tot <- Valida_na_tot[-ind_dup_na,]
ind_dup <- which(duplicated(Valida_na_tot$ID))
Valida_na_tot <- Valida_na_tot[-ind_dup,]
Valida_na_tot[, 1:49][Valida_na_tot[, 1:49] == 0] <- NA

# specify sex & age
Valida_na_tot$Age <- as.numeric(Valida_na_tot$Age)

Valida_na_tot[!Valida_na_tot$Gender%in%c('男','M','女','F'),]$Gender <- NA
Valida_na_tot[Valida_na_tot$Gender%in%c('男','M'),]$Gender <- 1
Valida_na_tot[Valida_na_tot$Gender%in%c('女','F'),]$Gender <- 0
Valida_na_tot$Gender <- as.numeric(Valida_na_tot$Gender)

Valida_na_unprep <- Valida_na_tot
