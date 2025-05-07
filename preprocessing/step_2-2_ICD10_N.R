aa_2 <-merge(aa_1,ICD_code[1:21640,c(1,3)],by = "dig",all.x = T)

list <- as.data.frame(table(aa_2$dig[which(is.na(aa_2$主要编码))]))
list <- list[order(list[,2],decreasing = T),]
b <- function(P){str_replace_all(string = P,pattern = "(^孕[0-9]{1,3}周宫内妊娠$)",replacement = "Z32.100")}
temp  <- unlist(lapply(aa_2$dig,b))
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
b <- function(P){str_replace_all(string = P,pattern = "(^乙型肝炎.*$)",replacement = "B18.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^乙肝.*$)",replacement = "B18.100")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*恶性肿瘤.*$)",replacement = "C80.000")}
temp  <- unlist(lapply(temp,b))
b <- function(P){str_replace_all(string = P,pattern = "(^.*恶性.*瘤.*$)",replacement = "C80.000")}
temp  <- unlist(lapply(temp,b))

##################################################
aa_2$主要编码[which(!temp==aa_2$dig)] <- temp[which(!temp==aa_2$dig)]
aa_3 <- aa_2[-c(which(is.na(aa_2$主要编码))),]
names(aa_3)[54] <- "ICD10"
sel_features[51] <- "ICD10"
aa_4 <- as.data.frame(aa_3)
##############################################
p <- function(x){str_sub(x,1,3)}
fir3_ICD <- unlist(lapply(aa_4$ICD10,p))
p <- function(x){str_sub(x,1,1)}
fir1_ICD <- unlist(lapply(aa_4$ICD10,p))
p <- function(x){str_sub(x,2,3)}
num_ICD <- unlist(lapply(aa_4$ICD10,p))
##### chapter
aa_4$"fir3" <- fir3_ICD
aa_4$"fir1" <- fir1_ICD
aa_4 <- as.data.frame(aa_4)
tt <- ncol(aa_4)+1
aa_4[which(aa_4$fir1=="A"|aa_4$fir1=="B"),tt] <- 1
aa_4[which(aa_4$fir1=="C"),tt] <- 2
aa_4$"ICDnum" <- num_ICD 
aa_4[which(aa_4$fir1=="D"&aa_4$ICDnum>=0&aa_4$ICDnum<=48),tt] <- 2
aa_4[which(aa_4$fir1=="D"&aa_4$ICDnum>=50&aa_4$ICDnum<=89),tt] <- 3
aa_4[which(aa_4$fir1=="E"),tt] <- 4
aa_4[which(aa_4$fir1=="F"),tt] <- 5
aa_4[which(aa_4$fir1=="G"),tt] <- 6
aa_4[which(aa_4$fir1=="H"&aa_4$ICDnum>=00&aa_4$ICDnum<=59),tt] <- 7
aa_4[which(aa_4$fir1=="H"&aa_4$ICDnum>=60&aa_4$ICDnum<=95),tt] <- 8
aa_4[which(aa_4$fir1=="I"),tt] <- 9
aa_4[which(aa_4$fir1=="J"),tt] <- 10
aa_4[which(aa_4$fir1=="K"),tt] <- 11
aa_4[which(aa_4$fir1=="L"),tt] <- 12
aa_4[which(aa_4$fir1=="M"),tt] <- 13
aa_4[which(aa_4$fir1=="N"),tt] <- 14
aa_4[which(aa_4$fir1=="O"),tt] <- 15
aa_4[which(aa_4$fir1=="P"),tt] <- 16
aa_4[which(aa_4$fir1=="Q"),tt] <- 17
aa_4[which(aa_4$fir1=="R"),tt] <- 18
aa_4[which(aa_4$fir1=="S"|aa_4$fir1=="T"),tt] <- 19
aa_4[which(aa_4$fir1=="Z"),tt] <- 21
names(aa_4)[57] <- "Chap"
####
data_H <-na.omit(aa_4)
################# rm Chapter IV-VI
index <- which(data_H$Chap==4|data_H$Chap==5|data_H$Chap==6)
data_H_CNN <- data_H[-index,]
data_H_CNN <- data_H_CNN[,c(2:58,1)]
data_H_CNN[,1:51] <- lapply(data_H_CNN[,c(1:51,53:57)],as.numeric)
data_H_CNN <- na.omit(data_H_CNN)
data_H_CNN <- subset(data_H_CNN, !duplicated(data_H_CNN,select = "ID"))
data_H_CNN <- data_H_CNN[-c(which(data_H_CNN$Age>90)),]

data_H_CNN <- data_H_CNN[,c(1:52,58,56)]
