library(dplyr)
d1 <- read.csv2("data/tests_price.csv")
#d2 <- read.csv2("data/tests_per_case.csv")
d2 <- read.csv2("data/tests_per_case_v2.csv")
d3 <- read.csv2("data/currency conversion.csv")

d3$conversion <- as.numeric(as.character(d3$conversion))
# .l <- lapply(paste0("data/",list.files("data")), read.csv2)
# for(x in 1:length(.l)){
#   assign(paste0("d",x),.l[[x]])
# } 


head(d1)
d1$cameroon_euro <- d1$cameroon_XAF * d3$conversion[d3$currency=="XAF"]
d1$CI_euro <- d1$CI_CFA  * d3$conversion[d3$currency=="CFA"]
d1$china_euro <- d1$china_RMB * d3$conversion[d3$currency=="RMB"]
d1$srilanka_euro <- d1$srilanka_SLR * d3$conversion[d3$currency=="SLR"]
d1$vietnam_euro <- d1$vietnam_VND * d3$conversion[d3$currency=="VND"]

head(d1)
d1 <- d1[ , c("Sample", "Test", "CI_euro", "cameroon_euro", "china_euro", "srilanka_euro", "vietnam_euro")]


#Merge with list of exams
d <-merge(d2, d1,  by = c("Sample", "Test"), all.x=TRUE, all.y=FALSE) 
d <- d[order(d$Case, d$Sample), ]

#price for all cases
# head(d)
# #min
# .l <- lapply(unique(d$Case), function(.case) apply(d[d$basic_extra == "basic" & d$Case == .case, grep("euro", names(d))], 2, sum, na.rm=T))
# min_pc <- data.frame(Case = unique(d$Case), round(do.call(rbind, .l),0))                 
# #max
# .l <- lapply(unique(d$Case), function(.case) apply(d[d$Case == .case, grep("euro", names(d))], 2, sum, na.rm=T))
# max_pc <- data.frame(Case = unique(d$Case), round(do.call(rbind, .l),0))                 
# 
# #merge min and max
# .l <- lapply(1: nrow(min_pc), function(i){
#   .d <- min_pc[i,] == max_pc[i,]
#   .vec <- as.character(unlist(ifelse(.d==TRUE, min_pc[i,], paste(min_pc[i,], max_pc[i,], sep="-"))))
# return(.vec)
# })
# .l <- data.frame(do.call(rbind, .l)) ; colnames(.l) <- colnames(min_pc)
# .l <- .l[-1]
# all_price <- data.frame(Case = unique(d$Case), .l)
# 
# write.table(print(all_price), file = "clipboard", sep="\t", row.names = F)

# level1
.list <- lapply(unique(d$level),function(.lev){
  .l <- lapply(unique(d$Case), function(.case) apply(d[d$level <= .lev & d$Case == .case, grep("euro", names(d))], 2, sum, na.rm=T))
  .df <- data.frame(Case = unique(d$Case), until_level = .lev, round(do.call(rbind, .l),0))
  assign(paste0("pc_", .lev), .df)
  return(.df)
})
dd <- do.call(rbind,.list)

#pour cote d'ivoire, pas d'analyse du liquide amniotique => le moins cher est en fait le niveau 2  
dd[dd$Case=="F", "CI_euro"][1:3] <- dd[dd$Case=="F", "CI_euro"][c(2:4)]

#max par cas

.l <- lapply(unique(d$Case), function(.case) {
  dd[dd$Case==.case & dd$until_level==4, ]
})
l <- do.call(rbind, .l)
apply(l[,grep("euro", names(l))],2, function(x)max(x)*24)
# 2 cases per month per site 
# 12 000 euros a year per site is the maximum

#level 4 is caryotype : price if 40% infectious
inf <- apply(dd[dd$until_level==3, grep("euro", names(l))], 2, function(x)max(x))
krom <- apply(dd[dd$until_level==4, grep("euro", names(l))], 2, function(x)max(x))
(inf*0.4 + krom*0.6)*24
