############
# scenarii #
############

library(dplyr)
library(stringr)
library(tidyr)
source ("src/all_scenarii.R")

myrow.df <- read.csv2("data/sample match.csv") %>% mutate_all(as.character)


#=======================
#Functions

repl.f <- function(x) ifelse(is.na(x), "NA",x)
repl.f2 <- function(x) ifelse(is.na(x), "",x)

#df <- LB_sc4_i 
get_tab_case <- function(df, .mycountry) {
  df <- df %>% mutate_at(vars(Sample, Test), funs(as.character)) 
  col_price <- names(df)[grep("_[A-Z]{3}", names(df))]
  for (name_col in col_price){
    a <- name_col
    curr <- regmatches(x=a, m=regexpr(pattern="[A-Z]{3}\\b", text = a))
    country <- regmatches(x=a, m=regexpr(pattern="\\b[A-Za-z]+", text = a))
    p <- d3[d3$currency==curr, "conversion"]
    df[,paste0(country, "_EUR")] <- df[ ,a]*p
  }
  df2 <- df %>% mutate_all(funs(repl.f)) %>% 
    right_join(myrow.df) %>% 
    select(Sample, Test, contains(.mycountry, ignore.case = TRUE)) %>% 
    select(contains("EUR")) %>% 
    mutate_all(funs(repl.f2))
  return(df2)
}

#=======================
#calculation per case

mycase <- F_sc1_af_i
mycountry <- "srilanka"
mycountry <- "vietnam"
mycountry <- "cameroon"
mycountry <- "ci"
mycountry <- "china"

.mycountry <- "srilanka"

#------------
#Live born
#------------
#----------
#scenario 4 : infection zika
LB_sc4_i 
#----------
#scenario 2 : infection CMV
LB_sc2_i
#----------
#scenario 5 : infection rubella
LB_sc5_i
#----------
#scenario 3 : infection toxo
LB_sc3_i 
#----------
#scenario 1 : pas infectieux
LB_sc1_i

#tab for LB
.l <- lapply(paste0("LB_sc",c(4, 2, 5, 3, 1),"_i"), function(x) {
  tmp <- get(x)
  get_tab_case(tmp, mycountry)
})
a <- do.call(cbind,.l)

write.table(print(a), file="clipboard", sep="\t", row.names = FALSE, col.names = FALSE)


#------------
#Still born
#------------

#----------
#scenario 3 : infection Zika
SB_sc3_i 
#----------
#scenario 4 : infection CMV
SB_sc4_i 
#----------
#scenario 5 : infection Rubella
SB_sc5_i 
#----------
#scenario 6 : infection Toxo
SB_sc6_i 
#----------
#scenario 1 : pas infectieux
SB_sc1_i

#tab for SB
.l <- lapply(paste0("SB_sc",c(3, 4, 5, 6, 1),"_i"), function(x) {
  tmp <- get(x)
  get_tab_case(tmp, mycountry)
})
a <- do.call(cbind,.l)

write.table(print(a), file="clipboard", sep="\t", row.names = FALSE, col.names = FALSE)


#------------
#Fetus
#------------

#----------
#scenario 4 : infection retrouvée dans le liquide amniotique
#fetus sample
F_sc4_af_i
#----------
#scenario 3 : infection Zika et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc3_af_i
#no amniotic fluid et fetus sample
F_sc3_naf_i
#----------
#scenario 6 : infection CMV et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc6_af_i
#no amniotic fluid et fetus sample
F_sc6_naf_i
#----------
#scenario 7 : infection Rubella et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc7_af_i
#no amniotic fluid et fetus sample
F_sc7_naf_i 
#----------
#scenario 8 : infection toxo et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc8_af_i
#no amniotic fluid et fetus sample
F_sc8_naf_i 
#----------
#scenario 1 : genetic or unknown
F_sc1_af_i
#no amniotic fluid et fetus sample
F_sc1_naf_i 

#a <- get_tab_case(mycase, mycountry)

#tab for fetus without AF
.l <- lapply(paste0("F_sc",c(3, 6, 7, 8,1),"_naf_i"), function(x) {
  tmp <- get(x)
  get_tab_case(tmp, mycountry)
})
a <- do.call(cbind,.l)
write.table(print(a), file="clipboard", sep="\t", row.names = FALSE, col.names = FALSE)

#tab for fetus with AF
.l <- lapply(paste0("F_sc",c(4, 3, 6, 7, 8,1),"_af_i"), function(x) {
tmp <- get(x)
get_tab_case(tmp, mycountry)
})
a <- do.call(cbind,.l)
write.table(print(a), file="clipboard", sep="\t", row.names = FALSE, col.names = FALSE)


