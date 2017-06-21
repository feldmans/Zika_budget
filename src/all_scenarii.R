############
# scenarii #
############

library(dplyr)
library(stringr)
library(tidyr)
#d <- read.csv2("data/tests_price2.csv")
#d <- read.csv2("data/tests_price_noNA.csv")
d <- read.csv2("data/tests_price_noNA2.csv")

d3 <- read.csv2("data/currency conversion.csv")
d3$conversion <- as.numeric(as.character(d3$conversion))

# prix max PCR

max <- d %>% filter (Sample =="Placenta" & Test %in% c("CMV PCR", "Rubella RT-PCR", "Toxo PCR")) %>% select(-Sample) %>% select(-Test) %>%
  summarise_all(funs(max(.,na.rm=T)))
max <- cbind(Sample = "Placenta", Test = "max_PCR_noZika", max)

d <- rbind(d, max)

max <- d %>% filter (Sample =="Placenta" & Test %in% c("CMV PCR", "Rubella RT-PCR")) %>% select(-Sample) %>% select(-Test) %>%
  summarise_all(funs(max(.,na.rm=T)))
max <- cbind(Sample = "Placenta", Test = "max_PCR_CMVRubella", max)

d <- rbind(d, max)


#---------------------------
# Fetus
#---------------------------

#----------
#scenario 1 : pas infectieux
#amniotic fluid et fetus sample
F_sc1_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             Sample == "AF" |
                             #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             Test =="Karyotype")
#amniotic fluid no fetus sample
F_sc1_af_ni <- d %>% filter(Sample == "AF" |
                             #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             Test =="Karyotype")
#no amniotic fluid et fetus sample
F_sc1_naf_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             Test =="Karyotype")
#no amniotic fluid no fetus sample
#F_sc1_naf_ni <- d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
F_sc1_naf_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             Test =="Karyotype")

#----------
#scenario 2 : infection (mais pas Zika) et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc2_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             Sample == "AF" |
                             #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="max_PCR_noZika"))
#amniotic fluid no fetus sample
F_sc2_af_ni <- d %>% filter(Sample == "AF" |
                              #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="max_PCR_noZika"))
#no amniotic fluid et fetus sample
F_sc2_naf_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                              #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="max_PCR_noZika"))
#no amniotic fluid no fetus sample
#F_sc2_naf_ni <- d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
F_sc2_naf_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                               (Sample == "Placenta" & Test =="max_PCR_noZika"))

#----------
#scenario 3 : infection Zika et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc3_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             Sample == "AF" |
                             #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
                             (Sample =="VB") | #select all IgM including DENV IgG zika and PCR ZIKA on VB
                             #((Sample == "Placenta" | Sample == "CB_VB") & Test =="ZIKV RT-PCR"))
                             (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
#amniotic fluid no fetus sample
F_sc3_af_ni <- d %>% filter(Sample == "AF" |
                              # (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
                              (Sample =="VB") | #select all IgM including DENV IgG zika and PCR ZIKA on VB
                              (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
                              
#no amniotic fluid et fetus sample
F_sc3_naf_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                              #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
                              (Sample =="VB") | #select all IgM including DENV IgG zika and PCR ZIKA on VB
                              (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
#no amniotic fluid no fetus sample
#F_sc3_naf_ni <- d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
F_sc3_naf_ni <- d %>% filter((Sample =="VB") | #select all IgM including DENV IgG zika and PCR ZIKA on VB
                               (Sample == "Placenta" & Test =="ZIKV RT-PCR"))

#----------
#scenario 4 : infection retrouvée dans le liquide amniotique
#fetus sample
F_sc4_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                          Sample == "AF" )
#no fetus sample
F_sc4_af_ni <- d %>% filter(Sample == "AF" )

#----------
#scenario 6 : infection CMV et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc6_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             Sample == "AF" |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="CMV PCR"))
#amniotic fluid no fetus sample
F_sc6_af_ni <- d %>% filter(Sample == "AF" |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="CMV PCR"))
#no amniotic fluid et fetus sample
F_sc6_naf_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="CMV PCR"))
#no amniotic fluid no fetus sample
#F_sc2_naf_ni <- d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
F_sc6_naf_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                               (Sample == "Placenta" & Test =="CMV PCR"))

#----------
#scenario 7 : infection Rubella et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc7_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             Sample == "AF" |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="Rubella RT-PCR"))
#amniotic fluid no fetus sample
F_sc7_af_ni <- d %>% filter(Sample == "AF" |
                              #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="Rubella RT-PCR"))
#no amniotic fluid et fetus sample
F_sc7_naf_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                              #(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="Rubella RT-PCR"))
#no amniotic fluid no fetus sample
#F_sc2_naf_ni <- d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
F_sc7_naf_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                               (Sample == "Placenta" & Test =="Rubella RT-PCR"))

#----------
#scenario 8 : infection toxo et non retrouvée dans AF
#amniotic fluid et fetus sample
F_sc8_af_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                             Sample == "AF" |
                             (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="Toxo PCR"))
#no amniotic fluid et fetus sample
F_sc8_naf_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                              (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                              (Sample == "Placenta" & Test =="Toxo PCR"))

#no amniotic fluid no fetus sample
F_sc8_naf_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                               (Sample == "Placenta" & Test =="Toxo PCR"))


#---------------------------
# Still born
#---------------------------

#----------
#scenario 1 : pas infectieux
SB_sc1_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
                           (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                           Test =="Karyotype")
SB_sc1_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            Test =="Karyotype")
# SB_sc1_i <- d %>% filter(Sample == "Tissue" | Sample == "OS" |
#                            (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                            Test =="Karyotype")
# SB_sc1_ni <- d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                             Test =="Karyotype")
#----------
#scenario 2 : infection (mais pas zika)
SB_sc2_i <-  d %>% filter(Sample == "Tissue" | Sample == "OS" |
                            (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            (Sample == "Placenta" & Test =="max_PCR_noZika"))
SB_sc2_ni <-  d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="max_PCR_noZika"))
#----------
#scenario 3 : infection Zika
SB_sc3_i <-  d %>% filter(Sample == "Tissue" | Sample == "OS" |
                            (Sample =="VB") |
                            (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
SB_sc3_ni <-  d %>% filter((Sample =="VB") |
                            (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
# SB_sc3_i <-  d %>% filter(Sample == "Tissue" | Sample == "OS" |
#                             (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
#                             (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
# SB_sc3_ni <-  d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
#                              (Sample == "Placenta" & Test =="ZIKV RT-PCR"))
#----------
#scenario 4 : infection CMV
SB_sc4_i <-  d %>% filter(Sample == "Tissue" | Sample == "OS" |
                            (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            (Sample == "Placenta" & Test =="CMV PCR"))
SB_sc4_ni <-  d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="CMV PCR"))
#----------
#scenario 5 : infection Rubella
SB_sc5_i <-  d %>% filter(Sample == "Tissue" | Sample == "OS" |
                            (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            (Sample == "Placenta" & Test =="Rubella RT-PCR"))
SB_sc5_ni <-  d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="Rubella RT-PCR"))

#----------
#scenario 6 : infection Toxo
SB_sc6_i <-  d %>% filter(Sample == "Tissue" | Sample == "OS" |
                            (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            (Sample == "Placenta" & Test =="Toxo PCR"))
SB_sc6_ni <-  d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample == "Placenta" & Test =="Toxo PCR"))


#---------------------------
# Live born
#---------------------------

#----------
#scenario 1 : pas infectieux
LB_sc1_i <- d %>% filter(Sample == "OS" |
                           ((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                           Test =="Karyotype")
LB_sc1_ni <- d %>% filter(((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            Test =="Karyotype")
#----------
#scenario 2 : infection CMV
LB_sc2_i <- d %>% filter(Sample == "OS" |
                           ((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                           ((Sample == "Placenta"|Sample == "Urine") & Test =="CMV PCR"))
LB_sc2_ni <- d %>% filter(((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            ((Sample == "Placenta"|Sample == "Urine") & Test =="CMV PCR"))
#----------
#scenario 3 : infection toxo
LB_sc3_i <- d %>% filter(Sample == "OS" |
                           ((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                           (Sample == "Placenta" & Test =="Toxo PCR"))
LB_sc3_ni <- d %>% filter(((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            (Sample == "Placenta" & Test =="Toxo PCR"))
#----------
#scenario 4 : infection zika
LB_sc4_i <- d %>% filter(Sample == "OS" |
                          (Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
                          ((Sample =="CB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                          (Sample =="Placenta" & Test =="ZIKV RT-PCR")))
LB_sc4_ni <- d %>% filter((Sample =="VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
                          ((Sample =="CB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                             (Sample =="Placenta" & Test =="ZIKV RT-PCR")))
#----------
#scenario 5 : infection rubella
LB_sc5_i <- d %>% filter(Sample == "OS" |
                          ((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                          ((Sample == "Placenta"|Sample == "Urine") & Test =="Rubella RT-PCR"))
LB_sc5_ni <- d %>% filter(((Sample =="CB" | Sample =="VB") & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
                            ((Sample == "Placenta"|Sample == "Urine") & Test =="Rubella RT-PCR"))
#----------
# #scenario 1 : pas infectieux
# LB_sc1_i <- bind_rows(d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM")),
#                       d %>% filter(Sample == "OS" |
#                            (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                            Test =="Karyotype"))
# LB_sc1_ni <- bind_rows(d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM")),
#                       d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                      Test =="Karyotype"))
# 
# #scenario 2 : infection CMV ou rubella
# LB_sc2_i <- bind_rows(d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                      Test =="max_PCR_CMVRubella"),
#                       d %>% filter(Sample == "OS" |
#                                      (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                      Test =="max_PCR_CMVRubella"))
# LB_sc2_ni <- bind_rows(d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                      Test =="max_PCR_CMVRubella"),
#                       d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                      Test =="max_PCR_CMVRubella"))
# 
# #scenario 3 : infection toxo
# LB_sc3_i <- bind_rows(d %>% filter(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM"),
#                       d %>% filter(Sample == "OS" |
#                                      (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                      (Sample =="Placenta" & Test =="Toxo PCR")))
# LB_sc3_ni <- bind_rows(d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM")),
#                        d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM") |
#                                       (Sample =="Placenta" & Test =="Toxo PCR")))
# 
# #scenario 4 : infection zika
# LB_sc4_i <- bind_rows(d %>% filter(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM"),
#                       d %>% filter(Sample == "OS" |
#                                      (Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
#                                      (Sample =="Placenta" & Test =="ZIKV RT-PCR")))
# LB_sc4_ni <- bind_rows(d %>% filter(Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)] & Test != "DENV IgM"),
#                       d %>% filter((Sample =="CB_VB" & Test %in% d$Test[grep("Ig", d$Test)]) |
#                                      (Sample =="Placenta" & Test =="ZIKV RT-PCR")))







#récupérer tous les objets
.l <- lapply(ls()[grep(".+_n{0,1}i",ls())], function(x){#......._ni ou _i
  tmp <- get(x)
  tmp$case <- regmatches(x=x, m=regexpr(pattern="\\b[A-Z]{1,2}", text = x))
  tmp$sc <- x
  return(tmp)
})
all_sc <- do.call(rbind,.l)

col_price <- names(all_sc)[grep("_[A-Z]{3}", names(all_sc))]

all <- all_sc %>% group_by(sc) %>% select(one_of(col_price)) %>%
  summarise_all(funs(sum))
all_noNA <- all_sc %>% group_by(sc) %>% select(one_of(col_price)) %>%
  summarise_all(funs(sum(., na.rm=TRUE)))

#conversion en euro
for (name_col in col_price){
  a <- name_col
  curr <- regmatches(x=a, m=regexpr(pattern="[A-Z]{3}\\b", text = a))
  country <- regmatches(x=a, m=regexpr(pattern="\\b[A-Za-z]+", text = a))
  p <- d3[d3$currency==curr, "conversion"]
  all_noNA[,paste0(country, "_EUR")] <- all_noNA[ ,a]*p
  all[,paste0(country, "_EUR")] <- all[ ,a]*p
}

View(all_noNA)
View(all)


#-------------------------------
#Cost per country
#-------------------------------

#cameroon

all %>% 
  filter(grepl("ni", sc) & !grepl("_af", sc) & !grepl("F_sc4", sc)) %>%
  select(sc, price = cameroon_EUR) %>% 
  extract(
    col = sc,
    into = c("case", "sc"),
    regex = "^([A-Z]{1,2})_sc([1-4]).*"
  ) %>%  
  mutate(inf = ifelse(sc==1, 1, 0)) %>% 
  group_by(case, inf) %>% 
  mutate(max = max(price)) %>% 
  ungroup %>% 
  filter(price==max) %>% 
  mutate(
    p_case = ifelse(case=="F" | case =="SB", 0.1, 0.8), 
    p_sc = ifelse(sc == 1, 0.75, 0.25),
    p_tot = p_case * p_sc,
    p_price = p_tot * price
  ) %>% 
  # to check that we have 100 %
  #summarise(n = sum(p_tot))
  summarise (sum(p_price)) %>% round(.,0) %>% 
  pull -> p_cam

#cote d'Ivoire
all %>% 
  filter(grepl("ni", sc) & !grepl("_af", sc) & !grepl("F_sc4", sc)) %>%
  select(sc, price = CI_EUR) %>% 
  extract(
    col = sc,
    into = c("case", "sc"),
    regex = "^([A-Z]{1,2})_sc([1-4]).*"
  ) %>%  
  mutate(inf = ifelse(sc==1, 1, 0)) %>% 
  group_by(case, inf) %>% 
  mutate(max = max(price)) %>% 
  ungroup %>% 
  filter(price==max) %>% 
  mutate(
    p_case = ifelse(case=="F" | case =="SB", 0.1, 0.8), 
    p_sc = ifelse(sc == 1, 0.75, 0.25),
    p_tot = p_case * p_sc,
    p_price = p_tot * price
  ) %>% 
  # to check that we have 100 %
  #summarise(n = sum(p_tot))
  summarise (sum(p_price)) %>% round(.,0) %>% 
  pull -> p_ci

#China
all_noNA %>% 
  filter(grepl("_i", sc)) %>%
  select(sc, price = china_EUR) %>% 
  extract(
    col = sc,
    into = c("case", "sc", "af"),
    regex = "^([A-Z]{1,2})_sc([1-4])_([naf]{0,}).*"
  ) %>%  
  mutate(inf = ifelse(sc==1, 1, 0)) %>% 
  group_by(case, inf, af) %>% 
  mutate(max = max(price)) %>% 
  ungroup %>% 
  filter(price==max) %>% 
  mutate(
    p_af = ifelse(case=="F", 0.5,1),
    p_case = ifelse(case == "F", 0.4, ifelse(case == "SB", 0.1, 0.5)), 
    p_sc = ifelse(sc == 1, 0.75, 0.25),
    p_tot = p_case * p_sc * p_af,
    p_price = p_tot * price
  ) %>% 
  # to check that we have 100 %
  # summarise(n = sum(p_tot))
  summarise (sum(p_price)) %>% round(.,0) %>% 
  pull -> p_ch

#vietnam
all_noNA %>% 
  filter(grepl("_i", sc)) %>%
  select(sc, price = vietnam_EUR) %>% 
  extract(
    col = sc,
    into = c("case", "sc", "af"),
    regex = "^([A-Z]{1,2})_sc([1-4])_([naf]{0,}).*"
  ) %>%  
  mutate(inf = ifelse(sc==1, 1, 0)) %>% 
  group_by(case, inf, af) %>% 
  mutate(max = max(price)) %>% 
  ungroup %>% 
  filter(price==max) %>% 
  mutate(
    p_af = ifelse(case=="F", 0.5,1),
    p_case = ifelse(case == "F", 0.4, ifelse(case == "SB", 0.1, 0.5)), 
    p_sc = ifelse(sc == 1, 0.75, 0.25),
    p_tot = p_case * p_sc * p_af,
    p_price = p_tot * price
  ) %>% 
  # to check that we have 100 %
  # summarise(n = sum(p_tot))
  summarise (sum(p_price)) %>% round(.,0) %>% 
  pull -> p_viet

#Sri lanka : idem que china mais changer proba de fetal SB et LB
all_noNA %>% 
  filter(grepl("_i", sc)) %>%
  select(sc, price = srilanka_EUR) %>% 
  extract(
    col = sc,
    into = c("case", "sc", "af"),
    regex = "^([A-Z]{1,2})_sc([1-4])_([naf]{0,}).*"
  ) %>%  
  mutate(inf = ifelse(sc == 1, 1, 0)) %>% 
  group_by(case, inf, af) %>% 
  mutate(max = max(price)) %>% 
  ungroup %>% 
  filter(price == max) %>% 
  mutate(
    p_af = ifelse(case=="F", 0.5,1),
    p_case = ifelse(case == "F", 0.25, ifelse(case == "SB", 0.1, 0.65)), 
    p_sc = ifelse(sc == 1, 0.75, 0.25),
    p_tot = p_case * p_sc * p_af,
    p_price = p_tot * price
  ) %>% 
  # to check that we have 100 %
  # summarise(n = sum(p_tot))
  summarise (sum(p_price)) %>% round(.,0) %>% 
  pull -> p_srilanka
