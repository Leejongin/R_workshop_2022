rm(list=ls())

library(tidyverse)
getwd()

#a <- sas7bdat::read.sas7bdat("./Rmd/data_nhis/nsc2_edu_bnc.sas7bdat")
# sas7bdat 패키지로는 건보자료가 잘 안열리는 경우가 많다. 
#a <- haven::read_sas("./Rmd/data_nhis/nsc2_edu_bnc.sas7bdat")

g1e <- haven::read_sas("./Rmd/data_nhis/nsc2_edu_g1e.sas7bdat")
#g1e 는 검진 데이터
head(g1e)
str(g1e)
## 보통 데이터 구조를 파악하기 위해서 head, tail, str 등을 사용하지만
## 이 데이터의 경우 변수이름이 알아보기 힘들다. 내가 만든 데이터라면 정리를
## 해둬야 하고, 남이 만든 데이터라면 코드북을 꼭 숙지해야 한다. 

# 2012년 검진만 
g1e_2012 = g1e %>% filter(EXMD_BZ_YYYY==2012)
str(g1e_2012)
length(g1e_2012) # 82: 변수가 너무 많다. 분석에 관심있는 변수만 고르자. 

my_data = g1e_2012 %>% select(RN_INDI, G1E_HGHT, G1E_WGHT, 
                              G1E_BMI, G1E_BP_SYS, G1E_BP_DIA,
                              G1E_HGB, G1E_FBS, G1E_TOT_CHOL,
                              G1E_LDL, G1E_CRTN, G1E_SGOT, G1E_SGPT,
                              G1E_GGT, G1E_GFR)
### (개인식별 못하도록 성별, 연령이 빠져 있음)

# chi-square test 
## 두 군별 이분형 변수(Y/N)에 따른 차이가 있는지 검정
my_data = my_data %>% mutate(overweight=ifelse(G1E_BMI>25,1,0))
my_data = my_data %>% mutate(obesity=ifelse(G1E_BMI>30,2,
                                            ifelse(G1E_BMI>25,1,0)))
my_data = my_data %>% mutate(IFG=ifelse(G1E_FBS>100,1,0))
my_data$overweight <- as.factor(my_data$overweight)
my_data$IFG <- as.factor(my_data$IFG)
table(my_data$overweight, my_data$IFG)
chisq.test(my_data$overweight, my_data$IFG)

# T-test
## 두 군 사이에 연속변수의 유의한 차이가 있는지 검정
ggplot(my_data) +
  aes(x = overweight, y = G1E_FBS) +
  geom_boxplot()

t.test(G1E_FBS ~ overweight, data=my_data)

# ANOVA

# moonBook 패키지
library(moonBook)
moonBook::mytable(overweight ~., data=my_data)
moonBook::mytable(overweight ~ IFG + G1E_HGB, data=my_data)

# tableone 패키지 
library(tableone)
CreateTableOne(data=my_data)
CreateTableOne(data=my_data, strata = "overweight")
