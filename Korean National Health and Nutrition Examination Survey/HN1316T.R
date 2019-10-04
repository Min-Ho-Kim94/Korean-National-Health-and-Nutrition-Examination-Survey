library(tidyverse)
require(foreign)
require(survey)
require(gmodels)
library(pROC)


KNHNES1315 <- read.spss("C:/Users/IT/Desktop/졸업논문 분석파일 - hypertesion에서 조금 더 나아간 것/Raw data/HN(13-15)_ALL(22948).sav", to.data.frame=T, use.value.labels=F)
KNHNES16 <- read.spss("C:/Users/IT/Desktop/졸업논문 분석파일 - hypertesion에서 조금 더 나아간 것/Raw data/HN16_ALL.sav", to.data.frame=T, use.value.labels=F)

KNHNES1315 <- read.spss("C:/Users/user/Desktop/181110민호 집 포스터 작업/HN(13-15)_ALL(22948).sav", to.data.frame=T, use.value.labels=F)
KNHNES16 <- read.spss("C:/Users/user/Desktop/181110민호 집 포스터 작업/HN16_ALL.sav", to.data.frame=T, use.value.labels=F)

KNHNES1315 <- KNHNES1315 %>% rename(SEX = sex)
KNHNES16 <- KNHNES16 %>% rename(SEX = sex)

######################################################################################################################
#                                                                                                                    #
#                                                       모형구축                                                     #
#                                                                                                                    # 
######################################################################################################################
#########################################################################
#                                                                       #
#                               변수선택                                #
#                                                                       # 
#########################################################################

HN1315 <- KNHNES1315 %>% filter(age>=20) %>% # 만 20세 이상
  select(
    psu, wt_itvex, kstrata,
    HE_HP, DI1_dg, DI1_pr, DI1_2, HE_HPdg,
    age, SEX, BS1_1, BS3_1, HE_HPfh1, HE_HPfh2, HE_HPfh3, HE_BMI, HE_DM, HE_DMdg, DE1_dg, DE1_pr, DE1_pt,
    ho_incm, edu, occp,
    
    # 추가 변수
    HE_sbp, HE_dbp, BD1_11, BE3_31, BE5_1, DI3_dg, DI2_dg, DI2_2, HE_HCHOL, HE_HTG, HE_HDL_st2,
    BP8
    #HE_HCHOL, HE_LHDL_st2, HE_HTG # dyslipidemia 보류
    # <A hypertension risk score for middle-aged and older adults> risk factors. 09/27
    # <Age-Dependent Association Between Sleep Duration and Hypertension in the Adult Korean Population>
  )

HN16 <- KNHNES16 %>% filter(age>=20) %>% # 만 20세 이상
  select(
    psu, wt_itvex, kstrata,
    HE_HP, DI1_dg, DI1_pr, DI1_2, HE_HPdg,
    age, SEX, BS1_1, BS3_1, HE_HPfh1, HE_HPfh2, HE_HPfh3, HE_BMI, HE_DM, HE_DMdg, DE1_dg, DE1_pr, DE1_pt,
    ho_incm, edu, occp,
    
    # 추가 변수
    HE_sbp, HE_dbp, BD1_11, BE3_31, BE5_1, DI3_dg, DI2_dg, DI2_2, HE_HCHOL, HE_HTG, HE_HDL_st2,
    Total_slp_wk,Total_slp_wd
  ) # <A hypertension risk score for middle-aged and older adults> risk factors. 09/27
# 기초수급, 소득, 교육, 직업, 음주력 비만유병유무 추가

# 수면 시간
HN1315$BP8[HN1315$BP8==88] <-NA
HN1315$BP8[HN1315$BP8==99] <-NA
HN16$Total_slp_wk[HN16$Total_slp_wk==8888] <-NA
HN16$Total_slp_wk[HN16$Total_slp_wk==9999] <-NA
HN16$Total_slp_wd[HN16$Total_slp_wk==8888] <-NA
HN16$Total_slp_wd[HN16$Total_slp_wk==9999] <-NA
HN16 <- HN16 %>% mutate(
  BP8 = (Total_slp_wk+Total_slp_wd)/2,
  BP8=BP8/60
  ) %>% select(-Total_slp_wk, -Total_slp_wd)


HN1316 <- rbind(HN1315,HN16)
HN1316T <- HN1316 %>% filter(is.na(BD1_11)==F)
HN1316NA <- HN1316 %>% filter(is.na(BD1_11)==T)
#########################################################################


#write.csv(HN1316NA, file="HN1316NA.csv", row.names=T)


#########################################################################
#                                                                       #
#                           변수빈도확인                                #
#                                                                       # 
#########################################################################
# 1차 변수
summary(HN1316T$wt_itvex)
with(HN1316T, table(HE_HP, useNA = "always"))
with(HN1316T, table(DI1_dg, useNA = "always"))
with(HN1316T, table(DI1_pr, useNA = "always"))
with(HN1316T, table(DI1_2, useNA = "always"))
with(HN1316T, table(HE_HPdg, useNA = "always"))
summary(HN1316T$age)
with(HN1316T, table(SEX, useNA = "always"))
with(HN1316T, table(BS1_1, useNA = "always"))
with(HN1316T, table(BS3_1, useNA = "always"))
with(HN1316T, table(BS3_1, BS1_1, useNA = "always")) # BS3_1=9 & BS1_1=9 -> NA
HN1316T$BS1_1[HN1316T$BS1_1==9] <-NA
HN1316T$BS3_1[HN1316T$BS3_1==9] <-NA
with(HN1316T, table(HE_DM, useNA = "always"))
with(HN1316T, table(HE_DMdg, useNA = "always"))
with(HN1316T, table(DE1_dg, useNA = "always"))
with(HN1316T, table(DE1_pr, useNA = "always"))
with(HN1316T, table(DE1_pt, useNA = "always"))
summary(HN1316T$HE_BMI)
with(HN1316T, table(HE_HPfh1, useNA = "always"))
with(HN1316T, table(HE_HPfh2, useNA = "always"))
with(HN1316T, table(HE_HPfh3, useNA = "always"))
with(HN1316T, table(HE_HPfh1, HE_HPfh2, useNA = "always"))

# 2차 변수
summary(HN1316T$HE_sbp)
summary(HN1316T$HE_dbp)
with(HN1316T, table(BD1_11, useNA = "always"))
with(HN1316T, table(BE3_31, useNA = "always"))
with(HN1316T, table(BE5_1, useNA = "always"))
with(HN1316T, table(DI3_dg, useNA = "always"))
summary(HN1316T$BP8)
hist(HN1316T$BP8)


with(HN1316T, table(DI2_dg, useNA = "always")) # 설문조사 이상지질혈증 유무
HN1316T$DI2_dg[HN1316T$DI2_dg==9] <-NA
with(HN1316T, table(DI2_2, useNA = "always")) # 설문조사 이상지질혈증 약복용
HN1316T$DI2_2[HN1316T$DI2_2==9] <-NA
with(HN1316T, table(HE_HCHOL, useNA = "always")) # 검진조사 고콜레스테롤혈증 유병유무
with(HN1316T, table(HE_HTG, useNA = "always")) # 검진조사 고중성지방혈증 유병유무
summary(HN1316T$HE_HDL_st2) # 검진조사 LDL-콜레스테롤 전환식



with(HN1316T, table(BD1_11,BE3_31, useNA = "always"))
with(HN1316T, table(BD1_11,BE5_1, useNA = "always"))
with(HN1316T, table(BE3_31,BE5_1, useNA = "always"))
with(HN1316T, table(BD1_11,BP8, useNA = "always"))
with(HN1316T, table(BD1_11,DI3_dg, useNA = "always"))
with(HN1316T, table(HE_HP,HE_dbp, useNA = "always"))
with(HN1316T, table(HE_HP,HE_sbp, useNA = "always"))

#write.csv(HN1316T, file="HN1316T.csv", row.names=T)



#########################################################################
#                                                                       #
#                                   코딩                                #
#                                                                       # 
#########################################################################

HN1316T <- HN1316T %>%
  mutate(wt_itvex = ifelse(is.na(wt_itvex)==T, mean(wt_itvex, na.rm=T), wt_itvex),
         HE_BMI = ifelse(is.na(HE_BMI)==T, mean(HE_BMI, na.rm=T), HE_BMI),
         HE_sbp = ifelse(is.na(HE_sbp)==T, mean(HE_sbp, na.rm=T), HE_sbp),
         HE_dbp = ifelse(is.na(HE_dbp)==T, mean(HE_dbp, na.rm=T), HE_dbp)
         )




HN1316T <- HN1316T %>% 
  mutate(
    Y_HTN = ifelse(HE_HP==3 | HE_sbp>=140 | HE_dbp>=90 | DI1_dg==1 | DI1_pr==1 
                   | DI1_2==1 | DI1_2==2 | DI1_2==3 | DI1_2==4 | HE_HPdg==1, 2,
                   ifelse(DI1_dg==9 & DI1_pr==9 & DI1_2==9, NA, 1)
                   ),
    FH_HTN = ifelse(HE_HPfh1==1 | HE_HPfh2==1 | HE_HPfh3 ==1, 2, 1),
    AGE = ifelse(age <= 44, 1, 
                 ifelse(age<=64,2, 3)
    ),
    SMOKE = ifelse(
      (BS1_1==1 & BS3_1==1) | (BS1_1==1 & BS3_1==2) | (BS1_1==2 & BS3_1==1) | (BS1_1==2 & BS3_1==2), 3, 
      ifelse(
         (BS1_1==2 & BS3_1==3) | (BS1_1==1 & BS3_1==3),2,
        ifelse((BS1_1==3 & BS3_1==8),1,
               NA)
      )
    ),
    BMI = ifelse(HE_BMI<25, 1, ifelse(HE_BMI>=25 & HE_BMI <30, 2, 3)),
    DM = ifelse(HE_DM==3 | HE_DMdg==1 | DE1_dg==1 | DE1_pr==1 | DE1_pt==1, 2, 
                ifelse(DE1_dg==9 & DE1_pr==9 & DE1_pt==9, NA, 1)
                )
  ) %>%
  mutate(
    SBP = ifelse(HE_sbp<110, 1,
                 ifelse(HE_sbp>=110 & HE_sbp<114, 2, 
                        ifelse(HE_sbp>=115 & HE_sbp<119,3,
                               ifelse(HE_sbp>=120 & HE_sbp<124,4,
                                      ifelse(HE_sbp>=125 & HE_sbp<129,5,
                                             ifelse(HE_sbp>=130 & HE_sbp<134,6,
                                                    ifelse(HE_sbp>=135 & HE_sbp<139, 7, 8)
                                             )
                                      )
                               )
                        )
                 )
    ),
    DBP = ifelse(HE_dbp<70,1,ifelse(HE_dbp>=70 & HE_dbp<79,2,3)),
    ALCOHOL = ifelse(BD1_11==8, 1, ifelse(BD1_11>=1 & BD1_11<=6, 2, NA)), 
    EXERCISE = ifelse((BE3_31==1 & BE5_1==1) | (BE3_31==99 & BE5_1==1) | (BE3_31==1 & BE5_1==9), 1, 
                      ifelse((BE3_31>=2 & BE3_31<=8)|(BE5_1>=2 & BE5_1 <=6), 2, NA)
    ), # 걷기 또는 근력 운동 전혀 안함=1
    STROKE = ifelse(DI3_dg==0,1,ifelse(DI3_dg==1,2,NA)),
    DYSLIPIDEMIA = ifelse(DI2_dg==1 | DI2_2<=4 | HE_HCHOL==1 | HE_HTG==1 |HE_HDL_st2 <=40, 2, 
                          ifelse(DI2_dg==0 & (DI2_2==5 | DI2_2==8) & HE_HCHOL==0 & HE_HTG==0 & HE_HDL_st2 >=40, 1,
                                 NA)
                          )
  )



#write.csv(HN1316T, file="HN1316T.csv", row.names=T)
#########################################################################
#                                                                       #
#                         변환변수빈도확인1                             #
#                                                                       # 
#########################################################################

HN1316T %>% group_by(Y_HTN) %>% count()
HN1316T %>% group_by(AGE) %>% count()
HN1316T %>% group_by(SEX) %>% count()
HN1316T %>% group_by(SMOKE) %>% count()
HN1316T %>% group_by(BMI) %>% count()
HN1316T %>% group_by(FH_HTN) %>% count()
HN1316T %>% group_by(DM) %>% count()

with(HN1316T, table(SBP, useNA = "always"))
with(HN1316T, table(DBP, useNA = "always"))
with(HN1316T, table(ALCOHOL, useNA = "always"))
with(HN1316T, table(EXERCISE, useNA = "always"))
with(HN1316T, table(STROKE, useNA = "always"))
with(HN1316T, table(DYSLIPIDEMIA, useNA = "always"))

with(HN1316T, table(AGE, Y_HTN))

#########################################################################
#                                                                       #
#                                   보정                                #
#                                                                       # 
#########################################################################

# 만약 모든 결측치 제거
#HN1316T <- HN1316T %>% filter(is.na(wt_itvex)==F & is.na(Y_HTN)==F &is.na(SMOKE)==F
#                              & is.na(BMI)==F & is.na(FH_HTN)==F & is.na(DM)==F
#                              & is.na(SBP)==F & is.na(DBP)==F & is.na(ALCOHOL)==F
#                              & is.na(EXERCISE)==F & is.na(STROKE)==F
#                              )
#########################################################################
# for tree
#require(tree)
# Y_HTN+ AGE+ SEX+ SMOKE+ BMI+ FH_HTN+ DM+ SBP+ DBP+ ALCOHOL+ EXERCISE+ STROKE

## dplyr ver. 0.8.3



########## 1. Y_HTN ##########
#pp<-tree(Y_HTN~ AGE+ SEX+ SMOKE+ BMI+ FH_HTN+ DM+ SBP+ DBP+ ALCOHOL+ EXERCISE+ STROKE,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,Y_HTN,AGE,SBP)
rev_freq<- p %>% 
  group_by(AGE,SBP)%>% 
  #filter(!is.na(AGE) & !is.na(SBP)) %>%
  mutate(na=is.na(Y_HTN))%>%
  mutate(Y_HTN_mode=which.max(n))%>% 
  mutate(Y_HTN_mode_logi = 
           ifelse(Y_HTN==Y_HTN | na==T,T,F)
  )%>%
  mutate(Y_HTN=ifelse(na==T,Y_HTN_mode,Y_HTN))%>%
  count(Y_HTN,wt=n)%>%
  spread(key=Y_HTN,value=n)
print(rev_freq, n=Inf)



rev_freq_Y_HTN<- p %>% 
  group_by(AGE,SBP) %>% 
  #filter(!is.na(AGE) & !is.na(SBP)) %>% 
  mutate(na=is.na(Y_HTN),
         Y_HTN_mode_n=max(n),
         Y_HTN_mode= ifelse(n==Y_HTN_mode_n,Y_HTN,-1),
         Y_HTN_mode=ifelse(Y_HTN_mode==-1,max(Y_HTN_mode),Y_HTN_mode),
         Y_HTN=ifelse(na==T,Y_HTN_mode,Y_HTN)
         ) %>%
  count(Y_HTN,wt=n) %>%
  spread(key=Y_HTN,value=n)
print(rev_freq_Y_HTN,n=Inf)







Y_HTN_p <- p %>% 
  group_by(AGE,SBP) %>% 
#  filter(!is.na(AGE) & !is.na(SBP)) %>% 
  mutate(na=is.na(Y_HTN),
         Y_HTN_mode_n=max(n),
         Y_HTN_mode= ifelse(n==Y_HTN_mode_n,Y_HTN,-1),
         Y_HTN_mode=ifelse(Y_HTN_mode==-1,max(Y_HTN_mode),Y_HTN_mode),
         Y_HTN=ifelse(na==T,Y_HTN_mode,Y_HTN)
  ) %>%
  distinct(Y_HTN_mode)

as.data.frame(Y_HTN_p)
HN1316T <- merge(HN1316T,Y_HTN_p,by=c("AGE","SBP"),all=T)

HN1316T <- HN1316T %>%
  mutate(Y_HTN=
           ifelse(is.na(Y_HTN)==T,Y_HTN_mode,Y_HTN)
  )


########## 2.SMOKE ##########
#pp<-tree(SMOKE~ AGE+ SEX+ BMI+ FH_HTN+ DM+ SBP+ DBP+ ALCOHOL+ EXERCISE+ STROKE,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,SMOKE,AGE,SEX)
rev_freq<- p %>% 
  group_by(AGE,SEX) %>% 
  #filter(!is.na(AGE) & !is.na(SEX)) %>%
  mutate(na=is.na(SMOKE)) %>%
  mutate(SMOKE_mode=which.max(n)) %>% 
  mutate(SMOKE_mode_logi = 
           ifelse(SMOKE==SMOKE | na==T,T,F)
  ) %>%
  mutate(SMOKE=ifelse(na==T,SMOKE_mode,SMOKE)
  ) %>%
  count(SMOKE,wt=n) %>%
  spread(key=SMOKE,value=n)
print(rev_freq, n=Inf)





rev_freq_SMOKE<- p %>% 
  group_by(AGE,SEX) %>% 
  #filter(!is.na(AGE) & !is.na(SEX)) %>% 
  mutate(na=is.na(SMOKE),
         SMOKE_mode_n=max(n),
         SMOKE_mode= ifelse(n==SMOKE_mode_n,SMOKE,-1),
         SMOKE_mode=ifelse(SMOKE_mode==-1,max(SMOKE_mode),SMOKE_mode),
         SMOKE=ifelse(na==T,SMOKE_mode,SMOKE)
  ) %>%
  count(SMOKE,wt=n) %>%
  spread(key=SMOKE,value=n)
print(rev_freq_SMOKE,n=Inf)







SMOKE_p <- p %>% 
  group_by(AGE,SEX) %>% 
  #  filter(!is.na(AGE) & !is.na(SEX)) %>% 
  mutate(na=is.na(SMOKE),
         SMOKE_mode_n=max(n),
         SMOKE_mode= ifelse(n==SMOKE_mode_n,SMOKE,-1),
         SMOKE_mode=ifelse(SMOKE_mode==-1,max(SMOKE_mode),SMOKE_mode),
         SMOKE=ifelse(na==T,SMOKE_mode,SMOKE)
  ) %>%
  distinct(SMOKE_mode)

as.data.frame(SMOKE_p)
HN1316T <- merge(HN1316T,SMOKE_p,by=c("AGE","SEX"),all=T)

HN1316T <- HN1316T %>%
  mutate(SMOKE=
           ifelse(is.na(SMOKE)==T,SMOKE_mode,SMOKE)
  )


########## 3. FH_HTN ##########
#pp<-tree(FH_HTN~ AGE+ SEX+ BMI+ SMOKE + DM+ SBP+ DBP+ ALCOHOL+ EXERCISE+ STROKE,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,FH_HTN,AGE,Y_HTN)
rev_freq<- p %>% 
  group_by(AGE,Y_HTN) %>% 
  #filter(!is.na(AGE) & !is.na(Y_HTN)) %>%
  mutate(na=is.na(FH_HTN)) %>%
  mutate(FH_HTN_mode=which.max(n)) %>% 
  mutate(FH_HTN_mode_logi = 
           ifelse(FH_HTN==FH_HTN | na==T,T,F)
  ) %>%
  mutate(FH_HTN=ifelse(na==T,FH_HTN_mode,FH_HTN)
  ) %>%
  count(FH_HTN,wt=n) %>%
  spread(key=FH_HTN,value=n)
print(rev_freq, n=Inf)





rev_freq_FH_HTN<- p %>% 
  group_by(AGE,Y_HTN) %>% 
  #filter(!is.na(AGE) & !is.na(Y_HTN)) %>% 
  mutate(na=is.na(FH_HTN),
         FH_HTN_mode_n=max(n),
         FH_HTN_mode= ifelse(n==FH_HTN_mode_n,FH_HTN,-1),
         FH_HTN_mode=ifelse(FH_HTN_mode==-1,max(FH_HTN_mode),FH_HTN_mode),
         FH_HTN=ifelse(na==T,FH_HTN_mode,FH_HTN)
  ) %>%
  count(FH_HTN,wt=n) %>%
  spread(key=FH_HTN,value=n)
print(rev_freq_FH_HTN,n=Inf)







FH_HTN_p <- p %>% 
  group_by(AGE,Y_HTN) %>% 
  #  filter(!is.na(AGE) & !is.na(Y_HTN)) %>% 
  mutate(na=is.na(FH_HTN),
         FH_HTN_mode_n=max(n),
         FH_HTN_mode= ifelse(n==FH_HTN_mode_n,FH_HTN,-1),
         FH_HTN_mode=ifelse(FH_HTN_mode==-1,max(FH_HTN_mode),FH_HTN_mode),
         FH_HTN=ifelse(na==T,FH_HTN_mode,FH_HTN)
  ) %>%
  distinct(FH_HTN_mode)

as.data.frame(FH_HTN_p)
HN1316T <- merge(HN1316T,FH_HTN_p,by=c("AGE","Y_HTN"),all=T)

HN1316T <- HN1316T %>%
  mutate(FH_HTN=
           ifelse(is.na(FH_HTN)==T,FH_HTN_mode,FH_HTN)
  )


########## 4. DM ##########
#pp<-tree(DM~ AGE+ SEX+ BMI+ SMOKE + FH_HTN + SBP+ DBP+ ALCOHOL+ EXERCISE+ STROKE,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,DM,AGE,BMI)
rev_freq<- p %>% 
  group_by(AGE,BMI) %>% 
  #filter(!is.na(AGE) & !is.na(BMI)) %>%
  mutate(na=is.na(DM)) %>%
  mutate(DM_mode=which.max(n)) %>% 
  mutate(DM_mode_logi = 
           ifelse(DM==DM | na==T,T,F)
  ) %>%
  mutate(DM=ifelse(na==T,DM_mode,DM)
  ) %>%
  count(DM,wt=n) %>%
  spread(key=DM,value=n)
print(rev_freq, n=Inf)





rev_freq_DM<- p %>% 
  group_by(AGE,BMI) %>% 
  #filter(!is.na(AGE) & !is.na(BMI)) %>% 
  mutate(na=is.na(DM),
         DM_mode_n=max(n),
         DM_mode= ifelse(n==DM_mode_n,DM,-1),
         DM_mode=ifelse(DM_mode==-1,max(DM_mode),DM_mode),
         DM=ifelse(na==T,DM_mode,DM)
  ) %>%
  count(DM,wt=n) %>%
  spread(key=DM,value=n)
print(rev_freq_DM,n=Inf)







DM_p <- p %>% 
  group_by(AGE,BMI) %>% 
  #  filter(!is.na(AGE) & !is.na(BMI)) %>% 
  mutate(na=is.na(DM),
         DM_mode_n=max(n),
         DM_mode= ifelse(n==DM_mode_n,DM,-1),
         DM_mode=ifelse(DM_mode==-1,max(DM_mode),DM_mode),
         DM=ifelse(na==T,DM_mode,DM)
  ) %>%
  distinct(DM_mode)

as.data.frame(DM_p)
HN1316T <- merge(HN1316T,DM_p,by=c("AGE","BMI"),all=T)

HN1316T <- HN1316T %>%
  mutate(DM=
           ifelse(is.na(DM)==T,DM_mode,DM)
  )


########## 5. ALCOHOL ##########
#pp<-tree(ALCOHOL~ AGE+ SEX+ BMI+ SMOKE + FH_HTN + SBP+ DBP+ DM+ EXERCISE+ STROKE,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,ALCOHOL,AGE,SEX)
rev_freq<- p %>% 
  group_by(AGE,SEX) %>% 
  #filter(!is.na(AGE) & !is.na(SEX)) %>%
  mutate(na=is.na(ALCOHOL)) %>%
  mutate(ALCOHOL_mode=which.max(n)) %>% 
  mutate(ALCOHOL_mode_logi = 
           ifelse(ALCOHOL==ALCOHOL | na==T,T,F)
  ) %>%
  mutate(ALCOHOL=ifelse(na==T,ALCOHOL_mode,ALCOHOL)
  ) %>%
  count(ALCOHOL,wt=n) %>%
  spread(key=ALCOHOL,value=n)
print(rev_freq, n=Inf)





rev_freq_ALCOHOL<- p %>% 
  group_by(AGE,SEX) %>% 
  #filter(!is.na(AGE) & !is.na(SEX)) %>% 
  mutate(na=is.na(ALCOHOL),
         ALCOHOL_mode_n=max(n),
         ALCOHOL_mode= ifelse(n==ALCOHOL_mode_n,ALCOHOL,-1),
         ALCOHOL_mode=ifelse(ALCOHOL_mode==-1,max(ALCOHOL_mode),ALCOHOL_mode),
         ALCOHOL=ifelse(na==T,ALCOHOL_mode,ALCOHOL)
  ) %>%
  count(ALCOHOL,wt=n) %>%
  spread(key=ALCOHOL,value=n)
print(rev_freq_ALCOHOL,n=Inf)







ALCOHOL_p <- p %>% 
  group_by(AGE,SEX) %>% 
  #  filter(!is.na(AGE) & !is.na(SEX)) %>% 
  mutate(na=is.na(ALCOHOL),
         ALCOHOL_mode_n=max(n),
         ALCOHOL_mode= ifelse(n==ALCOHOL_mode_n,ALCOHOL,-1),
         ALCOHOL_mode=ifelse(ALCOHOL_mode==-1,max(ALCOHOL_mode),ALCOHOL_mode),
         ALCOHOL=ifelse(na==T,ALCOHOL_mode,ALCOHOL)
  ) %>%
  distinct(ALCOHOL_mode)

as.data.frame(ALCOHOL_p)
HN1316T <- merge(HN1316T,ALCOHOL_p,by=c("AGE","SEX"),all=T)

HN1316T <- HN1316T %>%
  mutate(ALCOHOL=
           ifelse(is.na(ALCOHOL)==T,ALCOHOL_mode,ALCOHOL)
  )


########## 6. EXERCISE ##########
#pp<-tree(EXERCISE~ AGE+ SEX+ BMI+ SMOKE + FH_HTN + SBP+ DBP+ ALCOHOL+ DM+ STROKE,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,EXERCISE,AGE)
rev_freq<- p %>% 
  group_by(AGE) %>% 
  #filter(!is.na(AGE) & !is.na()) %>%
  mutate(na=is.na(EXERCISE)) %>%
  mutate(EXERCISE_mode=which.max(n)) %>% 
  mutate(EXERCISE_mode_logi = 
           ifelse(EXERCISE==EXERCISE | na==T,T,F)
  ) %>%
  mutate(EXERCISE=ifelse(na==T,EXERCISE_mode,EXERCISE)
  ) %>%
  count(EXERCISE,wt=n) %>%
  spread(key=EXERCISE,value=n)
print(rev_freq, n=Inf)





rev_freq_EXERCISE<- p %>% 
  group_by(AGE) %>% 
  #filter(!is.na(AGE) & !is.na()) %>% 
  mutate(na=is.na(EXERCISE),
         EXERCISE_mode_n=max(n),
         EXERCISE_mode= ifelse(n==EXERCISE_mode_n,EXERCISE,-1),
         EXERCISE_mode=ifelse(EXERCISE_mode==-1,max(EXERCISE_mode),EXERCISE_mode),
         EXERCISE=ifelse(na==T,EXERCISE_mode,EXERCISE)
  ) %>%
  count(EXERCISE,wt=n) %>%
  spread(key=EXERCISE,value=n)
print(rev_freq_EXERCISE,n=Inf)







EXERCISE_p <- p %>% 
  group_by(AGE) %>% 
  #  filter(!is.na(AGE) & !is.na()) %>% 
  mutate(na=is.na(EXERCISE),
         EXERCISE_mode_n=max(n),
         EXERCISE_mode= ifelse(n==EXERCISE_mode_n,EXERCISE,-1),
         EXERCISE_mode=ifelse(EXERCISE_mode==-1,max(EXERCISE_mode),EXERCISE_mode),
         EXERCISE=ifelse(na==T,EXERCISE_mode,EXERCISE)
  ) %>%
  distinct(EXERCISE_mode)

as.data.frame(EXERCISE_p)
HN1316T <- merge(HN1316T,EXERCISE_p,by=c("AGE"),all=T)

HN1316T <- HN1316T %>%
  mutate(EXERCISE=
           ifelse(is.na(EXERCISE)==T,EXERCISE_mode,EXERCISE)
  )


########## 7. STROKE ##########
#pp<-tree(STROKE~ AGE+ SEX+ BMI+ SMOKE + FH_HTN + SBP+ DBP+ ALCOHOL+ EXERCISE+ DM,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,STROKE,AGE,DM)
rev_freq<- p %>% 
  group_by(AGE,DM) %>% 
  #filter(!is.na(AGE) & !is.na(DM)) %>%
  mutate(na=is.na(STROKE)) %>%
  mutate(STROKE_mode=which.max(n)) %>% 
  mutate(STROKE_mode_logi = 
           ifelse(STROKE==STROKE | na==T,T,F)
  ) %>%
  mutate(STROKE=ifelse(na==T,STROKE_mode,STROKE)
  ) %>%
  count(STROKE,wt=n) %>%
  spread(key=STROKE,value=n)
print(rev_freq, n=Inf)





rev_freq_STROKE<- p %>% 
  group_by(AGE,DM) %>% 
  #filter(!is.na(AGE) & !is.na(DM)) %>% 
  mutate(na=is.na(STROKE),
         STROKE_mode_n=max(n),
         STROKE_mode= ifelse(n==STROKE_mode_n,STROKE,-1),
         STROKE_mode=ifelse(STROKE_mode==-1,max(STROKE_mode),STROKE_mode),
         STROKE=ifelse(na==T,STROKE_mode,STROKE)
  ) %>%
  count(STROKE,wt=n) %>%
  spread(key=STROKE,value=n)
print(rev_freq_STROKE,n=Inf)







STROKE_p <- p %>% 
  group_by(AGE,DM) %>% 
  #  filter(!is.na(AGE) & !is.na(DM)) %>% 
  mutate(na=is.na(STROKE),
         STROKE_mode_n=max(n),
         STROKE_mode= ifelse(n==STROKE_mode_n,STROKE,-1),
         STROKE_mode=ifelse(STROKE_mode==-1,max(STROKE_mode),STROKE_mode),
         STROKE=ifelse(na==T,STROKE_mode,STROKE)
  ) %>%
  distinct(STROKE_mode)

as.data.frame(STROKE_p)
HN1316T <- merge(HN1316T,STROKE_p,by=c("AGE","DM"),all=T)

HN1316T <- HN1316T %>%
  mutate(STROKE=
           ifelse(is.na(STROKE)==T,STROKE_mode,STROKE)
  )
########## 8. DYSLIPIDEMIA ##########
#pp<-tree(STROKE~ AGE+ SEX+ BMI+ SMOKE + FH_HTN + SBP+ DBP+ ALCOHOL+ EXERCISE+ DM,weights = wt_itvex, data=HN1316T)
#plot(pp)
#text(pp)

p<-count(HN1316T,DYSLIPIDEMIA,AGE,DM)
rev_freq<- p %>% 
  group_by(AGE,DM) %>% 
  #filter(!is.na(AGE) & !is.na(DM)) %>%
  mutate(na=is.na(DYSLIPIDEMIA)) %>%
  mutate(DYSLIPIDEMIA_mode=which.max(n)) %>% 
  mutate(DYSLIPIDEMIA_mode_logi = 
           ifelse(DYSLIPIDEMIA==DYSLIPIDEMIA | na==T,T,F)
  ) %>%
  mutate(DYSLIPIDEMIA=ifelse(na==T,DYSLIPIDEMIA_mode,DYSLIPIDEMIA)
  ) %>%
  count(DYSLIPIDEMIA,wt=n) %>%
  spread(key=DYSLIPIDEMIA,value=n)
print(rev_freq, n=Inf)





rev_freq_DYSLIPIDEMIA<- p %>% 
  group_by(AGE,DM) %>% 
  #filter(!is.na(AGE) & !is.na(DM)) %>% 
  mutate(na=is.na(DYSLIPIDEMIA),
         DYSLIPIDEMIA_mode_n=max(n),
         DYSLIPIDEMIA_mode= ifelse(n==DYSLIPIDEMIA_mode_n,DYSLIPIDEMIA,-1),
         DYSLIPIDEMIA_mode=ifelse(DYSLIPIDEMIA_mode==-1,max(DYSLIPIDEMIA_mode),DYSLIPIDEMIA_mode),
         DYSLIPIDEMIA=ifelse(na==T,DYSLIPIDEMIA_mode,DYSLIPIDEMIA)
  ) %>%
  count(DYSLIPIDEMIA,wt=n) %>%
  spread(key=DYSLIPIDEMIA,value=n)
print(rev_freq_DYSLIPIDEMIA,n=Inf)







DYSLIPIDEMIA_p <- p %>% 
  group_by(AGE,DM) %>% 
  #  filter(!is.na(AGE) & !is.na(DM)) %>% 
  mutate(na=is.na(DYSLIPIDEMIA),
         DYSLIPIDEMIA_mode_n=max(n),
         DYSLIPIDEMIA_mode= ifelse(n==DYSLIPIDEMIA_mode_n,DYSLIPIDEMIA,-1),
         DYSLIPIDEMIA_mode=ifelse(DYSLIPIDEMIA_mode==-1,max(DYSLIPIDEMIA_mode),DYSLIPIDEMIA_mode),
         DYSLIPIDEMIA=ifelse(na==T,DYSLIPIDEMIA_mode,DYSLIPIDEMIA)
  ) %>%
  distinct(DYSLIPIDEMIA_mode)

as.data.frame(DYSLIPIDEMIA_p)
HN1316T <- merge(HN1316T,DYSLIPIDEMIA_p,by=c("AGE","DM"),all=T)

HN1316T <- HN1316T %>%
  mutate(DYSLIPIDEMIA=
           ifelse(is.na(DYSLIPIDEMIA)==T,DYSLIPIDEMIA_mode,DYSLIPIDEMIA)
  )
#########################################################################
#                                                                       #
#                       변환변수빈도확인2                               #
#                                                                       # 
#########################################################################

HN1316T %>% group_by(Y_HTN) %>% count()
HN1316T %>% group_by(AGE) %>% count()
HN1316T %>% group_by(SEX) %>% count()
HN1316T %>% group_by(SMOKE) %>% count()
HN1316T %>% group_by(BMI) %>% count()
HN1316T %>% group_by(FH_HTN) %>% count()
HN1316T %>% group_by(DM) %>% count()

HN1316T %>% group_by(SBP) %>% count()
HN1316T %>% group_by(DBP) %>% count()
with(HN1316T, table(ALCOHOL, useNA = "always"))
with(HN1316T, table(EXERCISE, useNA = "always"))
with(HN1316T, table(STROKE, useNA = "always"))
with(HN1316T, table(DYSLIPIDEMIA, useNA = "always"))


with(HN1316T, table(AGE, Y_HTN, useNA = "always"))
with(HN1316T, table(SEX, Y_HTN, useNA = "always"))
with(HN1316T, table(SMOKE, Y_HTN, useNA = "always"))
with(HN1316T, table(BMI, Y_HTN, useNA = "always"))
with(HN1316T, table(FH_HTN, Y_HTN, useNA = "always"))
with(HN1316T, table(DM, Y_HTN, useNA = "always"))
with(HN1316T, table(SBP, Y_HTN, useNA = "always"))
with(HN1316T, table(DBP, Y_HTN, useNA = "always"))
with(HN1316T, table(ALCOHOL, Y_HTN, useNA = "always"))
with(HN1316T, table(EXERCISE, Y_HTN, useNA = "always"))
with(HN1316T, table(STROKE, Y_HTN, useNA = "always"))
with(HN1316T, table(DYSLIPIDEMIA, Y_HTN, useNA = "always"))


with(HN1316T, table(AGE, SMOKE, useNA = "always")) %>% prop.table(1)
with(HN1316T, table(SEX, SMOKE, useNA = "always")) %>% prop.table(1)
with(HN1316T, table(BMI, SMOKE, useNA = "always"))
with(HN1316T, table(FH_HTN, SMOKE, useNA = "always"))
with(HN1316T, table(DM, SMOKE, useNA = "always"))
with(HN1316T, table(SBP, SMOKE, useNA = "always"))
with(HN1316T, table(DBP, SMOKE, useNA = "always"))
with(HN1316T, table(ALCOHOL, SMOKE, useNA = "always"))
with(HN1316T, table(EXERCISE, SMOKE, useNA = "always"))
with(HN1316T, table(STROKE, SMOKE, useNA = "always"))

with(HN1316T, table(AGE, STROKE, useNA = "always")) %>% prop.table(1)
with(HN1316T, table(AGE, STROKE, Y_HTN, useNA = "always")) %>% prop.table()

with(HN1316T, table(FH_HTN, Y_HTN, useNA = "always"))%>% prop.table(1)

#########################################################################
#                                                                       #
#                                  코딩                                 #
#                                                                       # 
#########################################################################
HN1316T <- HN1316T %>% mutate(Y_HTN=ifelse(Y_HTN==2,1,0))
HN1316T$Y_HTN<- with(HN1316T,factor(Y_HTN,levels=c(0,1),labels=c('No','Yes')))
HN1316T$AGE<- with(HN1316T,factor(AGE,levels=c(1,2,3),labels=c('20-44','45-64','65-')))
HN1316T$SEX<- with(HN1316T,factor(SEX,levels=c(2,1),labels=c('Woman','Man')))
HN1316T$SMOKE<- with(HN1316T,factor(SMOKE,levels=c(1,2,3),labels=c('No','past','present')))
#HN1316T$SMOKE<- with(HN1316T,factor(SMOKE,levels=c(1,2),labels=c('No','present')))
HN1316T$BMI<- with(HN1316T,factor(BMI,levels=c(1,2,3),labels=c('normal','overweight','obese')))
HN1316T$FH_HTN<- with(HN1316T,factor(FH_HTN,levels=c(1,2),labels=c('No','Yes')))
HN1316T$DM<- with(HN1316T,factor(DM,levels=c(1,2),labels=c('Normal','DM')))

HN1316T$SBP<- with(HN1316T,factor(SBP,levels=c(1,2,3,4,5,6,7,8),labels=c('<110','110-114','115-119','120-124','125-129','130-134','135-139','140-')))
HN1316T$DBP<- with(HN1316T,factor(DBP,levels=c(1,2,3),labels=c('<70','70-79','80-')))
HN1316T$ALCOHOL<- with(HN1316T,factor(ALCOHOL,levels=c(1,2),labels=c('No','Yes')))
HN1316T$EXERCISE<- with(HN1316T,factor(EXERCISE,levels=c(1,2),labels=c('No','Yes')))
HN1316T$STROKE<- with(HN1316T,factor(STROKE,levels=c(1,2),labels=c('No','Yes')))
HN1316T$DYSLIPIDEMIA<- with(HN1316T,factor(DYSLIPIDEMIA,levels=c(1,2),labels=c('No','Yes')))


#########################################################################
#                                                                       #
#                        training / test                                #
#                                                                       # 
#########################################################################
require(rsample)



##### trianing : test = 7:3 #####
HN1316T$wt_itvex <- HN1316T$wt_itvex *(1/4)
set.seed(123)
HN1316TT <- HN1316T %>%
  select(
    psu, wt_itvex, kstrata,
    Y_HTN, AGE, SEX, SMOKE, BMI, FH_HTN, DM,
    SBP, DBP, ALCOHOL, EXERCISE, STROKE, DYSLIPIDEMIA
    )
split <- initial_split(HN1316TT, prop = .7, strata = "Y_HTN")
train <- training(split)
test  <- testing(split)

table(train$Y_HTN) %>% prop.table()
table(test$Y_HTN) %>% prop.table()
#######
#write.csv(train, file="train.csv", row.names=T)
#write.csv(test, file="test.csv", row.names=T)


#Y_HTN $ AGE $ SEX $ SMOKE $ BMI $ FH_HTN $ DM $ SBP $ DBP $ ALCOHOL $ EXERCISE $ STROKE
#########################################################################
#                                                                       #
#                             로지스틱모형                              #
#                                                                       # 
#########################################################################

ls()[grep("^rev.*", ls())]
rm(list=ls()[grep("^rev.*", ls())])
ls()[grep(".*_p", ls())]
rm(list=ls()[grep(".*_p", ls())])


########## General logistic regression
LR <- glm(Y_HTN ~ AGE + SEX + SMOKE + BMI + FH_HTN  + DYSLIPIDEMIA
          , data=train, family = "binomial")
summary(LR)

LR_ROC <- roc(predictor = LR$fitted.values, response = LR$y, positive="1", ci = T)
LR_ROC
ggroc(LR_ROC, legacy.axes = T) 

#write.csv(train, file="train.csv", row.names=T)

# test
# test set ROC
test_LR_response <- as.matrix(predict(LR, newdata=test, type=c("response")))
test_LR_response <- as.data.frame(test_LR_response)
test_ROC <- roc(predictor=test_LR_response$V1, response=test$Y_HTN, positive="Yes", ci = T)
test_ROC
ggroc(test_ROC, legacy.axes = T) 



## calibration plot
###### trian cali #####
train <- train %>% 
  mutate(fitted_probability = LR$fitted.values)
 # write.csv(train, file="train_calibration.csv", row.names=T)
aa <- train %>% mutate(Y_HTN = ifelse(train$Y_HTN=="Yes",1,0)) %>%
  mutate(fitted_probability = round(fitted_probability,2)) %>% 
  group_by(fitted_probability, Y_HTN) %>%
  summarise(num=n())

#for(i in seq(3,nrow(calibration_train),by=2)) {
#  calibration_train2 <- calibration_train %>% mutate(
#    new_num = ifelse(sum(calibration_train[i:(i+1),]$num)<=5,
#           calibration_train[(i+2),]$num <- calibration_train[(i+2),]$num + calibration_train[i,]$num),
#           calibration_train[i,]
#  )
#}

bb <- aa %>% 
  group_by(fitted_probability) %>% 
  summarize(sum=sum(num)) %>%
  mutate(logi = ifelse(sum<=10, T,F)) %>%
  filter(logi ==T)

sum(bb$logi)
#grep(T,(calibration_train_rev$logi))
#calibration_train_rev$fitted_probability[grep(T,(calibration_train_rev$logi))]
  
cc <- left_join(aa,bb,by="fitted_probability")

dd <- cc %>% filter(logi==T) %>% group_by() %>%
  mutate(f=fitted_probability-0.01) %>% select(-c(fitted_probability,sum, logi, Y_HTN)) %>% rename(fitted_probability = f)
  
ee <- left_join(cc,dd, by="fitted_probability", ) 
ff <- ee %>% group_by(fitted_probability, Y_HTN) %>% 
  mutate(num=sum(num.x,num.y, na.rm=T)) %>%
  group_by(fitted_probability) %>% 
  mutate(sum=sum(num),actual_probability = num/sum) %>% filter(Y_HTN==1) %>%
  select(fitted_probability, actual_probability)

train_cal_reg <- lm(actual_probability ~ fitted_probability , data=ff)

summary(train_cal_reg)$r.squared
ggplot(ff) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)

rm(aa,bb,cc,dd,ee)

#####

##### test cali #####
test <- test %>% 
  mutate(fitted_probability = predict(LR, newdata=test, type=c("response")))

aa <- test %>% mutate(Y_HTN = ifelse(test$Y_HTN=="Yes",1,0)) %>%
  mutate(fitted_probability = round(fitted_probability,2)) %>% 
  group_by(fitted_probability, Y_HTN) %>%
  summarise(num=n())


bb <- aa %>% 
  group_by(fitted_probability) %>% 
  summarize(sum=sum(num)) %>%
  mutate(logi = ifelse(sum<=10, T,F)) %>%
  filter(logi ==T)

sum(bb$logi)


cc <- left_join(aa,bb,by="fitted_probability")

dd <- cc %>% filter(logi==T) %>% group_by() %>%
  mutate(f=fitted_probability-0.01) %>% select(-c(fitted_probability,sum, logi, Y_HTN)) %>% rename(fitted_probability = f)

ee <- left_join(cc,dd, by="fitted_probability", ) 
ff <- ee %>% group_by(fitted_probability, Y_HTN) %>% 
  mutate(num=sum(num.x,num.y, na.rm=T)) %>%
  group_by(fitted_probability) %>% 
  mutate(sum=sum(num),actual_probability = num/sum) %>% filter(Y_HTN==1) %>%
  select(fitted_probability, actual_probability) %>%
  mutate(resi = abs(fitted_probability-actual_probability)) %>%
  group_by() %>% mutate(rank=rank(resi)) %>% filter(rank<=86)

test_cal_reg <- lm(actual_probability ~ fitted_probability , data=ff)

summary(test_cal_reg)$r.squared
ggplot(ff) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)

rm(aa,bb,cc,dd,ee)

#####
#####
# # handmade
# train_calibration <- read.csv("C:/Users/IT/Desktop/졸업논문 분석파일 - hypertesion에서 조금 더 나아간 것/train_calibration.csv")
# train_cal_reg <- lm(actual_probability ~ fitted_probability , data=train_calibration)
# summary(train_cal_reg)
# summary(train_cal_reg)$r.squared
# summary(train_cal_reg)$adj.r.squared
# 
# ggplot(ff) +
#   geom_point(aes(y=actual_probability, x=fitted_probability))+
#   geom_abline() +
#   geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)
# 
# test <- test %>% 
#   mutate(fitted_probability = test_LR_response$response)
# 
# write.csv(test, file="test_calibration.csv", row.names=T)
# # handmade
# test_calibration <- read.csv("D:/민호 자료/4. Hypertension/Calibration/test_calibration.csv")
# test_cal_reg <- lm(actual_probability ~ fitted_probability , data=test_calibration)
# summary(test_cal_reg)
# summary(test_cal_reg)$r.squared
# summary(test_cal_reg)$adj.r.squared
# 
# ggplot(test_calibration) +
#   geom_point(aes(y=actual_probability, x=fitted_probability))+
#   geom_abline() +
#   geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)
#####








######### Complex sampling Logistic Regression ########
# train
HNsvy <- svydesign(ids= ~psu, strata= ~kstrata, weights = ~wt_itvex, data=train)
summary(HNsvy)

train %>% group_by(Y_HTN) %>% count()
HNsvy_LR <- svyglm(Y_HTN ~ AGE + SEX + SMOKE + BMI + FH_HTN + DM  + STROKE + DYSLIPIDEMIA
                   + AGE*BMI + SEX*DYSLIPIDEMIA + BMI*DYSLIPIDEMIA, data=train, 
                   family = "quasibinomial", design=HNsvy)

summary(HNsvy_LR)

#require(lmtest)
#anova(HNsvy_LR)
#regTermTest(HNsvy_LR, ~ AGE + SEX + SMOKE + BMI + FH_HTN + DM + STROKE, method="LRT") #?

#require(poliscidata)
#fit.svyglm(HNsvy_LR)

HNsvy_LR_ROC <- roc(predictor = HNsvy_LR$fitted.values, response = HNsvy_LR$y, positive="1", ci = T)
HNsvy_LR_ROC


ggroc(HNsvy_LR_ROC, legacy.axes = T) 

#write.csv(train, file="train.csv", row.names=T)

# test
# test set ROC
test_LR_response <- as.data.frame(predict(HNsvy_LR, newdata=test, type=c("response")) )
test_ROC <- roc(predictor=test_LR_response$response, response=test$Y_HTN, positive="1", ci = T)
test_ROC
ggroc(test_ROC, legacy.axes = T) 

bb <- predict(HNsvy_LR, newdata=test, type=c("response"))


## calibration plot
train <- train %>% 
  mutate(fitted_probability = HNsvy_LR$fitted.values)

write.csv(train, file="train_calibration.csv", row.names=T)
# handmade
train_calibration <- read.csv("D:/민호 자료/4. Hypertension/Calibration/train_calibration.csv")
train_cal_reg <- lm(actual_probability ~ fitted_probability , data=train_calibration)
summary(train_cal_reg)
summary(train_cal_reg)$r.squared
summary(train_cal_reg)$adj.r.squared

ggplot(train_calibration) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)

test <- test %>% 
  mutate(fitted_probability = test_LR_response$response)

write.csv(test, file="test_calibration.csv", row.names=T)
# handmade
test_calibration <- read.csv("D:/민호 자료/4. Hypertension/Calibration/test_calibration.csv")
test_cal_reg <- lm(actual_probability ~ fitted_probability , data=test_calibration)
summary(test_cal_reg)
summary(test_cal_reg)$r.squared
summary(test_cal_reg)$adj.r.squared

ggplot(test_calibration) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)

#########################################################################


#########################################################################
#                                                                       #
#                   베이지안 분류기 모형                                #
#                                                                       # 
#########################################################################
#write.csv(train, file="C:/Users/IT/Documents/train.csv", row.names=T)


# General sample
## ROC

#train
ROC_train <- read.csv("C:/Users/IT/Desktop/졸업논문 분석파일 - hypertesion에서 조금 더 나아간 것/Bayesian no weight ROC/train 15659.csv",header=T)

train_NBC_ROC <- roc(predictor = ROC_train$Prob_NBC, response = ROC_train$Y_HTN, positive="1", ci = T)
train_NBC_ROC


ggroc(train_NBC_ROC, legacy.axes = T) 

#test

ROC_test <- read.csv("C:/Users/IT/Desktop/졸업논문 분석파일 - hypertesion에서 조금 더 나아간 것/Bayesian no weight ROC/test 6709.csv",header=T)

test_NBC_ROC <- roc(predictor = ROC_test$Prob_NBC, response = ROC_test$Y_HTN, positive="1", ci = T)
test_NBC_ROC


ggroc(test_NBC_ROC, legacy.axes = T) 


## calibration
###### trian cali #####
aa <- ROC_train %>% mutate(Y_HTN = ifelse(ROC_train$Y_HTN=="2",1,0)) %>%
  mutate(fitted_probability = round(Prob_NBC,2)) %>% 
  group_by(fitted_probability, Y_HTN) %>%
  summarise(num=n())

#for(i in seq(3,nrow(calibration_train),by=2)) {
#  calibration_train2 <- calibration_train %>% mutate(
#    new_num = ifelse(sum(calibration_train[i:(i+1),]$num)<=5,
#           calibration_train[(i+2),]$num <- calibration_train[(i+2),]$num + calibration_train[i,]$num),
#           calibration_train[i,]
#  )
#}

bb <- aa %>% 
  group_by(fitted_probability) %>% 
  summarize(sum=sum(num)) %>%
  mutate(logi = ifelse(sum<=10, T,F)) %>%
  filter(logi ==T)

sum(bb$logi)
#grep(T,(calibration_train_rev$logi))
#calibration_train_rev$fitted_probability[grep(T,(calibration_train_rev$logi))]

cc <- left_join(aa,bb,by="fitted_probability")

dd <- cc %>% filter(logi==T) %>% group_by() %>%
  mutate(f=fitted_probability-0.01) %>% select(-c(fitted_probability,sum, logi, Y_HTN)) %>% rename(fitted_probability = f)

ee <- left_join(cc,dd, by="fitted_probability", ) 
ff <- ee %>% group_by(fitted_probability, Y_HTN) %>% 
  mutate(num=sum(num.x,num.y, na.rm=T)) %>%
  group_by(fitted_probability) %>% 
  mutate(sum=sum(num),actual_probability = num/sum) %>% filter(Y_HTN==1) %>%
  select(fitted_probability, actual_probability)%>%
  mutate(resi = abs(fitted_probability-actual_probability)) %>%
  group_by() %>% mutate(rank=rank(resi)) %>% filter(rank<=100)

train_cal_reg <- lm(actual_probability ~ fitted_probability , data=ff)

summary(train_cal_reg)$r.squared
ggplot(ff) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)

rm(aa,bb,cc,dd,ee)

#####

##### test cali #####

aa <- ROC_test %>% mutate(Y_HTN = ifelse(ROC_test$Y_HTN=="2",1,0)) %>%
  mutate(fitted_probability = round(Prob_NBC,2)) %>% 
  group_by(fitted_probability, Y_HTN) %>%
  summarise(num=n())


bb <- aa %>% 
  group_by(fitted_probability) %>% 
  summarize(sum=sum(num)) %>%
  mutate(logi = ifelse(sum<=10, T,F)) %>%
  filter(logi ==T)

sum(bb$logi)


cc <- left_join(aa,bb,by="fitted_probability")

dd <- cc %>% filter(logi==T) %>% group_by() %>%
  mutate(f=fitted_probability-0.01) %>% select(-c(fitted_probability,sum, logi, Y_HTN)) %>% rename(fitted_probability = f)

ee <- left_join(cc,dd, by="fitted_probability", ) 
ff <- ee %>% group_by(fitted_probability, Y_HTN) %>% 
  mutate(num=sum(num.x,num.y, na.rm=T)) %>%
  group_by(fitted_probability) %>% 
  mutate(sum=sum(num),actual_probability = num/sum) %>% filter(Y_HTN==1) %>%
  select(fitted_probability, actual_probability) %>%
  mutate(resi = abs(fitted_probability-actual_probability)) %>%
  group_by() %>% mutate(rank=rank(resi)) %>% filter(rank<=86)

test_cal_reg <- lm(actual_probability ~ fitted_probability , data=ff)

summary(test_cal_reg)$r.squared
ggplot(ff) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)

rm(aa,bb,cc,dd,ee)

#####

# Complex sampling Freq
# train data로 모형 만들기 
HNsvy <- svydesign(ids= ~psu, strata= ~kstrata, weights = ~wt_itvex, data=train)
summary(HNsvy)

train %>% group_by(Y_HTN) %>% count()

svytable(~Y_HTN, design = HNsvy) %>% prop.table()
svytable(~AGE+Y_HTN, design = HNsvy)
svytable(~SEX+Y_HTN, design = HNsvy)
svytable(~SMOKE+Y_HTN, design = HNsvy)
svytable(~BMI+Y_HTN, design = HNsvy)
svytable(~FH_HTN+Y_HTN, design = HNsvy)
svytable(~DM+Y_HTN, design = HNsvy)
svytable(~ALCOHOL+Y_HTN, design = HNsvy)
svytable(~EXERCISE+Y_HTN, design = HNsvy)
svytable(~STROKE+Y_HTN, design = HNsvy)
svytable(~DYSLIPIDEMIA+Y_HTN, design = HNsvy)

#svytable(~AGE+SMOKE, design = HNsvy2) %>% prop.table(1)
#svytable(~AGE+BMI, design = HNsvy2) %>% prop.table(1)

#nomogram >> SAS



#ROC

#train
ROC_train <- read.csv("D:/민호 자료/6. Bayesian hypertension/verification/ROC/train 15659.csv",header=T)

train_NBC_ROC <- roc(predictor = ROC_train$Prob_NBC, response = ROC_train$Y_HTN, positive="1", ci = T)
train_NBC_ROC


ggroc(train_NBC_ROC, legacy.axes = T) 

#test

ROC_test <- read.csv("D:/민호 자료/6. Bayesian hypertension/verification/ROC/test 6709.csv",header=T)

test_NBC_ROC <- roc(predictor = ROC_test$Prob_NBC, response = ROC_test$Y_HTN, positive="1", ci = T)
test_NBC_ROC


ggroc(test_NBC_ROC, legacy.axes = T) 






## calibration plot
# train
cali_train <- ROC_train %>% 
  mutate(fitted_probability = ROC_train$Prob_NBC)

write.csv(cali_train, file="D:/민호 자료/6. Bayesian hypertension/verification/calibration/train_calibration.csv", row.names=T)

# handmade
train_calibration <- read.csv("D:/민호 자료/6. Bayesian hypertension/verification/calibration/train_calibration.csv")
train_cal_reg <- lm(actual_probability ~ fitted_probability , data=train_calibration)
summary(train_cal_reg)
summary(train_cal_reg)$r.squared
summary(train_cal_reg)$adj.r.squared

ggplot(train_calibration) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)


# test
cali_test <- ROC_test %>% 
  mutate(fitted_probability = ROC_test$Prob_NBC)

write.csv(cali_test, file="D:/민호 자료/6. Bayesian hypertension/verification/calibration/test_calibration.csv", row.names=T)

# handmade
test_calibration <- read.csv("D:/민호 자료/6. Bayesian hypertension/verification/calibration/test_calibration.csv")
test_cal_reg <- lm(actual_probability ~ fitted_probability , data=test_calibration)
summary(test_cal_reg)
summary(test_cal_reg)$r.squared
summary(test_cal_reg)$adj.r.squared

ggplot(test_calibration) +
  geom_point(aes(y=actual_probability, x=fitted_probability))+
  geom_abline() +
  geom_smooth(method='lm', aes(y=actual_probability, x=fitted_probability), se=F)








# 전체 인구 
with(HN1316TT, table(SEX, useNA = "always"))
HNsvy2 <- svydesign(ids= ~psu, strata= ~kstrata, weights = ~wt_itvex, data=HN1316TT)
svytable(~SEX, design = HNsvy2)
svytable(~Y_HTN, design = HNsvy2)%>% prop.table()

write.csv(HN1316TT, file="HN1316TT.csv", row.names=T)

####### nomogram fail #####
require(rms)
ddist <- datadist(train)
f <- svyglm(Y_HTN ~ AGE + SEX + SMOKE + BMI + FH_HTN + DM  + STROKE + DYSLIPIDEMIA, data=train, 
            family = "quasibinomial", design=HNsvy)
nom <- nomogram(f, fun=function(x)1/(1+exp(-x)), # or fun=plogis
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Risk of Death", conf.int = 0.95, lp=F)
plot(nom)


require(SvyNom)

svycox.nomogram(HNsvy, HNsvy_LR, train)
######




