################################
# freq ~ corpus + sex + adverb #
################################

rm(list = ls())
setwd("D:/KOREA_Univ/2021-1/한국어정보처리/NNN 4조 팀플 NNN/not used")
data <- read.table("MAGandMAJ_1is_FandSpoken.csv",
                   header = T, sep =',')
data <- data.frame(data)
head(data)
str(data)


# mag랑 maj freq값들을 한 열로 연결해버리기 -> Data 만들기기
freq <- c(data$MAG, data$MAJ) # 전체부사 빈도수
adverb_c <- c(rep("MAG", 66), rep("MAJ", 66)) # 부사 종류
sex <- c(data$SEX, data$SEX)
corpus <- c(data$CORPUS, data$CORPUS)


Data <- data.frame(freq, adverb_c, sex, corpus)
head(Data)
tail(Data)
# anova : 비모수 검정
### 모수검정으로 테스트해봄ㅎ 
result <- aov(freq ~ sex + corpus + adverb_c, 
              data = Data)
summary(result)

TukeyHSD(result)
### kruskal-wallis test 실시..비모수검정 못해먹겠다. ----
friedman_test(Data.freq ~ c | s, data = t)

c <- factor(Data$corpus)
s <- factor(Data$sex)

t <- data.frame(Data$freq, c, s)

?friedman.test.formula



#----------------------------
########### data ############
# MAG_freq ~ corpus + sex   #
# MAJ_freq ~ corpus + sex   #
# TOTAL_freq ~ corpus + sex #
#############################
head(data)

# MAG_freq ~ corpus + sex #
aov_MAG <- aov(MAG ~ SEX + CORPUS, data = data)
summary(aov_MAG) # CORPUS p<2e-16 ***
TukeyHSD(aov_MAG)

# MAJ_freq ~ corpus + sex #
aov_MAJ <- aov(MAJ ~ SEX + CORPUS, data = data)
summary(aov_MAJ) # CORPUS p<2e-16 ***
TukeyHSD(aov_MAJ)

# TOTAL_freq ~ corpus + sex #
aov_TOTAL <- aov(total ~ SEX + CORPUS, data = data)
summary(aov_TOTAL) # CORPUS p<2e-16 ***
TukeyHSD(aov_TOTAL)


# 설명력을 쓸 수 있나? ####

# 일반부사에서 성별과 코퍼스의 설명력 계산 
 # SST = 439.5
 # 성별의 설명력: 
 1.3/439.5
   # 즉, 0.3%
 # 코퍼스의 설명력:
 351.1/439.5
   # 즉, 79.9%
 
 # 종속부사에서 성별과 코퍼스의 설명력 계산 
 # SST = 49.41
 # 성별의 설명력:
 0.15/49.41
   # 즉, 0.30%
 # 코퍼스의 설명력:
 34.83/49.41
   # 즉, 70.5%
 
 # 전체부사에서 성별과 코퍼스의 설명력 계산
 # SST = 733.5
 # 성별의 설명력:
 2.4/733.5
   # 즉, 0.327%
 # 코퍼스의 설명력:
 607.1/733.5
   # 즉, 82.8%
# levene test? ####
head(data)
#library(car)
leveneTest(total ~ SEX * CORPUS, 
           data = data) # nope (p = 0.02213*)

