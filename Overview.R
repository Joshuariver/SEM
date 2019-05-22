# 구조방정식 실습
# Overview
#

# 인근에 자리잡은 두 학교, Grant-White 와  Pasteur를 두고 학부형들끼리 논쟁을 벌이고 있다. 
# 두 학교 중에 어느 학교가 학생을 잘 가르치는가에 대한 문제를 두고말이다.  평가영역은 '시각',
# '문장','반응성'이다.  이 평가 영역에 관한 9개의 항목이 x1, x2, x3, x4, x5, x6, x7, x8, x9다.

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

library(lavaan)
library(semPlot)
library(semTools)

data("HolzingerSwineford1939")
dim(HolzingerSwineford1939)
names(HolzingerSwineford1939)
head(HolzingerSwineford1939)
str(HolzingerSwineford1939)
summary(HolzingerSwineford1939)
?HolzingerSwineford1939


# Chapter 1. 어떤 분석 방법을 사용할 것인가?
# 시각, 문장, 반응성을 어떻게 측정할 것인가? 차원(요인)축소에 대한 고민
# 기술통계 vs. 추리통계
# 탐색적 요인분석
# 평균 차이 검정
# 정규성이 인정되는 경우에 t-test
# 정규성이 인정되지 않는 경우에는 비모수 통계검정
# 확인적 요인분석



# 탐색적 요인분석 (Exploratory Factor Analysis)
# 탐색적 요인분석의 목적은 데이터를 이모저모 듣어봄으로써, 데이터가 분석이 가능한 데이터 
# 인지를 검증하고 분석가능할 경우에 차원(요인)을 얼마나 줄여야 의미가 있을 것인지를 결정

############################################################################
# 1단계: 요인의 갯수의 결정 - 주성분 분석 (Principal Component Analysis, PCA)
############################################################################


.PC <- princomp(HolzingerSwineford1939[, 7:15])
screeplot(.PC)


############################################################################
# 2단계: 데이터의 속성 파악: EFA (Exploratory Factor Analysis)
############################################################################

.FA <- factanal(HolzingerSwineford1939[, 7:15], factor=3, rotation="varimax", scores="regression")
.FA

# 탐색적 요인분석은 다변량 정보를 단변량화 시키는 차원 축소의 영역이다.

HolzingerSwineford1939$F1 <- .FA$scores[,1]
HolzingerSwineford1939$F2 <- .FA$scores[,2]
HolzingerSwineford1939$F3 <- .FA$scores[,3]

# tutorial 에서의 기본 모델 설정

model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6; speed =~ x7 + x8 + x9'
fit <- cfa(model,data=HolzingerSwineford1939)
summary(fit, standardized = TRUE,fit.measures = TRUE)
moreFitIndices(fit)
semPaths(fit, "std")

# 집단별 평균 차이 검정 (t-test) - 신종화 박사의 방식

t.test(F1~school, alternative='two.sided', conf.level=.95, var.equal=FALSE, data= HolzingerSwineford1939)
t.test(F2~school, alternative='two.sided', conf.level=.95, var.equal=FALSE, data= HolzingerSwineford1939)
t.test(F3~school, alternative='two.sided', conf.level=.95, var.equal=FALSE, data= HolzingerSwineford1939)

# 집단별 평균 차이 검정 (ANOVA) - Tutorial 방식

config <- cfa(model,
              data=HolzingerSwineford1939,
              group="school")

weak <- cfa(model,data=HolzingerSwineford1939,group="school",group.equal="loadings")
strong<- cfa(model,data=HolzingerSwineford1939,group="school", group.equal = c("loadings", "intercepts"))
strict<- cfa(model,data=HolzingerSwineford1939,group="school", group.equal =c("loadings", "intercepts",
"residuals"))
anova(config, weak, strong, strict)

# 개별 모델들에 대한 CFI 인덱스가 ANOVA 에서는 나오지 않으므로 별도로 분석해야 함. 
# - Tutorial 방식

# measurementInvariance(model=model,data=HolzingerSwineford1939,group="school")
cat(as.character(measEq.syntax(configural.model=model,data=HolzingerSwineford1939,group="school")))


############################################################################
# 3단계: 확인적 요인분석 (Confirmatory Factor Analysis, CFA)
# 
# 탐색적 요인분석
# - 의미있는 특징을 발견하고, 수리적으로 정리하는 데 사용한다.
#
# 확인적 요인분석
# - 경험적인 직관이나 지식에 기반한 추론의 적절성 여부
# - 적절성 판단 이후에 수리적인 논거를 제시하는 데 사용한다
# - 연역적 연구방법 
#
############################################################################


library(psych)
?mardia
library(mvnormtest)
?mshapiro.test

# normality vs. Partical Least Squares (PLS)
# 모형적합성이란? 연역적 논거의 설계가 설득적인가에 대한 입장 표명
# - 설계된 모형과 수집된 표본의 분석 결과가 차이가 있는가의 여부: χ²검정

# 모형의 설계 - 신종화 박사의 방식

cfa.model1 <- 'F1 =~ x1 + x2 + x3; F2 =~ x4 + x5 + x6; F3 =~ x7 + x8 + x9'

# 모형 적합성 판단

cfa.model1.fit <- cfa(cfa.model1, data=HolzingerSwineford1939, estimator="MLR")
cfa.model1.fit
summary(cfa.model1.fit, fit.measures=TRUE)

# 집단별 모형 적합성 판단: 기초 - 데이터셋에 집단변수로 'school'이 들어있다.

cfa.model1.fit.group <- cfa(cfa.model1, data=HolzingerSwineford1939, estimator="MLR", group='school')
cfa.model1.fit.group
summary(cfa.model1.fit.group, fit.measures=TRUE)

# 확인적 요인분석: 측정 동일성 - loadings 포함
cfa.model1.fit.group.loadings <- cfa(cfa.model1, data=HolzingerSwineford1939, estimator="MLR", group='school',group.equal="loadings")
cfa.model1.fit.group.loadings
summary(cfa.model1.fit.group.loadings, fit.measures=TRUE)

# 확인적 요인분석: 측정 동일성 - intercepts  추가
cfa.model1.fit.group.loadings.intercepts <- cfa(cfa.model1, data=HolzingerSwineford1939, estimator="MLR", group='school',group.equal=c("loadings","intercepts"))
cfa.model1.fit.group.loadings.intercepts
summary(cfa.model1.fit.group.loadings.intercepts, fit.measures=TRUE)

# ANOVA 실시를 통한 χ² 검증 - cfa.model1.fit.group.loadings와
# cfa.model1.fit.group.loadings.intercepts의 차이가 분산분석으로 드러난다.

anova(cfa.model1.fit.group.loadings, cfa.model1.fit.group.loadings.intercepts)

# 관찰변수 중에서 mi 가 큰 두 개를 선택하려고 한다.
options(max.print=1000000)
 modindices(cfa.model1.fit.group.loadings.intercepts)

# 모델 중 의미있는 변수 도출
cfa.model1.fit.group.loadings.intercepts.group.partial <- cfa(cfa.model1,data=HolzingerSwineford1939,group="school", group.partial =
c("x3 ~1", "x7~1"))

### Grant-White 의 intercept에서 요인들 (F1, F2, F3)의 점수를 살펴보고, 유의확률로 근거를 확보한다.
### Grant-White 의 F2의 상수값 0.576이 유의확률을 바탕으로 Pasteur 학교보다 높다고 말할 수 있다.

# 여기 부터는 Tutorial 방식
# 하기 분석에 따르면 x3 인덱스가 MI (Modification Indice)가 intercept 중 
# 가장 크다는 것을 알 수 있다.- Tutorial 방식
mod_strong<-modificationIndices(strong)
mod_strong[mod_strong$op == "~1",]
cat(as.character(measEq.syntax(configural.model=model,data=HolzingerSwineford1939,group="school", group.partial =c("x3 ~1", "x7~1"))))

