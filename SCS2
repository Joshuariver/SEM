# Revised code from SCS.R

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

library(lavaan)
library(semPlot)
library(semTools)

# download.file("http://personalitytesting.info/_rawdata/SCS.zip","SCS.zip")
# unzip("SCS.zip")
tmp <- read.csv("SCS.csv")
scs <- subset(tmp, gender == "1" |gender == "2")

.PC <- princomp(scs[, 1:10])
screeplot(.PC)

.FA <- factanal(scs[, 1:10], factor=3, rotation="varimax", scores="regression")
.FA

scs$F1 <- .FA$scores[,1]
scs$F2 <- .FA$scores[,2]
scs$F3 <- .FA$scores[,3]


# Specifying and inspecting the baseline model

scs_model <- 'F1 =~ Q5 + Q7 + Q8 + Q9; F2 =~ Q1 + Q2 + Q3 + Q4 ; F3 =~ Q6 + Q10'

scs_model_fit<- cfa(scs_model, ordered =
                      c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
                        "Q7", "Q8", "Q9", "Q10"), data=scs)
summary(scs_model_fit, standardized =
            TRUE, fit.measures = TRUE)

semPaths(scs_model_fit, "std",
         curvePivot = TRUE, thresholds = FALSE)


# Running multiple-group tests

config <- cfa(scs_model,
              data=scs,
              group="gender")
scs_model_weak <- cfa(scs_model,
                        ordered = c("Q1", "Q2", "Q3", "Q4",
                                    "Q5", "Q6", "Q7", "Q8", "Q9",
                                    "Q10"), group = "gender",
                        group.equal = c("loadings"),
                        data=scs)

semTools:::difftest(scs_model_config,
                      scs_model_weak)

scs_model_strong <- cfa(scs_model,
                        ordered = c("Q1", "Q2", "Q3", "Q4",
                                    "Q5", "Q6", "Q7", "Q8", "Q9",
                                    "Q10"), group = "gender",
                        group.equal = c("loadings",
                                        "thresholds"), data=scs)
semTools:::difftest(scs_model_weak,
                      scs_model_strong)



measurementInvariance(scs_model,
                      data=scs, group="gender",
                      strict=TRUE)

