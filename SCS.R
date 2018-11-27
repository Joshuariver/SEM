rm(list=ls())
setwd("~/R/Structural Equation Modeling")

library(lavaan)
library(semPlot)
library(semTools)

download.file("http://personalitytesting.info/_rawdata/SCS.zip","SCS.zip")
unzip("SCS.zip")
tmp <- read.csv("SCS/data.csv")
scs <- subset(tmp, gender == "1" |gender == "2")


# Specifying and inspecting the baseline model

scs_model_fit<- cfa(scs_model, ordered =
                      c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
                        "Q7", "Q8", "Q9", "Q10"), data=scs)
summary(scs_model_fit, standardized =
            TRUE, fit.measures = TRUE)

semPaths(scs_model_fit, "std",
         curvePivot = TRUE, thresholds = FALSE)


# Running multiple-group tests

config <- cfa(model,
              data=HolzingerSwineford1939,
              group="school")
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

