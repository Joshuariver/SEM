# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: Moderating Effects (조절변수)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: Simplified Customer Satisfaction
#  Two-Stage Regression Approach (이단계 회귀 접근법)

# load package 'plspm'
library("plspm")

# get data satisfaction
data(satisfaction)

# Stage #1

# create path matrix
f1 = c(0, 0, 0)
f2 = c(0, 0, 0)
f3 = c(1, 1, 0)
reg_path = rbind(f1, f2, f3)
rownames(reg_path) = c("Image", "Satisfaction", "Loyalty")
# define list of blocks
reg_blocks = list(1:3, 20:22, 24:26)

# define reflective indicators
reg_modes = rep("A", 3)
# run plspm analysis
reg_pls = plspm(satisfaction, reg_path, reg_blocks, modes = reg_modes)


# Stage #1  Step #1

# get the latent variable scores in data frame format
Scores = as.data.frame(reg_pls$scores)
# create the interaction term
Scores$Inter = Scores$Image * Scores$Satisfaction
# let's see how Scores look like
head(Scores, n = 5)


# Stage #1  Step #2

# regression analysis
reg = lm(Loyalty ~ Image + Inter + Satisfaction - 1, data = Scores)

# check the coefficients
reg$coefficients

# Inner Model 을 plotting 하기 위해서 아래와 같이 진행

# define path matrix
c1 = c(0, 0, 0, 0)
c2 = c(0, 0, 0, 0)
c3 = c(0, 0, 0, 0)
c4 = c(reg$coefficients, 0)
reg_path = rbind(c1, c2, c3, c4)
rownames(reg_path) = c("Image", "Inter", "Satisfaction", "Loyalty")
colnames(reg_path) = c("Image", "Inter", "Satisfaction", "Loyalty")
# plot
innerplot(reg_path, show.values = TRUE)
