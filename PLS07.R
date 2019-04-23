# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: Moderating Effects (조절변수)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: Simplified Customer Satisfaction
#  Categorical Variable Approach (범주변수 접근법)

# load package 'plspm'
library("plspm")

# get data satisfaction
data(satisfaction)

# Stage #1

# create a categorical variable
categorical = gl(n = 3, k = 80, length = 250)
# quick look at categorical
table(categorical)


# Stage #2

# initialize dummy variables
dummy1 = rep(0, 250)
dummy2 = rep(0, 250)
# populate dummy variables
dummy1[categorical == 1] = 1
dummy2[categorical == 2] = 1


# Stage #3

# selected indicators from satisfaction
satisfaction2 = satisfaction[, c(20:22, 24:26)]
# add dummy variables to satisfaction2
satisfaction2$dummy1 = dummy1
satisfaction2$dummy2 = dummy2
# add product terms to satisfaction2
satisfaction2$sat1m1 = satisfaction2$sat1 * dummy1
satisfaction2$sat2m1 = satisfaction2$sat2 * dummy1
satisfaction2$sat3m1 = satisfaction2$sat3 * dummy1
satisfaction2$sat1m2 = satisfaction2$sat1 * dummy2
satisfaction2$sat2m2 = satisfaction2$sat2 * dummy2
satisfaction2$sat3m2 = satisfaction2$sat3 * dummy2


# Stage #4

# path matrix
c1 = c(0, 0, 0, 0, 0, 0)
c2 = c(0, 0, 0, 0, 0, 0)
c3 = c(0, 0, 0, 0, 0, 0)
c4 = c(0, 0, 0, 0, 0, 0)
c5 = c(0, 0, 0, 0, 0, 0)
c6 = c(1, 1, 1, 1, 1, 0)
cat_path = rbind(c1, c2, c3, c4, c5, c6)
rownames(cat_path) = c("Satis", "M1", "SatisM1", "M2", "SatisM2", "Loyalty")
# blocks of outer model
cat_blocks = list(1:3, 7, 9:11, 8, 12:14, 4:6)
# vector of modes
cat_modes = rep("A", 6)
# apply plspm with bootstrap validation
cat_pls = plspm(satisfaction2, cat_path, cat_blocks, modes = cat_modes,
                boot.val = TRUE)
# plot inner model
plot(cat_pls)


# Stage #5

# bootstrap results
round(cat_pls$boot$paths, 4)
