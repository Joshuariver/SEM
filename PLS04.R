# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: Moderating Effects (조절변수)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: Simplified Customer Satisfaction

# load package 'plspm'
library("plspm")

# Step 1: plspm 라이브러리를 열고 satisfaction 데이터를 가져오기

# get data satisfaction
data(satisfaction)

# duplicate satisfaction as satisfaction1
satisfaction1 = satisfaction
# how many columns in satisfaction1?
ncol(satisfaction1)

# create product indicator terms between Image and Satisfaction
satisfaction1$inter1 = satisfaction$imag1 * satisfaction$sat1
satisfaction1$inter2 = satisfaction$imag1 * satisfaction$sat2
satisfaction1$inter3 = satisfaction$imag1 * satisfaction$sat3
satisfaction1$inter4 = satisfaction$imag2 * satisfaction$sat1
satisfaction1$inter5 = satisfaction$imag2 * satisfaction$sat2
satisfaction1$inter6 = satisfaction$imag2 * satisfaction$sat3
satisfaction1$inter7 = satisfaction$imag3 * satisfaction$sat1
satisfaction1$inter8 = satisfaction$imag3 * satisfaction$sat2
satisfaction1$inter9 = satisfaction$imag3 * satisfaction$sat3

# check again the number of columns in satisfaction1
ncol(satisfaction1)


# Step 2: PLS inner model 을 만들고, outer model list 를 만들기 위한 block 을 지정하고, plspm 함수를
# 사용하여 경로분석을 실시함.

# create path matrix
r1 = c(0, 0, 0, 0)
r2 = c(0, 0, 0, 0)
r3 = c(0, 0, 0, 0)
r4 = c(1, 1, 1, 0)
prod_path = rbind(r1, r2, r3, r4)
rownames(prod_path) = c("Image", "Inter", "Satisfaction", "Loyalty")
colnames(prod_path) = c("Image", "Inter", "Satisfaction", "Loyalty")
# define outer model list
prod_blocks = list(1:3, 29:37, 20:22, 24:26)
# define reflective indicators
prod_modes = rep("A", 4)
# run plspm analysis with bootstrap validation
prod_pls = plspm(satisfaction1, prod_path, prod_blocks, modes = prod_modes,
                 boot.val = TRUE, br = 200)



# Step 3: plspm 을 실행한 후 경로계수 (path coefficient)를 확인함.

# check path coefficients
prod_pls$path_coefs

# plot inner model
plot(prod_pls)

# 경로계수의 유의성을 판단하기 위해 bootstrapping 을 수행함.
# check bootstrapped path coefficients
prod_pls$boot$paths


# Even though Inter has a negative effect on Loyalty, its associated bootstrap confidence
# interval contains the zero, having a non-significant effect. This means that the moderating
# effect of Inter on the relation between Satisfaction and Loyalty is not significant.