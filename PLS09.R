# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: PLS Model in Higher Order Construct (상위 구성개념상에서의 경로분석)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: NFL data 
#  Repeated Indicators Approach (poor man’s approach)

# load package 'plspm'
library("plspm")


# load offense dataset
data(offense)
# let's take a peek
head(offense)

# path matrix
n1 = c(0, 0, 0, 0, 0)
n2 = c(0, 0, 0, 0, 0)
n3 = c(0, 0, 0, 0, 0)
n4 = c(0, 1, 1, 0, 0)
n5 = c(1, 0, 0, 1, 0)
nfl_path = rbind(n1, n2, n3, n4, n5)
# adding row and column names
rownames(nfl_path) = c("Special", "Rushing", "Passing", "Offense", "Scoring")
colnames(nfl_path) = rownames(nfl_path)
# list of blocks
nfl_blocks = list(7:8, 1:3, 4:6, 1:6, 9:11)

# vector modes

nfl_modes = c("B", "A", "A", "A", "A")
# apply plspm
nfl_pls1 = plspm(offense, nfl_path, nfl_blocks, modes = nfl_modes)

# plot path coeffs
plot(nfl_pls1)

