# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: PLS Model in Higher Order Construct (상위 구성개념상에서의 경로분석)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: NFL data 
#  Two Step Approach (patch approach)

# installing plsdepot
# install.packages("plsdepot")

# load nipals
library(plsdepot)

# load offense dataset
data(offense)

# Step 1

# PCA of Rushing block
rush_pca = nipals(offense[, 1:3])
# print rush_pca
rush_pca

# get first component
rush1 = rush_pca$scores[, 1]

# PCA of Passing block
pass_pca = nipals(offense[, 4:6])
# first component
pass1 = pass_pca$scores[, 1]

# what do rush1 and pass1 look like?
head(cbind(rush1, pass1))


# Step 2

# dataset for two-step approach
off_twostep = cbind(offense[, c(7:8, 1:6)], rush1, pass1, offense[, 9:11])

# list of blocks
nfl_blocks2 = list(1:2, 3:5, 6:8, 9:10, 11:13)

# path matrix
n1 = c(0, 0, 0, 0, 0)
n2 = c(0, 0, 0, 0, 0)
n3 = c(0, 0, 0, 0, 0)
n4 = c(0, 1, 1, 0, 0)
n5 = c(1, 0, 0, 1, 0)
nfl_path = rbind(n1, n2, n3, n4, n5)

# vector modes
nfl_modes = c("B", "A", "A", "A", "A")

# apply plspm
nfl_pls2 = plspm(off_twostep, nfl_path, nfl_blocks2, modes = nfl_modes)

# plot path coeffs
plot(nfl_pls2)



# some traps beware of

# PCA of Rushing block
rush_prcomp = prcomp(offense[, 1:3], scale. = TRUE)
# select fisrt component
rush_pc1 = rush_prcomp$x[, 1]
# PCA of Passing block
pass_prcomp = prcomp(offense[, 4:6], scale. = TRUE)
# select fisrt component
pass_pc1 = pass_prcomp$x[, 1]

# compare nipals components versus prcomp components
head(cbind(rush1, pass1, rush_pc1, pass_pc1))


# another dataset for two-step approach
other_twostep = cbind(offense[,c(7:8, 1:6)], rush_pc1, pass_pc1,
                      offense[,9:11])
# apply plspm
other_pls2 = plspm(other_twostep, nfl_path, nfl_blocks2, modes = nfl_modes)

# plot path coeffs
plot(other_pls2)
