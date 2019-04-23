# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: PLS Model in Higher Order Construct (상위 구성개념상에서의 경로분석)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: NFL data 
#  Hybrid Approach (The Give Away approach)

# installing plsdepot
# install.packages("plsdepot")

# load nipals
library(plsdepot)

# load offense dataset
data(offense)

# path matrix
n1 = c(0, 0, 0, 0, 0)
n2 = c(0, 0, 0, 0, 0)
n3 = c(0, 0, 0, 0, 0)
n4 = c(0, 1, 1, 0, 0)
n5 = c(1, 0, 0, 1, 0)
nfl_path = rbind(n1, n2, n3, n4, n5)

# vector modes
nfl_modes = c("B", "A", "A", "A", "A")

# redefine list of blocks
nfl_blocks3 = list(7:8, c(1, 3), c(4, 6), c(2, 5), 9:11)

# apply plspm
nfl_pls3 = plspm(offense, nfl_path, nfl_blocks3, modes = nfl_modes)

# plot path coeffs
plot(nfl_pls3)

# Wrapping up

# effects of nfl_pls1
nfl_pls1$effects

# useful row indices of effects
aux = c(4, 6, 8, 10)
# select desired path coefficients of each approach
paths1 = nfl_pls1$effects[aux, 2]
paths2 = nfl_pls2$effects[aux, 2]
paths3 = nfl_pls3$effects[aux, 2]
# put them in a matrix
nfl_paths = cbind(paths1, paths2, paths3)
rownames(nfl_paths) = nfl_pls1$effects[aux, 1]
# inspect nfl_paths
nfl_paths

# barplot
barplot(t(nfl_paths), beside=TRUE, border=NA, ylim=c(0,1), axes=FALSE,
        # legend
        legend.text = c("Repeat", "2-step", "hybrid"),
        args.legend=list(x="top", title="Approach", bty="n", ncol=3))
# add y-axis
axis(side = 2, las = 2)
