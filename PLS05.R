# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 7: Moderating Effects (조절변수)

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: Simplified Customer Satisfaction
#  Two-Stage Path Modeling Approach (이단계 경로모델 접근법)

# load package 'plspm'
library("plspm")

# get data satisfaction
data(satisfaction)

# Stage #1

# create path matrix
f1 = c(0, 0, 0)
f2 = c(0, 0, 0)
f3 = c(1, 1, 0)
first_path = rbind(f1, f2, f3)
rownames(first_path) = c("Image", "Satisfaction", "Loyalty")
# define list of blocks
first_blocks = list(1:3, 20:22, 24:26)
# define reflective indicators
first_modes = rep("A", 3)
# run plspm analysis
first_pls = plspm(satisfaction, first_path, first_blocks, modes = first_modes)

# Stage #2  Step #1

# get the latent variable scores in data frame format
Scores = as.data.frame(first_pls$scores)
# create the interaction term
Scores$Inter = Scores$Image * Scores$Satisfaction
# let's see how Scores look like
head(Scores, n = 5)

# setting graphical parameters
op = par(mfrow = c(2, 2), mar = c(4, 5, 2, 2), bty = "n")
# for each scores
for (j in 1:4)
{  
# calculate density
score.dens = density(Scores[,j])
# plot window but don't show the density
plot(score.dens, main = names(Scores)[j], xlab = "", type = "n")
# add polygon
polygon(score.dens$x, score.dens$y, col = "gray90", border = "gray80")
}
# reset deafult values of graphical parameters
par(op)


# Stage #2  Step #2

# create path matrix
two_path = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0),
                  nrow = 4, ncol = 4, byrow = TRUE)
rownames(two_path) = c("Image", "Inter", "Satisfaction", "Loyalty")
colnames(two_path) = c("Image", "Inter", "Satisfaction", "Loyalty")
# define list of blocks
two_blocks = list(1, 4, 2, 3)
# define reflective indicators
two_modes= rep("A", 4)
# run plspm analysis with bootstrap validation (200 resamples)
two_pls = plspm(Scores, two_path, two_blocks, modes = two_modes,
                boot.val = TRUE, br = 200)

# check bootstrap results
round(two_pls$boot$paths, 4)

# plot inner model
plot(two_pls)

