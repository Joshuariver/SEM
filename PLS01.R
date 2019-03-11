rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# installing colortools (this is a comment!)
install.packages("colortools")

# loading colortools (this is another comment!)
library(colortools)

# installing the package 'plspm'
install.packages("plspm")

# load package 'plspm'
library("plspm")

# asking for help about function plspm
help(plspm)

# load data spainfoot
data(spainfoot)

# first 5 rows of spainfoot
head(spainfoot, n = 5)


# rows of the inner model matrix
Attack = c(0, 0, 0)
Defense = c(0, 0, 0)
Success = c(1, 1, 0)
# path matrix created by row binding
foot_path = rbind(Attack, Defense, Success)
# add column names (optional)
colnames(foot_path) = rownames(foot_path)

# let's see it
foot_path

# plot the path matrix
innerplot(foot_path)

# define list of indicators: what variables are associated with
# what latent variables
foot_blocks = list(1:4, 5:8, 9:12)

# all latent variables are measured in a reflective way
foot_modes = c("A", "A", "A")

# Success in formative mode B
foot_modes2 = c("A", "A", "B")

# run plspm analysis
foot_pls = plspm(spainfoot, foot_path, foot_blocks, modes = foot_modes)

# what's in foot_pls?
foot_pls

# what class of object is foot_pls?
class(foot_pls)

# path coefficients
foot_pls$path_coefs

# inner model
foot_pls$inner_model

# summarized results
summary(foot_pls)

# plotting results (inner model)
plot(foot_pls)

# plotting loadings of the outer model
plot(foot_pls, what = "loadings", arr.width = 0.1)


# show me the first scores
head(foot_pls$scores, n = 5)

# show me the last scores
tail(foot_pls$scores, n = 5)
