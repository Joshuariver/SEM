# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# installing colortools (this is a comment!)
# install.packages("colortools")

# loading colortools (this is another comment!)
library(colortools)

# installing the package 'plspm'
# install.packages("plspm")

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

# unidimensionality
foot_pls$unidim

# cronbach's alpha
foot_pls$unidim[, 3, drop = FALSE]

# dillon-goldstein rho
foot_pls$unidim[, 4, drop = FALSE]

# eigenvalues
foot_pls$unidim[, 5:6]

# plotting loadings
plot(foot_pls, what = "loadings")

# outer model results
foot_pls$outer_model

# Defense outer model results
subset(foot_pls$outer_model, block == "Defense")

# plotting weights
plot(foot_pls, what = "weights")

# defense loading 값이 음의 값을 보이므로 이를 양의 값으로 전환함.
# add two more columns NGCH and NGCA
spainfoot$NGCH = -1 * spainfoot$GCH
spainfoot$NGCA = -1 * spainfoot$GCA

# check column names
names(spainfoot)

# new list of blocks (with column positions of variables)
new_blocks_pos = list(1:4, c(15,16,7,8), 9:12)

# new list of blocks (with names of variables)
new_blocks_str = list(
  c("GSH", "GSA", "SSH", "SSA"),
  c("NGCH", "NGCA", "CSH", "CSA"),
  c("WMH", "WMA", "LWR", "LRWL"))

# re-apply plspm
foot_pls = plspm(spainfoot, foot_path, new_blocks_str, modes = foot_modes)
# plot loadings
plot(foot_pls, "loadings")

# unidimensionality
foot_pls$unidim

# loadings and communalities
foot_pls$outer_model

# cross-loadings
foot_pls$crossloadings

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("Crossloadings")

# inner model
foot_pls$inner_model

# inner model summary
foot_pls$inner_summary

# select R2
foot_pls$inner_summary[, "R2", drop = FALSE]

# R Square의 해석. (절대적인 것은 아님)
# 1. Low: R < 0:30 (although some authors consider R < 0:20)
# 2. Moderate: 0:30 < R < 0:60 (you can also find 0:20 < R < 0:50)
# 3. High: R > 0:60 (alternatively there’s also R > 0:50)

# inner model summary
foot_pls$inner_summary


# The GoF index is a pseudo Goodness of fit measure that accounts for the model quality at
# both the measurement and the structural models. GoF is calculated as the geometric mean of
# the average communality and the average R2 value. Since it takes in to account communality,
# this index is more applicable to reflective indicators than to formative indicators. 

# a GoF value of 0.78 could be interpreted as if the prediction power of the model is of 78%. 
# The naive rule of thumb is: the higher, the better. Acceptable \good" values within the PLS-PM 
# community are GoF >0.7

# gof index
foot_pls$gof


# Bootstrapping
# 
# running bootstrap validation
foot_val = plspm(spainfoot, foot_path, new_blocks_str, modes = foot_modes,
                 boot.val = TRUE, br = 200)

# bootstrap results
foot_val$boot

# What we obtain in foot val$boot is a list with results for:
# • the outer weights (foot val$boot$weigts)
# • the loadings (foot val$boot$loadings)
# • the path coefficients (foot val$boot$paths)
# • the R2 (foot val$boot$rsq)
# • the total effects (foot val$boot$total.efs)


