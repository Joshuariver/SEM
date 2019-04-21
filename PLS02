# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 5: Running a PLS-PM Analysis

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Modeling Customer Satifaction
# ESCI (European Customer Saatisfaction Model) by EU

# Image:  refers to the brand name and the kind of associations customers get from the product/brand/company. 
# It is expected that image will have a positive effect on customer satisfaction and loyalty. 
# In addition, image is also expected to have a direct effect on expectations.

# Expectations: is the information based on, not actual consumption experienced but,accumulated information 
# about quality from outside sources, such as advertising, word of mouth, and general media.

# Perceived Quality:  comprises product quality (hardware) and service quality (software/humanware). 
# Perceived product quality is the evaluation of recent consumption experience of products. 
# Perceived service quality is the evaluation of recent consumption experience of associated services 
# like customer service, conditions of product display, range of services and products, etc. 
# Perceived quality is expected to affect satisfaction.

# Perceived Value:  is the perceived level of product quality relative to the price paid of the \value for the money" 
# aspect of the customer experience.

# Satisfaction:  is defined as an overall evaluation of a firm’s post-purchase performance or 
# utilization of a service.

# Complaints:  implies the complaints of customers

# Loyalty:  refers to the intention repurchase and price tolerance of customers. It is the ultimate dependent 
# variable in the model and it is expected that the better image and higher customer satisfaction should 
# increase customer loyalty

# read "education.txt"
education = read.table("education.txt", header = TRUE, row.names = 1)

# read "education.csv"
education = read.csv("education.csv", header = TRUE, row.names = 1)

# how many rows and columns?
dim(education)

# summarized statistics of the first 20 columns
summary(education[, 1:20])

# 각 변수별 하위변수 정의

# Support
# sup.help I feel comfortable asking for help from the program’s staff
# sup.under I feel underappreciated in the program
# sup.safe I can find a place where I feel safe in the program
# sup.conc I go to the program when I have concerns about school

# Advising
# adv.comp Competence of advisors
# adv.acces Access to advisors
# adv.comm Communication skills of advisors
# adv.qual Overall quality of advising

# Tutoring
# tut.prof Proficiency of tutors
# tut.sched Tutoring schedules
# tut.stud Variety of study groups
# tut.qual Overall quality of tutoring

# Value
# val.devel Helpfulness in my personal development
# val.deci Helpfulness in personal decision making
# val.meet Facilitating meeting people and contacts
# val.info Accessibility to support and information

# Satisfaction
# sat.glad I’m glad to be a member of the program
# sat.expe The program meets my expectations
# sat.over Overall, I’m very satisfied with the program

# Loyalty
# loy.proud I’m proud to tell others I’m part of the program
# loy.recom I would recommend the program to my colleagues
# loy.asha I often feel ashamed of being a member of the program
# loy.back I’m interested in giving something back to the program

# 7점 척도.

# distribution of first column
aux_distrib = table(education[, 1])/nrow(education)
# barplot of the distribution
barplot(aux_distrib, border = NA, main = colnames(education)[1])

# package RColorBrewer (for nice colors)
library(RColorBrewer)

# questions of Support indicators
sq1 = "Help when not doing well"
sq2 = "I feel underappreciated"
sq3 = "I can find a place where I feel safe"
sq4 = "Concerns about school"
# put questions in one vector
sup_questions = c(sq1, sq2, sq3, sq4)

# setting graphical parameters
op = par(mfrow = c(2,2), mar = c(2.5, 3.2, 2, 0.8))
# bar-chart for each indicator of Support
for (j in 1:4) {
distribution = table(education[,j]) / nrow(education)
barplot(distribution, border = NA, col = brewer.pal(8, "Blues")[2:8],
        axes = FALSE, main = sup_questions[j], cex.main = 1)
# add vertical axis, and rectangle around figure
axis(side = 2, las=2)
box("figure", col="gray70")
}
# reset default graphical parameters
par(op)

# correlations of Support indicators
cor(education[, 1:4])

# load plsdepot
# install.packages("plsdepot")
library(plsdepot)
# PCA of Support indicators with nipals
support_pca = nipals(education[,1:4])
# plot
plot(support_pca, main = "Support indicators (circle of correlations)",
     cex.main = 1)

# rows of path matrix
Support = c(0, 0, 0, 0, 0, 0)
Advising = c(0, 0, 0, 0, 0, 0)
Tutoring = c(0, 0, 0, 0, 0, 0)
Value = c(1, 1, 1, 0, 0, 0)
Satisfaction = c(1, 1, 1, 1, 0, 0)
Loyalty = c(0, 0, 0, 0, 1, 0)
# matrix (by row binding)
edu_path = rbind(Support, Advising, Tutoring, Value, Satisfaction, Loyalty)
# add column names (optional)
colnames(edu_path) = rownames(edu_path)

# plot the inner matrix
innerplot(edu_path, box.size = 0.1)

# outer model
edu_blocks = list(1:4, 5:8, 9:12, 13:16, 17:19, 20:23)
# modes (reflective blocks)
edu_modes = rep("A", 6)

# apply plspm
edu_pls1 = plspm(education, edu_path, edu_blocks, modes = edu_modes)

# print edu_pls1
edu_pls1

# check unidimensionality
edu_pls1$unidim

# plotting the loadings
plot(edu_pls1, what = "loadings")


# Round 2

# adding Support 'appreciated'
education$sup.appre = 8 - education$sup.under
# adding 'Loyalty' pleased
education$loy.pleas = 8 - education$loy.asha

# outer model 2
edu_blocks2 = list(c(1, 27, 3, 4), 5:8, 9:12, 13:16, 17:19, c(20, 21, 28, 23))

# apply plspm
edu_pls2 = plspm(education, edu_path, edu_blocks2, modes = edu_modes)

# plotting the loadings
plot(edu_pls2, what = "loadings")

# check unidimensionality
edu_pls2$unidim

# check outer model
edu_pls2$outer_model

# load ggplot2
library(ggplot2)
# barchart of loadings
ggplot(data = edu_pls2$outer_model,
       aes(x = name, y = loading, fill = block)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  # threshold line (to peek acceptable loadings above 0.7)
  geom_hline(yintercept = 0.7, color = 'gray50') +
  # add title
  ggtitle("Barchart of Loadings") +
  # rotate x-axis names
  theme(axis.text.x = element_text(angle = 90))


# Round 3

# outer model 3
edu_blocks3 = list(c(1, 3, 4), 5:8, 9:12, 13:16, 17:19, c(20, 21, 23))
# re-apply plspm
edu_pls3 = plspm(education, edu_path, edu_blocks3, modes = edu_modes)

# check unidimensionality
edu_pls3$unidim

# check outer model
edu_pls3$outer_model

# check cross loadings
edu_pls3$crossloadings

# plotting results (inner model)
plot(edu_pls3)

# matrix of path coefficients
edu_pls3$path_coefs

# plotting results (inner model)
plot(edu_pls3, arr.pos = 0.35)

# matrix of path coefficients
Paths = edu_pls3$path_coefs
# matrix with values based on path coeffs
arrow_lwd = 10 * round(Paths, 2)
# how does it look like?
arrow_lwd

# arrows of different sizes reflecting the values of the path coeffs
plot(edu_pls3, arr.pos = 0.35, arr.lwd = arrow_lwd)

# inner model
edu_pls3$inner_model

# effects
edu_pls3$effects

# selecting effects ('active' rows)
good_rows = c(3:5, 7:15)
# 'active' effects in matrix format
path_effs = as.matrix(edu_pls3$effects[good_rows, 2:3])

# add rownames to path_effs
rownames(path_effs) = edu_pls3$effects[good_rows, 1]

# how does path_effs look like?
path_effs

# setting margin size
op = par(mar = c(8, 3, 1, 0.5))
# barplots of total effects (direct + indirect)
barplot(t(path_effs), border = NA, col = c("#9E9AC8", "#DADAEB"),
        las = 2, cex.names = 0.8, cex.axis = 0.8,
        legend = c("Direct", "Indirect"),
        args.legend = list(x = "top", ncol = 2, border = NA,
                           bty = "n", title = "Effects"))
# resetting default margins
par(op)

# Inner Model Summary
edu_pls3$inner_summary

# gof index
edu_pls3$gof

# running bootstrap validation
edu_val = plspm(education, edu_path, edu_blocks3, modes = edu_modes,
                boot.val = TRUE, br = 200)
# bootstrap results
edu_val$boot

# bootstrap path coefficients
edu_val$boot$paths

# summary of latent variable scores
summary(edu_pls3$scores)

# setting graphic layout and margin sizes
op = par(mfrow = c(2, 3), mar = c(4, 5, 2, 0.5))
# for each score
for (j in 1:6) {
# histogram (with probability density)
hist(edu_pls3$scores[, j], freq = FALSE, xlab = "", border = "#6A51A3",
     col = "#DADAEB", main = colnames(edu_pls3$scores)[j])
# add axes
axis(side = 1, col = "gray70", col.axis = "gray70")
axis(side = 2, col = "gray70", col.axis = "gray70")
}
par(op)

# rescaling scores
Scores = rescale(edu_pls3)
# summary
summary(Scores)

# setting graphic layout and margin sizes
op = par(mfrow = c(2,3), mar = c(4, 5, 2, 1.5), bty = "n")
# for each score
for (j in 1:6) {  
# histogram (not showing axes)
hist(Scores[,j], main = colnames(Scores)[j], axes = FALSE,
     xlim = c(1,7), ylim = c(0, 125), xlab = "",
     border = "#6A51A3", col = "#DADAEB")
# add horizontal axis
axis(side = 1, col = "gray70", col.axis = "gray70")
# add vertical axis
axis(side = 2, col = "gray70", col.axis = "gray70", las = 2)
}
# resetting default graphical parameters
par(op)

# scatter plots of rescaled scores
pairs(Scores, pch = 19, cex = 0.7, col = "#807DBA33", cex.axis = 0.8,
      col.axis = "gray70", gap = 0.5)
