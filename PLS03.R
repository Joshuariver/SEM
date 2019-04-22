# Source: PLS Path Modeling with R
# by Gaston Sanchez
# www.gastonsanchez.com
# Chapter 6: Comparing Groups

rm(list=ls())
setwd("~/R/Structural Equation Modeling")

# Case Study: Colleage GPA

# only if you haven't load plspm
library(plspm)

# load data college
data(college)

# what does the data look like
head(college, n = 5)

# what's the structure?
str(college)

# Table 6.1: Description of variables in data college
# Variable Description

# High School Readiness
# - HS GPA High School GPA
# - SAT Verbal Verbal SAT score
# - SAT Math Math SAT score
# Introductory Courses
# - Biology1 Introductory Biology
# - Chemistry1 Introductory Chemistry
# - Math1 Calculus 1
# - Physics1 Introductory Physics
# Intermediate Courses
# - Biology2 Intermediate Biology
# - Chemistry2 Intermediate Chemistry
# - Math2 Calculus 2
# - Physics2 Intermediate Physics
# Graduation
# - FinalGPA Graduation GPA
# Gender Gender

# path matrix (inner model)
HighSchool = c(0, 0, 0, 0)
Intro = c(1, 0, 0, 0)
Medium = c(1, 1, 0, 0)
Graduation = c(1, 1, 1, 0)
gpa_path = rbind(HighSchool, Intro, Medium, Graduation)
# list of blocks (outer model)
gpa_blocks = list(1:3, 4:7, 8:11, 12)
# vector of reflective modes
gpa_modes = rep("A", 4)

# apply plspm
gpa_pls = plspm(college, gpa_path, gpa_blocks, modes = gpa_modes,
                boot.val = TRUE)
# plot path coefficients
plot(gpa_pls)

# bootstrapped path coefficients
gpa_pls$boot$paths

# select data of female students
female = college[college$Gender == "FEMALE", ]
# female students plspm
female_gpa_pls = plspm(female, gpa_path, gpa_blocks, modes = gpa_modes)

# select data of male students
male = college[college$Gender == "MALE", ]
# male students plspm
male_gpa_pls = plspm(male, gpa_path, gpa_blocks, modes = gpa_modes)

# plot path coefficients
plot(female_gpa_pls, box.size = 0.14)

plot(male_gpa_pls, box.size = 0.14)

# plspm.groups(pls, group, method, reps)

# Compairing Groups: Bootstrap t-test
# apply plspm.groups bootstrap
gpa_boot = plspm.groups(gpa_pls, college$Gender, method = "bootstrap")
# see the results
gpa_boot

# Compairing Groups: permutation Test
# apply plspm.groups premutation
gpa_perm = plspm.groups(gpa_pls, college$Gender, method = "permutation")
# see the results
gpa_perm

# path coefficients between female and male students
barplot(t(as.matrix(gpa_boot$test[,2:3])), border = NA, beside = TRUE,
        col = c("#FEB24C","#74A9CF"), las = 2, ylim = c(-0.1, 1),cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
# add horizontal line
abline(h = 0, col = "gray50")
# add itle
title("Path coefficients of Female and Male Students",
      cex.main = 0.95, col.main = "gray30")
# add legend
legend("top", legend = c("female", "male"), pt.bg = c("#FEB24C", "#A6BDDB"),
       ncol = 2, pch = 22, col = c("#FEB24C", "#74A9CF"), bty = "n",
       text.col = "gray40")


summary()