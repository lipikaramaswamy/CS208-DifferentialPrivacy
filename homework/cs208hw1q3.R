## CS208 
## Q3 - Membership attack on PUMS


# rm(list=ls())
## Import packages

library(caret)
library(plyr)
library(dplyr)


### READ IN DATA

setwd("/Users/lipikaramaswamy/Documents/Harvard/CS208/cs208_lr/")
sample.full <- read.csv("data/FultonPUMS5sample100.csv")
population.full <- read.csv('data/FultonPUMS5full.csv')

sample = select(sample.full, sex,  latino, black, asian, married, #age, educ,
                        divorced,children,disability,militaryservice,employed,englishability)
population = select(population.full, sex, latino, black, asian, married, #age, educ, 
                    divorced,children,disability,militaryservice,employed,englishability)

## Get p
population.mean = as.vector(colMeans(population, na.rm = FALSE, dims = 1))

## Get sample mean from query (baseline, no noise)
sample.mean = as.vector(colMeans(sample, na.rm = FALSE, dims = 1))


