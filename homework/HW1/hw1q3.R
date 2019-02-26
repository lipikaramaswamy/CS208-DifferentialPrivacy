## CS208 
## Q3 - Membership attack on PUMS

rm(list=ls())

## Import packages
library(caret)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

### GET DATA AND PREPARE IT
setwd("/Users/lipikaramaswamy/Documents/Harvard/CS208/cs208_lr/")

## read in full datasets
sample.full <- read.csv("data/FultonPUMS5sample100.csv")

## select only PUB cols
sample = select(sample.full, sex, latino, black, asian, married, age, educ,
                divorced,children,disability,militaryservice,employed,englishability)

## hash random predicates
P <- 105701
list.of.rs <- replicate(10000, sample(0:(P-1),size=13), simplify=TRUE)

## make 10000 attributes in sample
mat.of.obs.samp = as.matrix(sample)
new.samp = (mat.of.obs.samp %*% list.of.rs) %% P %% 2

# population means would be 0 given how the hash works
population.mean = rep(0, 10000)

### FUNCTIONS

## query the dataset and return sample means with the three defense mechanisms (rounding, gaussian noise, subsampling)
membershipQuery <- function(samp, 
                            rounding = FALSE, R, 
                            gaussian = FALSE, gaussian.sigma, 
                            subsample = FALSE, subsample.size){
  sample.sum.true = as.vector(colSums(samp, na.rm = FALSE, dims = 1))

  ## round if specified
  if (rounding == TRUE){
    round <- round_any(sample.sum.true, R)
    means <- round /100
    means <- 2*(means-0.5)
    return(means)
  }
  
  ## add gaussian noise if specified
  if (gaussian == TRUE){
    noisy <- sample.sum.true + rnorm(1,0,(gaussian.sigma))
    means <- noisy / 100
    means <- 2 * (means - 0.5)
    return(means)
  }
  
  ## subsample and scale query result if specified
  if (subsample == TRUE){
    subsample.index <- sample(x=1:nrow(samp), size=subsample.size, replace = FALSE)
    subset <- samp[subsample.index,]
    means <- as.vector(colMeans(subset, na.rm = FALSE, dims = 1))
    means <- 2*(means-0.5)
    return(means)
  }
}

# Dwork et al. test statistic using population means
test.Dwork <- function(alice, sample.mean, population.mean){
  test.statistic <- sum(alice * sample.mean) - sum(population.mean * sample.mean)
  return(test.statistic)
}

## A utility function to create data from the population
rmvbernoulli <- function(n=1, prob){
  history <- matrix(NA, nrow=n, ncol=length(prob))
  for(i in 1:n){
    x<- rbinom(n=length(prob), size=1, prob=prob)
    x[x==0] <- -1      								# Transform from {0,1} to {-1,1}
    history[i,] <- x
  }
  return(history)
}

#### PARAMETERS
n.attributes = ncol(sample.augmented)
my.alpha <- 0.001
list.of.parameters <- seq(from = 10, to = 10000, by = 50)

#### ROUNDING
true.pos.rounding <- matrix(NA, nrow = length(list.of.parameters), ncol = 1)
sample.mean.rounding = membershipQuery(new.samp, rounding = TRUE, R = 10)

i = 1
for(parameters in list.of.parameters){
  cat("\nin rounding loop, number of parameters is now ", parameters)
  sample.mean <- sample.mean.rounding[1:parameters]
  pop.prob <- population.mean[1:parameters]
  calc.variance <- sum((sample.mean)^2 * (1-(pop.prob)^2))
  crit.val <- qnorm(my.alpha, mean = 0, sd = sqrt(calc.variance), lower.tail = FALSE, log.p = FALSE)
  
  history.rounding <- matrix(NA, nrow = 100, ncol = 2)
  # print(calc.variance, crit.val, nullDist.Dwork$criticalVal)
  for(row in 1:100){
    ## get real Alice from the sample and scale her to {-1,+1}
    alice <- new.samp[row,1:parameters]
    alice[alice==0] <- -1
    
    ## get the test stat for Alice
    test.alice.Dwork <- test.Dwork(alice=alice, sample.mean=sample.mean, population.mean=pop.prob)
    history.rounding[row,1]<-test.alice.Dwork
    history.rounding[row,2]<-test.alice.Dwork>crit.val
  }
  true.pos.rounding[i, 1] = sum(history.rounding[,2]/nrow(history.rounding))
  i = i + 1
}

rounding.for.plots = data.frame(list.of.parameters , true.pos.rounding)

p1 <- ggplot(rounding.for.plots, aes(x = list.of.parameters, y = true.pos.rounding)) + 
  geom_point() +
  labs(x = "Number of attributes", y = 'True positive probability', title = 
         'True positive probability for different number of attributes\n(Rounding sample means, R = 10)') + 
  theme(plot.title = element_text(hjust = 0.5))

#### ROUNDING
true.pos.rounding <- matrix(NA, nrow = length(list.of.parameters), ncol = 1)
sample.mean.rounding = membershipQuery(new.samp, rounding = TRUE, R = 100)

i = 1
for(parameters in list.of.parameters){
  cat("\nin rounding loop, number of parameters is now ", parameters)
  sample.mean <- sample.mean.rounding[1:parameters]
  pop.prob <- population.mean[1:parameters]
  calc.variance <- sum((sample.mean)^2 * (1-(pop.prob)^2))
  crit.val <- qnorm(my.alpha, mean = 0, sd = sqrt(calc.variance), lower.tail = FALSE, log.p = FALSE)
  
  history.rounding <- matrix(NA, nrow = 100, ncol = 2)
  # print(calc.variance, crit.val, nullDist.Dwork$criticalVal)
  for(row in 1:100){
    ## get real Alice from the sample and scale her to {-1,+1}
    alice <- new.samp[row,1:parameters]
    alice[alice==0] <- -1
    
    ## get the test stat for Alice
    test.alice.Dwork <- test.Dwork(alice=alice, sample.mean=sample.mean, population.mean=pop.prob)
    history.rounding[row,1]<-test.alice.Dwork
    history.rounding[row,2]<-test.alice.Dwork>crit.val
  }
  true.pos.rounding[i, 1] = sum(history.rounding[,2]/nrow(history.rounding))
  i = i + 1
}

### plots for rounding 
rounding.for.plots = data.frame(list.of.parameters , true.pos.rounding)

p11 <- ggplot(rounding.for.plots, aes(x = list.of.parameters, y = true.pos.rounding)) + 
  geom_point() +
  labs(x = "Number of attributes", y = 'True positive probability', title = 
         'True positive probability for different number of attributes\n(Rounding sample means, R = 100)') + 
  theme(plot.title = element_text(hjust = 0.5))


plot.rounding = grid.arrange(p1, p11, nrow = 2, ncol = 1)


ggsave(filename = 'q3_rounding_R_both.pdf', plot = plot.rounding, width = 11, height = 10, units = 'in')



### Gaussian noise

true.pos.gaussian <- matrix(NA, nrow = length(list.of.parameters), ncol = 1)
sample.mean.gaussian = membershipQuery(new.samp, gaussian = TRUE, gaussian.sigma = 10)

i = 1
for(parameters in list.of.parameters){
  
  cat("\nin rounding loop, number of parameters is now ", parameters)
  sample.mean <- sample.mean.gaussian[1:parameters]
  pop.prob <- population.mean[1:parameters]
  calc.variance <- sum((sample.mean)^2 * (1-(pop.prob)^2))
  crit.val <- qnorm( my.alpha, mean = 0, sd = sqrt(calc.variance), lower.tail = FALSE, log.p = FALSE)
  
  history.gaussian <- matrix(NA, nrow = 100, ncol = 2)
  
  for(row in 1:100){
    ## get real Alice from the sample
    alice <- new.samp[row,1:parameters]
    alice[alice==0] <- -1
    
    ## get the test stat for Alice
    test.alice.Dwork <- test.Dwork(alice=alice, sample.mean=sample.mean, population.mean=pop.prob)
    history.gaussian[row,1]<-test.alice.Dwork
    history.gaussian[row,2]<-test.alice.Dwork>crit.val
  }
  true.pos.gaussian[i, 1] = sum(history.gaussian[,2]/nrow(history.gaussian))
  i = i + 1
}

## gaussian plots
gaus.for.plots = data.frame(list.of.parameters , true.pos.gaussian)

p2 <- ggplot(gaus.for.plots, aes(x = list.of.parameters, y = true.pos.gaussian)) + 
  geom_point() +
  labs(x = "Number of attributes", y = 'True Positive Probability', title = 
         'True positive probability for different number of attributes\nGaussian noise added to sample means') + 
  theme(plot.title = element_text(hjust = 0.5))


true.pos.gaussian <- matrix(NA, nrow = length(list.of.parameters), ncol = 1)
sample.mean.gaussian = membershipQuery(new.samp, gaussian = TRUE, gaussian.sigma = 40)

i = 1
for(parameters in list.of.parameters){
  
  cat("\nin rounding loop, number of parameters is now ", parameters)
  sample.mean <- sample.mean.gaussian[1:parameters]
  pop.prob <- population.mean[1:parameters]
  calc.variance <- sum((sample.mean)^2 * (1-(pop.prob)^2))
  crit.val <- qnorm( my.alpha, mean = 0, sd = sqrt(calc.variance), lower.tail = FALSE, log.p = FALSE)
  
  history.gaussian <- matrix(NA, nrow = 100, ncol = 2)
  
  for(row in 1:100){
    ## get real Alice from the sample
    alice <- new.samp[row,1:parameters]
    alice[alice==0] <- -1
    
    ## get the test stat for Alice
    test.alice.Dwork <- test.Dwork(alice=alice, sample.mean=sample.mean, population.mean=pop.prob)
    history.gaussian[row,1]<-test.alice.Dwork
    history.gaussian[row,2]<-test.alice.Dwork>crit.val
  }
  true.pos.gaussian[i, 1] = sum(history.gaussian[,2]/nrow(history.gaussian))
  i = i + 1
}

## gaussian plots
gaus.for.plots = data.frame(list.of.parameters , true.pos.gaussian)

p22 <- ggplot(gaus.for.plots, aes(x = list.of.parameters, y = true.pos.gaussian)) + 
  geom_point() +
  labs(x = "Number of attributes", y = 'True Positive Probability', title = 
         'True positive probability for different number of attributes\nGaussian noise added to sample means') + 
  theme(plot.title = element_text(hjust = 0.5))

plots.gaus = grid.arrange(p2, p22,  nrow = 2, ncol = 1)

ggsave(filename = 'q3_gaussian_sigma_all.pdf', plot = plots.gaus, width = 11, height = 5, units = 'in')


### Subsampling 

true.pos.subsamp <- matrix(NA, nrow = length(list.of.parameters), ncol = 1)
sample.mean.subsamp = membershipQuery(new.samp, subsample = TRUE, subsample.size = 2)

i = 1
for(parameters in list.of.parameters){
  
  cat("\nin rounding loop, number of parameters is now ", parameters)
  
  sample.mean <- sample.mean.subsamp[1:parameters]
  pop.prob <- population.mean[1:parameters]
  calc.variance <- sum((sample.mean)^2 * (1-(pop.prob)^2))
  crit.val <- qnorm(my.alpha, mean = 0, sd = sqrt(calc.variance), lower.tail = FALSE, log.p = FALSE)
  
  history.subsamp <- matrix(NA, nrow = 100, ncol = 2)
  
  for(row in 1:100){
    ## get real Alice from the sample
    alice <- new.samp[row,1:parameters]
    alice[alice==0] <- -1
    
    ## get the test stat for Alice
    test.alice.Dwork <- test.Dwork(alice=alice, sample.mean=sample.mean, population.mean=pop.prob)
    
    history.subsamp[row,1]<-test.alice.Dwork
    history.subsamp[row,2]<-test.alice.Dwork>crit.val
  }
  true.pos.subsamp[i, 1] = sum(history.subsamp[,2]/nrow(history.subsamp))
  i = i+1
}



## subsampling plots
subsamp.for.plots = data.frame(list.of.parameters , true.pos.subsamp)

p3 <- ggplot(subsamp.for.plots, aes(x = list.of.parameters, y = true.pos.subsamp)) + 
  geom_point() +
  labs(x = "Number of attributes", y = 'True Positive Probability', title = 
         'True positive probability for different number of attributes\nsubsampling (t=2)') + 
  theme(plot.title = element_text(hjust = 0.5))



true.pos.subsamp <- matrix(NA, nrow = length(list.of.parameters), ncol = 1)
sample.mean.subsamp = membershipQuery(new.samp, subsample = TRUE, subsample.size = 50)

i = 1
for(parameters in list.of.parameters){
  
  cat("\nin rounding loop, number of parameters is now ", parameters)
  
  sample.mean <- sample.mean.subsamp[1:parameters]
  pop.prob <- population.mean[1:parameters]
  calc.variance <- sum((sample.mean)^2 * (1-(pop.prob)^2))
  crit.val <- qnorm(my.alpha, mean = 0, sd = sqrt(calc.variance), lower.tail = FALSE, log.p = FALSE)
  
  history.subsamp <- matrix(NA, nrow = 100, ncol = 2)
  
  for(row in 1:100){
    ## get real Alice from the sample
    alice <- new.samp[row,1:parameters]
    alice[alice==0] <- -1
    
    ## get the test stat for Alice
    test.alice.Dwork <- test.Dwork(alice=alice, sample.mean=sample.mean, population.mean=pop.prob)
    
    history.subsamp[row,1]<-test.alice.Dwork
    history.subsamp[row,2]<-test.alice.Dwork>crit.val
  }
  true.pos.subsamp[i, 1] = sum(history.subsamp[,2]/nrow(history.subsamp))
  i = i+1
}


## subsampling plots
subsamp.for.plots = data.frame(list.of.parameters , true.pos.subsamp)

p31 <- ggplot(subsamp.for.plots, aes(x = list.of.parameters, y = true.pos.subsamp)) + 
  geom_point() +
  labs(x = "Number of attributes", y = 'True Positive Probability', title = 
         'True positive probability for different number of attributes\nsubsampling (t = 50)') + 
  theme(plot.title = element_text(hjust = 0.5))

plots.subsamp = grid.arrange(p3, p31,  nrow = 2, ncol = 1)
ggsave(filename = 'q3_subsamp_t_all.pdf', plot = plots.subsamp, width = 11, height = 10, units = 'in')

