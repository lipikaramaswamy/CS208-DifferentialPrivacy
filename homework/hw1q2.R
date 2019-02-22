## CS208 
## Q2 - Reconstruction attack on PUMS

## import libraries
library(caret)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# ------------------------------------------------- #

#### Parameters ####

## number of queries
k.trials <- 200
## number of rows in dataset
n <- 100 

# ------------------------------------------------- #

### ALL THINGS DATA - Read in, subset, set aside sensitive data ###
setwd("/Users/lipikaramaswamy/Documents/Harvard/CS208/cs208_lr/")

pums <- read.csv("data/FultonPUMS5sample100.csv")

# subset data to that which is available to attacker
available.pums = select(pums, uscitizen, sex, age, educ, latino, black, asian, married,
                        divorced,children,disability,militaryservice,employed,englishability)

# make new column to contain randomly hashed values that determine membership to the random subsets
available.pums$subset.indicator<-NA

# set aside sensitive data:
sensitive.data <- pums[, "uscitizen"]

# ------------------------------------------------- #

### Setup to build random subsets of data ###

# Chose random large prime number
P = 491

# Make vector of all integers up to P
prime.options <- seq(from=0, to=P-1, by=1)

# ------------------------------------------------- #

### BUILD QUERY ###

runQuery <- function(df, rounding = FALSE, R = 0, gaussian = FALSE, gaussian.sigma = 0, subsample = FALSE, subsample.size = 1){
  
  ## random subset creation 
  r = sample(prime.options, size=13, replace = FALSE)
  mat.of.obs = as.matrix(available.pums[,2:14])
  p = (mat.of.obs %*% r) %% P %% 2
  df$subset.indicator = p

  ## subsetting and returning the sum
  subset = df[df$subset.indicator == 1,]
  sum <- sum(subset$uscitizen)
  index = as.numeric(rownames(subset))
  
  ## round if specified
  if (rounding == TRUE){
    round <- round_any(sum, R)
    return(list(sum=round, index=index, truesum = sum))
  }

  ## add gaussian noise if specified
  if (gaussian == TRUE){

    noisy <- sum + rnorm(1,0,(gaussian.sigma))
    return(list(sum=noisy, index=index, truesum = sum))
  }
  
  ## subsample and scale query result if specified
  if (subsample == TRUE){
    
    subsample.index <- sample(x=1:nrow(df), size=subsample.size, replace = FALSE)
    subset <- df[subsample.index,]
    subsetsum <- sum(subset$subset.indicator * subset$uscitizen) * (nrow(df)/subsample.size)
    return(list(sum=subsetsum, index=index, truesum = sum))
  }
  
  ## if none of the defense mechanisms are specified, return true sum
  if (rounding == FALSE & rounding == FALSE & gaussian == FALSE){
    return(list(index = index, truesum = sum))
  }
}

# ------------------------------------------------- #

### RUNNING ATTACKS - MULTIPLE PARAMETERS, MULTIPLE EXPERIMENTS ### 

## set varnames
xnames <- paste("x", 1:n, sep="")
varnames<- c("y", xnames)

## Make formula
formula <- paste(xnames, collapse=" + ")
formula <- paste("y ~ ", formula, "-1")
formula <- as.formula(formula)

## matrix to contain results of the queries and indices
history.rounding <- matrix(NA, nrow=k.trials, ncol=100+2)
history.gaussian <- matrix(NA, nrow=k.trials, ncol=100+2)
history.subsamp <- matrix(NA, nrow=k.trials, ncol=100+2)

## set parameter ranges to loop thru
parameter.range <- seq(from=1, to=100, by=1)
RMSE.matrix <- matrix (, nrow = 100, ncol = 10)
acc.matrix <- matrix (, nrow = 100, ncol = 10)

for(b in 1:10){
  
  for(a in parameter.range){
    ## build matrix for regression
    
    for(i in 1:k.trials){
      res <- runQuery(df=available.pums, rounding = TRUE, R = a)
      indicator <- 1:n %in% res$index
      indicator <- as.numeric(indicator)
      history.rounding[i,] <- c(res$truesum, res$sum, indicator)
    }
    
    ## Convert matrix into data frame
    release.data.rounding <- as.data.frame(history.rounding[,2:102])                    
    names(release.data.rounding) <- varnames                   
    
    ## Run reg and get estimates
    output.rounding <- lm(formula, data=release.data.rounding)
    estimates.rounding <- output.rounding$coef
    
    RMSE.matrix[a,b] <- postResample(history.rounding[,1], history.rounding[,2])[1]
    correct.preds <- ((estimates.gaussian>0.5) == sensitive.data)  
    acc.matrix[a,b] <- sum(correct.preds)/100

  }
  
  RMSE.rounding <-rowMeans(RMSE.matrix)
  acc.rounding <-rowMeans(acc.matrix)

}


for(b in 1:10){

  for(a in parameter.range){
    ## build matrix for regression
    
    for(i in 1:k.trials){
      res <- runQuery(df=available.pums, gaussian = TRUE, gaussian.sigma = a)
      indicator <- 1:n %in% res$index                         # convert indices into a series of boolean/dummy variables
      indicator <- as.numeric(indicator)
      history.gaussian[i,] <- c(res$truesum, res$sum, indicator)
    }
    
    ## Convert matrix into data frame
    release.data.gaussian <- as.data.frame(history.gaussian[,2:102])             
    names(release.data.gaussian) <- varnames
    
    ## Run reg and get estimates
    output.gaussian <- lm(formula, data=release.data.gaussian)
    estimates.gaussian <- output.gaussian$coef
    
    RMSE.matrix[a,b] <- postResample(history.gaussian[,1], history.gaussian[,2])[1]
    correct.preds <- (estimates.gaussian>0.5) & (sensitive.data==1) | (estimates.gaussian<0.5) & (sensitive.data==0)  
    acc.matrix[a,b] <- sum(correct.preds)/100
    
    # estimated
    # correct.preds1 <- ((estimates.gaussian>0.5) == sensitive.data) 
    # acc.matrix1[a,b] <- sum(correct.preds1)/100    
  }
  
  RMSE.gaussian <-rowMeans(RMSE.matrix)
  acc.gaussian <-rowMeans(acc.matrix)
}


for(b in 1:10){
  
  for(a in parameter.range){
    ## build matrix for regression
    
    for(i in 1:k.trials){
      res <- runQuery(df=available.pums, subsample = TRUE, subsample.size = a)
      indicator <- 1:n %in% res$index                         # convert indices into a series of boolean/dummy variables
      indicator <- as.numeric(indicator)
      history.subsamp[i,] <- c(res$truesum, res$sum, indicator)                    # save into our results matrix
      }
    
    ## Convert matrix into data frame
    release.data.subsamp <- as.data.frame(history.subsamp[,2:102])                    
    names(release.data.subsamp) <- varnames                   
    
    ## Run reg and get estimates
    output.subsamp <- lm(formula, data=release.data.subsamp)
    estimates.subsamp <- output.subsamp$coef
    
    RMSE.matrix[a,b] <- postResample(history.subsamp[,1], history.subsamp[,2])[1]
    correct.preds <- (estimates.subsamp>0.5) & (sensitive.data==1) | (estimates.subsamp<0.5) & (sensitive.data==0)  
    acc.matrix[a,b] <- sum(correct.preds)/100
   }
  
  RMSE.subsamp <-rowMeans(RMSE.matrix)
  acc.subsamp <-rowMeans(acc.matrix)

}


## PLOT Results 
rounding.for.plots = data.frame(parameter.range, RMSE.rounding, acc.rounding)

p1 <- ggplot(rounding.for.plots, aes(x = parameter.range, y = RMSE.rounding)) + 
  geom_point() +
  labs(x = "R", y = 'RMSE', title = 'Root Mean Squared Error (RMSE)') + 
  theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(rounding.for.plots, aes(x = parameter.range, y = acc.rounding)) + 
  geom_point() +
  geom_hline(yintercept = 0.5, color = "darkcyan") +
  labs(x = "R", y = 'Accuracy', title = 'Accuracy') + 
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(rounding.for.plots, aes(x = RMSE.rounding, y = acc.rounding)) + 
  geom_point() +
  labs(x = "RMSE", y = 'Accuracy', title = 'Accuracy vs. RMSE') + 
  theme(plot.title = element_text(hjust = 0.5))

plots.rounding = grid.arrange(p1, p2, p3, nrow = 1)

ggsave(filename = 'rounding_plots.pdf', plot = plots.rounding, width = 11, height = 5, units = 'in')

### 

gaussian.for.plots = data.frame(parameter.range, RMSE.gaussian, acc.gaussian)

p4 <- ggplot(gaussian.for.plots, aes(x = parameter.range, y = RMSE.gaussian)) + 
  geom_point() +
  labs(x = "R", y = 'RMSE', title = 'Root Mean Squared Error (RMSE)') + 
  theme(plot.title = element_text(hjust = 0.5))

p5 <- ggplot(gaussian.for.plots, aes(x = parameter.range, y = acc.gaussian)) + 
  geom_point() +
  geom_hline(yintercept = 0.5, color = "darkcyan") +
  labs(x = "R", y = 'Accuracy', title = 'Accuracy') + 
  theme(plot.title = element_text(hjust = 0.5))

p6 <- ggplot(gaussian.for.plots, aes(x = RMSE.gaussian, y = acc.gaussian)) + 
  geom_point() +
  labs(x = "RMSE", y = 'Accuracy', title = 'Accuracy vs. RMSE') + 
  theme(plot.title = element_text(hjust = 0.5))

plots.gaussian = grid.arrange(p4, p5, p6, nrow = 1)

ggsave(filename = 'gaussian_plots.pdf', plot = plots.gaussian, width = 11, height = 5, units = 'in')

subsamp.for.plots = data.frame(parameter.range, RMSE.subsamp, acc.subsamp)

p7 <- ggplot(subsamp.for.plots, aes(x = parameter.range, y = RMSE.subsamp)) + 
  geom_point() +
  labs(x = "R", y = 'RMSE', title = 'Root Mean Squared Error (RMSE)') + 
  theme(plot.title = element_text(hjust = 0.5))

p8 <- ggplot(subsamp.for.plots, aes(x = parameter.range, y = acc.subsamp)) + 
  geom_point() +
  geom_hline(yintercept = 0.5, color = "darkcyan") +
  labs(x = "R", y = 'Accuracy', title = 'Accuracy') + 
  theme(plot.title = element_text(hjust = 0.5))

p9 <- ggplot(subsamp.for.plots, aes(x = RMSE.subsamp, y = acc.subsamp)) + 
  geom_point() +
  labs(x = "RMSE", y = 'Accuracy', title = 'Accuracy vs. RMSE') + 
  theme(plot.title = element_text(hjust = 0.5))

plots.subsampling = grid.arrange(p7, p8, p9, nrow = 1)

ggsave(filename = 'subsampling_plots.pdf', plot = plots.subsampling, width = 11, height = 5, units = 'in')


