##
##  cs208
##  hw1 q2
##
##  demonstrate reconstruction attack by regression on sums of subsets
##
##  lipika ramaswamy
##  

## import libraries
library(dplyr)

#### Parameters ####
# set.seed(123)
# n <- 100        # Dataset size
k.trials <- 200  # Number of queries <- fewer observations than variables <- could add some regularization


## Read in data:
getwd()
setwd("/Users/lipikaramaswamy/Documents/Harvard/CS208/cs208_lr/examples/wk1_attacks/")
pums <- read.csv("../../data/FultonPUMS5sample100.csv")

var <- "uscitizen"
my.pi <- mean(pums[,var])

### build random subsets of data

P = 491
prime.options <- seq(from=1, to=P-1, by=1)

### subset data to that which is available to attacker
available.pums = select(pums, uscitizen, sex, age, educ, latino, black, asian, married,
                        divorced,children,disability,militaryservice,employed,englishability)



# build ONE subset function
build.one.subset <- function(df, num.rows=100, num.cols = 13){
  df$p.v <- NA
  for(i in 1:num.rows){
  vec = as.numeric(df[i,2:14])
  r = sample(prime.options, size=num.cols, replace = FALSE)
  p = (sum(vec*r))%%P%%2
  df$p.v[i] = p
  }
}

##### build query
query <- function(n, data){
  available.pums$p.v<-NA
  for(i in 1:100){
    cat('in row number __ of 100:', i)
    print('making vec')
    vec = as.numeric(available.pums[i,2:14])
    print(vec)
    r = sample(prime.options, size=13, replace = FALSE)
    print(r)
    p = (sum(vec*r))%%P%%2
    print(p)
    available.pums$p.v[i] = p
  }
  
  subset = available.pums[available.pums$p.v == 1,]
  sum <- sum(subset$uscitizen)
  index = as.numeric(rownames(subset))
  return(list(sum=sum, index=index))
}

history <- matrix(NA, nrow=k.trials, ncol=100+1)            # a matrix to store results in

for(i in 1:k.trials){
  cat('in loop number', i)
  res <- query(n=q.size, data=available.pums)
  indicator <- 1:n %in% res$index                         # convert indices into a series of boolean/dummy variables
  indicator <- as.numeric(indicator)
  history[i,] <- c(res$sum, indicator)                    # save into our results matrix
}

#### Convert matrix into data frame
xnames <- paste("x", 1:n, sep="")
varnames<- c("y", xnames)
releaseData <- as.data.frame(history)                     # convert matrix into data frame
names(releaseData) <- varnames                   


formula <- paste(xnames, collapse=" + ")                  # construct the formula, y ~ x1 ... xn -1
formula <- paste("y ~ ", formula, "-1")
formula <- as.formula(formula)
print(formula)

output <- lm(formula, data=releaseData)                   # run the regression
estimates <- output$coef                                  # save the estimates



sensitiveData <- pums[, "uscitizen"]

#### Plot results ####

true.1 <- (estimates>0.5) & (sensitiveData==1)            # Correctly predicted values
true.0 <- (estimates<0.5) & (sensitiveData==0)
true1.frac <- round(sum(true.1, na.rm=TRUE)/sum(sensitiveData)*100)/100
true0.frac <- round(sum(true.0, na.rm=TRUE)/sum(1-sensitiveData)*100)/100
truth.col<-1 + true.1 + true.0

delta <- 0.05                                             # Slight disturbance to add
jitterx <- runif(n=n, min=-delta, max=delta)
jittery <- runif(n=n, min=-delta, max=delta)
semi.blue <- rgb(0,90,239,200,maxColorValue=255)          # Slightly transparent colors
semi.red  <- rgb(239,90,0,200,maxColorValue=255)
col.values <- c(semi.red, semi.blue)


plot(x=estimates + jitterx, y=sensitiveData + jittery, xlab="estimate", ylab="sensitive value", 
     main="Reconstruction of USCitizen Variable", col=col.values[truth.col])    
# Plot reconstruction against actual sensitive data
abline(v=0.5, lty=2)
text(x=0.5, y=0.8, labels=paste("fraction ones correct: ", true1.frac), pos=4)
text(x=0.5, y=0.2, labels=paste("fraction zeros correct: ", true0.frac), pos=2)

true.1
