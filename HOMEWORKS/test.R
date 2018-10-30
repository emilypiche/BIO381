


### PACKAGES ###
library(MASS)
library(ggplot2)
library(dplyr)

### GLOBAL TASKS ###
PL <- read.csv("/Users/emilypiche/Desktop/plotlevel.csv", as.is=TRUE)
#SOM - real soil organic matter data
SOM <- PL$LOI
#RC - real conifer proportion data
RC <- PL$CONIFER_D
# conifer parameters
normPars2 <- fitdistr(RC,"normal")
normPars2
# creating fake conifer proportions using parameters from my real conifer proportions
fakeCon <- rnorm(length(RC), normPars2$estimate["mean"], normPars2$estimate["sd"]) 

# Here is a function that will take a set of data and create a new distribution with a positive relationship to the first by adding normally distributed noise. 

##############

# Function: makeNoise - creates a vector with positive or negative relationship to an existing vector by tranforming, and adding noise generated from a normal distribution
# INPUTS: a vector of data, parameters for transformation and noise
# var1 = original data/sample 
# m = mean of normal distribution for noise 
# sd = standard deviation of added noise 
# int = intercept of equation to set relationship with original data
# exp = expansion transforms data
# OUTPUTS: new vector of transformed data
# -------------
makeNoise <- function(var1=runif(10), m=.5, sd=.25, int=0, exp=1){
  e <- rnorm(length(var1), m, sd)
  newVar <- int + exp*var1 + e 
  newFrame <- data.frame(var1, newVar)
  return(newFrame)
}
# -------------
fakeSOM <- makeNoise(fakeCon, .2, .1, 0, .05)


# The following function will run a linear regression of of the new variable against the original variable:

##############

# Function: testFakes - runs a linear regression of of the new variable from makeNoise against the original variable (var1).
# INPUTS: dataframe containing orginal data input to makeNoise, and the output of makeNoise. 
# OUTPUTS: r^2 and p value of linear regression
# -------------
testFakes <- function(df){
  sum <- summary(lm(df[,2]~df[,1]))
  r2<-sum$adj.r.squared
  p<-sum$coefficients[7]
  report <- data.frame(r2, p)
  return(report)
}
testFakes(fakeSOM)
# -------------


# The following function make a scatterplot of the new variable against the original variable:

##############

# Function: showScatter - creates scatterplot of two variables from a dataframe
# INPUTS: dataframe containing orginal data input to makeNoise, and the output of makeNoise. 
# OUTPUTS: quick plot of the variables
# -------------
showScatter <- function(df){
  plot <- qplot(df[,1], df[,2], xlab="Propotion of Conifers on Plot (%)", ylab="Soil Organic Matter(%)")+ theme_bw() + geom_smooth(method="lm")
  print(plot)
}
showScatter(fakeSOM)
# -------------



####### Daisy Chaining  my functions together? ########
fakeSOM <- makeNoise(fakeCon, .2, .1, 0, .03)
testFakes(fakeSOM)
showScatter(fakeSOM)
