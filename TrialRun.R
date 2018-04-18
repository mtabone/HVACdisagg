setwd('~/repos/HVACDisagg/')
source('functions/classModelFit.R')
require(ggplot2)


## Load data for a test home 
data = read.csv('data/testhome2.csv')
data$localhour = as.POSIXlt(data$localhour)

## Create factors for hoour of day and day of week. 
data$HOD = as.factor(data$localhour$hour)
data$dayType = as.factor(is.element( data$localhour$wday , c(0,6) ))

## Define changepoints to try
cps = data.frame(matrix(c(50, 55, 60, 65, 70, 75, 80),ncol = 1))
colnames(cps) = 'cool'

## Fit models
out = classModelFit(data , ChangePoints = cps,
                     dependentColName = 'use', CPColName = 'temperature', 
                     otherRegressors = 'HOD:dayType', emisShape = 'kernel',
                     coolingIntercept = TRUE, 
                     coolingState = TRUE, 
                     storeall = TRUE, verbose= F, class_model = TRUE)

## Print names of elements in model output
print(names(out))

## Idenitfy total log likelihood with minimim log likelihood
print(out$totalLL)
k = which.max(out$totalLL)


## Plot temperature versus power use colored by probability of cooling
data$probcool = out$prob_cool[[k]]

ggplot(data) + 
  geom_point( aes(x = temperature, y = use, col = probcool) )
