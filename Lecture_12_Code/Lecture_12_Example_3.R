# Formal Inference in Logistic Regression#

# setwd("SET THE Working Director to THE PATH TO THIS DIRECTORY")
rm(list = ls())

# install.packages("aod")
# install.packages("pROC")
library(aod)
library(stats)
library(pROC)
require(lmtest)


data<-read.csv("Datasets/cevent.csv")

#Binarize Sex
data$male <- ifelse(data$sex =="M", 1, 0)

#Use cut to convert a continuous variable to discrete
data$chol_disc = cut(data$chol,c(0,120,500),labels = c("Low","High"))

# Generate logistic regression models

m <- glm(event ~ age + chol + sex, data=data, family = "binomial")
m2 = glm(event ~ age,data=data,family="binomial")
summary(m2)

summary(m)


# overall test of happiness. 
# install.package("aod")

# Two Ways to do a global test
# 1. Wald Test on the standard errors
# 2. Likelihood Ratio test between full and null models


# Terms: An optional integer vector specifying which coefficients 
# should be jointly tested
# Terms define to compare which regression coefficients, 
# Here we want to compare the 2 to 4 (first is the intercept)

# It gives as a result Chi-Squared test results and p-value of it 
# if p is smaller than 0.05 you can reject the null hypothesis
library(aod)

#Global test
wald.test(b = coef(m) , Sigma = vcov(m) , Terms = 2:4)

#Individiual tests
summary(m)

#Null hypothesis all coefficients are 0
#Vs. alternative at least one is different than 0
m <- glm(event ~ age + sex + chol , data=data, family = "binomial")
m2 <- glm(event ~ age + sex,data=data,family="binomial")
m3 <- glm(event ~ 1,data=data,family="binomial")

#Likelihood ratio test compares whether the added predictors
#Improve the log likelihood beyond what would be expected
#if random predictors were added
lrtest(m,m2)


lrtest(m,m3)
#Likelihood is the probability of seeing this data
#Given the model is true 

summary(m)


#Wald test tests whether the coefficients for "Terms" are different enough from 0 
#Wald with multiple terms is similar to a "partial F-Test"
wald.test(b = coef(m) , Sigma = vcov(m) , Terms = 2:3)
coef(m)

#Does cholesterol predict event?
m = glm(event ~ chol,data=data,family="binomial")
m2 = glm(event ~ 1,data=data,family="binomial")

#Is this model better than always predicting 
#the prevalence of event?
lrtest(m,m2)


#Same as null deviance - residual deviance to do global test 
#(this is distributed according to a chi-sq distribution with
#degrees of freedom = # of terms in full model)

summary(m)

