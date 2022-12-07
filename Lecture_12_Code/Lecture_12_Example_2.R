#Logistic Regression and Multiple-Logistic Regression#
# install.packages("aod")
# install.packages("pROC")
library(aod)
library(stats)
library(pROC)
#age, cholesterol, sex, and whether or not they had a coronary event
data<-read.csv("Datasets/cevent.csv")

#Learn logistic regression model
m <- glm(data$event ~ data$chol + data$age + data$sex, 
         family = "binomial")

#Excluding age
m2 = glm(data$event ~ data$chol,family="binomial")

#Predict probability for each example - 
#without the type option, it outputs the "logit" or "log odds"
data$logits = predict(m,data)


#"response" denotes probability
data$predictions = predict(m,data,type="response")
summary(m2)

#Calculate probability for a specific example
chol = 110
age = 65
#for female

#First use coefficients to calculate the log odds or logit
logit = 0.04226*age + 0.02889*chol -8.53621 + 2.52120*0

#Then use the logistic function to calcluate the probability
prob = exp(logit) / (1+exp(logit))

table(data$event)

summary(m)

# Another way to do logistic regression in R is to use rms package. 
# install.packages("rms")
library(rms)
# see 
?lrm
lrm(event ~ chol, data=data)


fit <- lrm(event ~ chol, data = data, x=TRUE, y=TRUE)
print(fit)


# find influence points 
which.influence(fit, cutoff=.3)


# Multiple Logistic Regression. 
lrm(event~ chol + age, data = data)

