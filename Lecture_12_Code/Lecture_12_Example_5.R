#Choosing a cutoff value and testing sets#

# setwd("SET THE Working Director to THE PATH TO THIS DIRECTORY")
rm(list = ls())

# https://www.r-bloggers.com/evaluating-logistic-regression-models/

# If you have some trouble with packages on MacOS use the following link 
# https://swvanderlaan.github.io/post/getting-r-with-tcl-tk-on-my-mac/

library(aod)
library(stats)
library(pROC)
library(lmtest)
library(survey)

# Heart disease is a comprehensive disease that many factors can contribute to its morbidity.
# To figure out the relationship between some common factors or heart related factors and the incidence, a study group set the Cleverland database of about 300 
# people about their sex, age and so on, and whether they have heart disease.
# First they want to know whether there are some common factors that influence the morbidity.

# Link to original data set https://archive.ics.uci.edu/ml/datasets/Heart+Disease
# The link to the original data:  https://www.kaggle.com/ronitf/heart-disease-uci/version/1

data<-read.csv("Datasets/heart-disease.csv")

head(data)

# Now let us separate the data set into two parts of train and test. 

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train.Data <- data[train_ind, ]

test.Data <- data[-train_ind, ]

train.Data$X = NULL
#Stepwise logistic regression
m <- step(glm(heart.target ~ .,   
              data=train.Data, 
              family = "binomial"))


summary(m)
#AIC = log likelihood (model fit) - # of predictors*samplesize

#LASSO Logistic Regression
require(glmnet)
m3 = cv.glmnet(as.matrix(data[,2:6]),data$heart.target,family="binomial",nfolds=5)
coef(m3)

#NULL Model
m2 = glm(heart.target ~ 1,data=train.Data,family="binomial")
lrtest(m,m2)

summary(m)


# Find the optimal cutoff
# Default cutoff is 0.5
# Chaning the cutoff can improve the accuracy 
# prob <-predict(m, type=c("response"))
predictValues <- predict(m , newdata = test.Data,  type = "response")
# log_predict <- predict(m , newdata = test.Data)


#Calculate AUC manually


df = data.frame(cbind(predictValues,test.Data$heart.target))
pos = df[df$V2==1,]
neg = df[df$V2==0,]

#Repeat 1,000 times
B = 1000
result = c()
for(i in 1:B)
{
  #Model prediction for random positive case
  pos_prediction = sample(pos$predictValues,1)
  
  #Model prediction for random negative case
  neg_prediction = sample(neg$predictValues,1)
  
  #Was the model right?
  result = c(result,ifelse(pos_prediction>neg_prediction,1,0))
}

#AUC = how often was the model right?
sum(result)/B


#Calculate AUC using software package
library(pROC)
r = roc(test.Data$heart.target,predictValues,direction="<")

#Look at sensitivity/specificity across thresholds
df = data.frame(cbind(r$sensitivities,r$specificities,r$thresholds))
colnames(df) = c("Sensitivity","Specificity","Thresholds")

#One criteria is youden index calculated here
df$criteria = df$Sensitivity*df$Specificity

#Choose best threshold
df$Thresholds[which.max(df$criteria)]


#Binarize prediction based on a threshold
table(test.Data$heart.target)

final.prediction <- ifelse(predictValues > 0.4217, 1, 0)

sum(train.Data$heart.target)/nrow(train.Data)

# install.packages("caret")
# install.packages('e1071', dependencies=TRUE)
library(caret)

#Calculate confusion matrix
cm <- confusionMatrix(factor(final.prediction,levels=c(1,0)), factor(test.Data$heart.target,levels=c(1,0)))
print(cm$table)

table(final.prediction,test.Data$heart.target)

#This package is useful to compare diagnostic tests
#And get confidence intervals for Sensitivity/Specificity
require(DTComPair)


#plot ROC
library(ROCR)
library(Metrics)
pr <- prediction(predictValues,  test.Data$heart.target)
perf <- performance(pr, measure = "tpr",x.measure = "fpr")
plot(perf)
auc(test.Data$heart.target, predictValues)


# The InformationValue::optimalCutoff function provides ways to find 
# the optimal cutoff

# install.packages("InformationValue")

#Another way to calculate optimal cutoff value
library(InformationValue)
optCutOff <- optimalCutoff(actuals=test.Data$heart.target, predictedScores=predictValues, optimiseFor="Both", returnDiagnostics=TRUE)

optCutOff$optimalCutoff
# optCutOff$optimalCutoff
# [1]0.4014107

plot.roc(roc(test.Data$heart.target,predictValues))





