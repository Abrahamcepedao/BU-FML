#Exit example and class project example #

# Load the data
data = read.csv("Smoking_Data_Example.csv")

# Summarize the data
summary(data)


#Check the missing values for cig_years, never smokers will have cig years= 0

tmp = data[is.na(data$cig_years),]

data$cig_years[is.na(data$cig_years)] = 0

#Make never smokers the reference group
data$smoking_status = factor(data$smoking_status,levels=c("Never","Former","Current"))

#1000 samples for testing
indices = sample(seq(1:5000))

test = data[indices[1:1000],]
train = data[-indices[1:1000],]

#Cancer incidence by group
table(train$is_lungcancer,train$smoking_status)

#Logistic regression model
mdl = glm(is_lungcancer ~ smoking_status + age.x + sex.x + cig_years,data=train,family="binomial")

summary(mdl)
#Cig years is associated with smoking status, must remove

mdl = glm(is_lungcancer ~ smoking_status + age.x + sex.x,data=train,family="binomial")

summary(mdl)

#Can convert odds ratio to relative risk if the risk in the control group is known
# rr = or / (1 - risk_control + (risk_control * or))

#Get testing predictions
test$predictions = predict(mdl,test,type="response")

#Calculate AUC
require(pROC)

r = roc(test$is_lungcancer,test$predictions)

#Optimal cutoff in training set
train$predictions = predict(mdl,train,type="response")

library(InformationValue)
optCutOff <- optimalCutoff(actuals=train$is_lungcancer, predictedScores=train$predictions,
                           optimiseFor="Both", returnDiagnostics=TRUE)

# confusion matrix for this cutoff
test$pred_cancer = ifelse(test$predictions>=optCutOff$optimalCutoff,1,0)

c2 = confusionMatrix(test$is_lungcancer,test$pred_cancer,threshold=optCutOff$optimalCutoff)

table(test$is_lungcancer,test$pred_cancer)
#compute sensitivity and specificity

#sensitivity Tp / (Tp + Fn)
sens = c2[2,2] / (c2[2,2] + c2[1,2])

#specificity
spec = c2[1,1] / (c2[1,1] + c2[2,1])



