#5,000 individuals about their lung cancer status in 12 years

#1. Read data. Are there missing values? Is there an intelligent way to deal with them?
data <- read.csv("Smoking_Data_Example.csv")
head(data)
data$cig_years[is.na(data$cig_years)] = 0

#2. Split the data into a training 20% and testing set. Fit a logistic regression 
#model using all predictors to predict lung cancer. Interpret. Is an assumption model violated? 
#Should we remove any variables?
indices = sample(seq(1:5000))

test = data[indices[1:1000],]
train = data[-indices[1:1000],]

mdl = glm(is_lungcancer ~ smoking_status + age.x + sex.x + cig_years,data=train,family="binomial")

summary(mdl)

mdl = glm(is_lungcancer ~ smoking_status + age.x + sex.x,data=train,family="binomial")

summary(mdl)

#3.  Use predict for testing set. Compute the ROC curve and the AUC
test$predictions = predict(mdl,test,type="response")

require(pROC)

r = roc(test$is_lungcancer,test$predictions)

#4. Find the optimal cutoff value using the training set and compute the sensitivity
#and specificity of the model
