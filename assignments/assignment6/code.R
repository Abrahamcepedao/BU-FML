data <- read.csv("data.csv", header = TRUE)
attach(data)
head(data)

#------> Problem 1.
data$temp_level <- ifelse(data$temp >= 98.6, 1, 0)

#------> Problem 2.
men <- data[data$sex == 1,]
women <- data[data$sex == 2,]
summary(men)
summary(women)

#------> Problem 3.
#1. Hypothesis
#H0 ∶ p1 = p2 (The underlying proportion of high body temperature between women and men who h)
#H1 ∶ p1 ≠ p2
#α = 0.05

#2. Select test statistic
#z-test

n1 <- length(men$sex)
n2 <- length(women$sex)
p1 <- 14/n1
p2 <- 35/n2
p <- (14 + 35)/(n1+n2)
z <- (p1 - p2) / (sqrt(p*(1-p) * (1/n1 + 1/n2)))

#3. Decision Rule
#Decision Rule: Reject H0 if ∣z∣ ≥ 3.8 , Otherwise, do not reject H0

#4. Compute test
prop.test(x = c(14,35), n = c(n1,n2), alternative = "two.sided", conf.level = 0.95, correct = TRUE)

#5. Conclusion
#Given that 13.102 ≥ 3.8 and a p-value of 0.0002951, we have sufficient evidence to reject the null hypothesis. 
#The percentage of higher body temperatures is higher among women than in men.

#Risk difference = p1 - p2
print(rd <- p1 - p2)

#------> Problem 4.
#1. Hypothesis
#H0 ∶ β1 = 0
#H1 ∶ β1 ≠ 0
#α = 0.05

#2. Select test statistic
#Wald Statistic
#z = β1/SE_β1

#3. State decision rule
qnorm(0.95)
#Decision Rule: Reject H0 if ∣z∣ ≥ 1.644 or Reject H0 if p ≤ α
#Otherwise, do not reject H0

#4. Compute test statistic
mdl <- glm(data$temp_level ~  data$sex, family = "binomial")
summary(mdl)
exp(mdl$coefficients[2])
exp((mdl$coefficients[2]-qnorm(0.95)*summary(mdl)$coefficients[2,2]))
exp((mdl$coefficients[2]+qnorm(0.95)*summary(mdl)$coefficients[2,2]))

#5. Conclusion
#Reject H0 since z (3.7) ≥ 1.644 and p-value ≤ 0.05 
#We have significant evidence at the α = 0.05 level that β1 ≠ 0. 
#There is evidence of an association between sex and level of body temperature.
#With an odds ratio of 4.25. At the 95% confidence intervals for the odds ratio are = 2.23369, 8.086396.

#------> Problem 5.
mdl2 <- glm(data$temp_level ~ data$sex + data$Heart.rate, family = "binomial")
summary(mdl2)
exp(mdl2$coefficients[2] + mdl2$coefficients[3]*10)

#------> Problem 6.
library(pROC)
data$prob <- predict(mdl2, type=c("response"))
g <- roc(data$temp_level ~ data$prob)
plot(1-g$specificities, g$sensitivities, type="l", xlab="1-specificity",
     ylab="Sensitivity", main="ROC  curve")
abline(a=0, b=1)

#Quiz 6
prop.test(x = c(22,6), n = c(63,45), alternative = "two.sided", conf.level = 0.95, correct = FALSE)
exp((2.07 + 0.18*60)/(1+2.07 + 0.18*60))


#table
#   TP = 14 | FP = 64
#   FN = 6  | TN = 116

#install.packages('devtools')
#library(devtools)

#install_github('andreacirilloac/updateR')


#library(updateR)
#updateR(admin_password = 'code2018')
