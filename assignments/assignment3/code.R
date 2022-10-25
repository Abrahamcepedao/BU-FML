#1. Read data
data <- read.csv("data.csv", header = TRUE)
attach(data)

#2. Scatterplot
# Scatter plot using ggplot
require(ggplot2)
ggplot(data, aes(x=meals,y=mercury)) + 
  geom_point(shape=1,color="red") + xlab("Number of meals with fish") +
  ylab("Total Mercury (mg/g)") + xlim(c(0,max(meals))) + ylim(c(0,max(mercury))) + 
  ggtitle("Scatterplot of number of meals with fish and the total mercury") + theme_bw(base_size=14) 

#3. Correlation coefficient
cor(meals, mercury)

#4. Least squares regression
my.model<-lm(data$mercury~data$meals)
#abline(my.model)

ggplot(data, aes(x=meals,y=mercury)) + geom_point(shape=1,color="red") +
  xlab("Number of meals with fish") + 
  ylab("Total Mercury (mg/g)") + xlim(c(0,max(meals))) + ylim(c(0,max(mercury))) + 
  ggtitle("Scatterplot of number of meals with fish and the total mercury") + theme_bw(base_size=14) + 
  geom_abline(slope = my.model$coefficients[2],intercept=my.model$coefficients[1])

#5. B1 & B0
sprintf("β_0 %.3f", my.model$coefficients[1])
sprintf("β_1 %.3f", my.model$coefficients[2])

#6. Test hypothesis
#6.1 - Hypothesis
#H0 ∶ β1 = 0 (there is no linear association) 
#H1 ∶ β1 ≠ 0 (there is a linear association)
#α = 0.05

#6.2 - Test statistic
#F-test
#df1 = 1
#df2 = 100-2 = 98

#6.3 - Decision rule
qf(.90, df1=1, df2=98)
#Decision Rule: Reject H0 if F ≥ 2.75743 Otherwise, do not reject H0

#6.4 - Compute the test statistic
anova(my.model)
summary(my.model)
#Reject H0 since 93.689 ≥ 4.183.
#We have significant evidence at the α = 0.10 level that β1 ≠ 0.
#There is evidence of a significant linear association between study time and
#exam score (here, p < 0.001 as calculated using software program).

#R^2 value = 0.4888

library(Metrics)

pred <- predict(my.model, data.frame(mercury = data$mercury))

rmse(data$mercury, predict(my.model, data.frame(mercury = data$mercury)))
summary(my.model)$r.squared 









