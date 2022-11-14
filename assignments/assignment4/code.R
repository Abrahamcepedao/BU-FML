data = read.csv("data.csv")
attach(data)
head(data)

#Problem 2
require(ggplot2)
ggplot(data, aes(x=education,y=score)) + 
  geom_point(shape=1,color="red") + xlab("Years of education") + ylab("Prestige score") + xlim(c(5,max(education))) + ylim(c(0,max(score))) + 
  ggtitle("Scatterplot of years of education and prestige score") + theme_bw(base_size=14) 

print(cor(education, score))

#Problem 3
mdl = lm(score ~ education, data=data)

data$prediction = predict(mdl,data)

data$residuals = data$score - data$prediction

# Plot of regression line

# Plot of residuals
ggplot(data=data,aes(x=score,y=residuals)) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept=0,linetype="dashed")

#Problem 4
mdl2 = lm(score ~ education + income + per_women)

#1 - Hypothesis
#H0 ∶ βeducation = βincome = βper_women = 0
#H1 ∶ βeducation ≠ 0 and/or βincome ≠ 0 and/or βper_women ≠ 0

#2 - Test statistic
#df1 = 3
#df2 = 102 - 3 - 1 = 98
#F-test

#3 - Decision rule
qf(.95, df1=3, df2=98)
#Decision Rule: Reject H0 if F ≥ 2.697. Otherwise, do not reject H0

#4 - Compute test
summary(mdl2)

#5 - Conclusion
#F-statistic = 129.2 ≥ 2.697
#p-value < 0.005


#Problem 5
confint(mdl2, level =0.95)

#Problem 6
plot(fitted(mdl2), resid(mdl2), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')
