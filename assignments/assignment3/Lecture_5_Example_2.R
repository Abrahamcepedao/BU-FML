#Simple linear regression and basic model diagnostics

student <- read.csv("students.csv")
student

attach(student)

study.hours<-hours

plot(hours, score, main="Scatterplot of exam score vs. hours of study", xlab="Hours of study", ylab="Score", 
     xlim=c(0,10), ylim=c(40,100), pch=1, col="red", cex.lab=1.5)


#calculate sample correlation
cor(study.hours, score)

cor(score, study.hours)

a<-cor(score, study.hours)
print(a)

my.model<-lm(student$score~student$hours)

# print the linearValues
print(my.model)

# or and use the intercept and score to draw a line 
# abline(51.515, 5.012)

# OR

# or just pass the result values of the lm function to draw a line 
abline(my.model)

# Calculate Confidence Intervals for Model Parameters
confint(my.model)

?confint


# default level is .95
confint(my.model, level = .90)


# report a summary of my model 
summary(my.model)

# create Analysis of Variance Table - we'll talk about this in Module 5
anova(my.model)

# fitted values of my model 
fitted(my.model)

# residuals of my model 
resid(my.model)

# We will talk about these diagnostic plots in detail in Lecture 8
plot(my.model)



quiz = c(95,90,88,97,75,70,66,83,40,79)
hw = c(93,88,80,96,77,80,63,93,70,82)

cor(quiz,hw)

#1.
#h0 p = 0
#h1 P not equal 0

#2.
#t-statistic, alpha = 0.05

#3.
qt(0.975, length(quiz)-2)
#Receject h0 if |t| > 2.31

#4.
cor.test(quiz,hw)
#t = 3.6302

#5.
# 3.63 > 2.31 reject h0
# There is a linear associationnn between quiz scores and homework scores (r ) 
# p-value = 0.007
