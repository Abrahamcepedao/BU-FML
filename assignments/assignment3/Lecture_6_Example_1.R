#Calculating correlation and the simple linear regression
# model from last time
student <- read.csv("students.csv")
student

attach(student)

study.hours<-hours

# Base-R plot study hours vs exam score
plot(hours, score, main="Scatterplot of exam score vs. hours of study", xlab="Hours of study", ylab="Score", 
     xlim=c(0,10), ylim=c(0,100), pch=1, col="red", cex.lab=1.5)

# Scatter plot using ggplot
require(ggplot2)

# shape = pch, base_size changes font size
ggplot(student, aes(x=hours,y=score)) + 
  geom_point(shape=1,color="red") + xlab("Hours of study") +
  ylab("Exam Score") + xlim(c(0,10)) + ylim(c(40,100)) + 
  ggtitle("Scatterplot of exam scores vs. hours of study") + theme_bw(base_size=14) 


#calculate sample correlation
cor(study.hours, score)

#Correlation is symmetric
cor(score, study.hours)

a<-cor(score, study.hours)
print(a)

#Generate simple linear regression model
my.model<-lm(student$score~student$hours)

# print the linearValues
print(my.model)

# or and use the intercept and score to draw a line 
# abline(51.515, 5.012)

# OR

# or just pass the result values of the lm function to draw a line 
# NOTE this only works if the scatterplot was generated using the plot command
abline(my.model)

# Here's how you add the linear regression line to a ggplot
ggplot(student, aes(x=hours,y=score)) + geom_point(shape=1,color="red") +
  xlab("Hours of study") + 
  ylab("Exam Score") + xlim(c(0,10)) + ylim(c(40,100)) + 
  ggtitle("Scatterplot of exam scores vs. hours of study") + theme_bw(base_size=14) + 
  geom_abline(slope = my.model$coefficients[2],intercept=my.model$coefficients[1])

# Calculate Confidence Intervals for Model Parameters
confint(my.model)

?confint


# default level is .95 - swtiching to 90% confidence
confint(my.model, level = .90)


# report a summary of my model 
summary(my.model)

# create Analysis of Variance Table
anova(my.model)

# fitted values of my model 
fitted(my.model)

# residuals of my model 
resid(my.model)

#Predicting on a new dataset
mdl = lm(score ~ hours,data=student)

#Create new dataset of study hours randomly
hours = runif(10,0,10)
new_df = data.frame(hours)

#Get predicted test scores for new dataset
new_df$predicted_score = predict(mdl,new_df)

my.model<-lm(student$score~student$hours)

summary(my.model)


quiz <- c(76,82,85,85,85,90,90,90)
hw <- 
data <- data.frame(
  quiz <- c(76,82,85,85,85,90,90,90),
  hw <- c(90,85,85,95,85,95,95,90)
)
colnames(data) <- c("quiz", "hw")
mdl <- lm(data$hw~data$quiz)
summary(mdl)
print(mdl)
plot(data$hw, data$quiz)
