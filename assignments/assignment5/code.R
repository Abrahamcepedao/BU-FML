data <- read.csv("data.csv", header=TRUE)
attach(data)

#Problem 1
table(group)

chem <- data[data$group == "Chemistry student",]
math <- data[data$group == "Math student",]
phys <- data[data$group == "Physics student",]
summary(chem$iq)
summary(math$iq)
summary(phys$iq)

install.packages("ggplot2")
require(ggplot2)
ggplot(chem, aes(x=chem$age,y=chem$iq)) + 
  geom_point(shape=1,color="red") + xlab("Age") + ylab("IQ score") + xlim(c(min(chem$age)-2,max(chem$age))) + ylim(c(min(chem$iq)-2,max(chem$iq))) + 
  ggtitle("Scatterplot of age and IQ score in Chemistry students") + theme_bw(base_size=14) 

ggplot(math, aes(x=math$age,y=math$iq)) + 
  geom_point(shape=1,color="red") + xlab("Age") + ylab("IQ score") + xlim(c(min(math$age)-2,max(math$age))) + ylim(c(min(chem$iq)-2,max(chem$iq))) + 
  ggtitle("Scatterplot of age and IQ score in Math students") + theme_bw(base_size=14) 

ggplot(phys, aes(x=phys$age,y=phys$iq)) + 
  geom_point(shape=1,color="red") + xlab("Age") + ylab("IQ score") + xlim(c(min(phys$age)-2,max(phys$age))) + ylim(c(min(phys$iq)-2,max(chem$iq))) + 
  ggtitle("Scatterplot of age and IQ score in Physics students") + theme_bw(base_size=14) 

#Problem 2 -
#2.1 - Hypothesis
#H0 ∶ μ1 = μ2 = μ3
#H1 : μ1 ≠ μ2 ≠ μ3
#α = 0.05

#2.2 - Select test statistic
#F= MSB/MSW
#k-1 = 2
#n-k = 45 - 3 = 42 deg of freedom

#2.3 - State decision rule
qf(.95, df1=2, df2=42)
#Decision Rule: Reject H0 if F ≥ 3.219942
#Otherwise, do not reject H0

#2.4 - Compute test statistic
anova.model <- aov(data$iq~group, data=data)
summary(anova.model)

#2.5 - Conclusion
#We have enough evidence to reject null hypothesis given that F value (26.57) > 3.219942


#2.6 - Tukey
TukeyHSD(anova.model)


#Problem 3
data$g1 <- ifelse(data$group == "Math student",1,0)
data$g2 <- ifelse(data$group == "Physics student",1,0)

model <- lm(data$iq~data$g1+data$g2, data=data)
summary(model)

#Problem 4
data$group = factor(data$group)
library(car)

mdl = lm(iq ~ group + age,data=data)
summary(mdl)

library(emmeans)
emmeans(mdl, specs = "group")
