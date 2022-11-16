# setwd("SET THE Working Director to THE PATH TO THIS DIRECTORY")



# Load the smoker data set. 
data<- read.csv("Datasets/smoker.csv")
attach(data)

# The function factor is used to encode a vector as a factor 
#(the terms ‘category’ and ‘enumerated type’ 
# are also used for factors).

#Check if the variable is a factor
is.factor(data$group)

#Convert to factor
data$group = factor(data$group,levels=c("formerSmokers","neverSmokers",
                                        "currentLightSmokers","currentHeavySmokers"))

#Generate a histogram of systolic blood pressure
hist(data$SBP)

# Calculate mean, SD of SBP by groups
aggregate(data$SBP, by=list(data$group), summary)
aggregate(data$SBP, by=list(data$group), sd)

# Dplyr way to do this
require(dplyr)

#Look at summary statistics using dplyr
data %>% 
  group_by(group) %>% 
  summarise(std = sd(SBP), m2 = mean(SBP))

# Boxplot data by group (grouped boxplot)
boxplot(data$SBP~data$group, data=data, main="SBP by smoking status", xlab="group",  ylab="SBP", ylim=c(100, 160))


#ANOVA
m <- aov(data$SBP~data$group, data=data)

#Linear Regression - to generate an ANOVA analysis
anova(lm(SBP ~ group, data = data))

#Get summary of the ANOVA model
summary(m)

#If ANOVA is significant, can conduct pairwise t-tests
#To identify differences between pairs of groups
pairwise.t.test(data$SBP, data$group, p.adj="none")

#Using two-sample t-test to accomplish the same tasks
#This does not assume equal variances
#And requires a higher t-statistic (degrees of freedom is smaller)
t.test(data$SBP[data$group=="neverSmokers"],data$SBP[data$group=="currentLightSmokers"])

#Pairwise t test with bonferroni adjustment 
pairwise.t.test(data$SBP, data$group, p.adj="bonferroni")


# Compute Tukey Honest Significant Differences
?TukeyHSD

# Create a set of confidence intervals on the differences between the means of the levels of a factor 
# with the specified family-wise probability of coverage. 
# The intervals are based on the Studentized range statistic, Tukey's Honest Significant Difference method.

# pairwise test with TukeyHSD
TukeyHSD(m)



