# Load the smoker data set. 
data<- read.csv("Datasets/smoking_SBP.csv")


# Print out the data and check what it is inside
print(data)

attach(data)

# First run a normal one way anova 
anova.model <- aov(data$SBP~group, data=data)
summary(anova.model)

##There is a difference in blood pressure between at least one pair of
#smoking statuses

# Create dummy variables
data$g0 <- ifelse(data$group=='Current heavy smoker', 1, 0)
data$g1 <- ifelse(data$group=='Current light smoker', 1, 0)
data$g2 <- ifelse(data$group=='Former smoker', 1, 0)
data$g3 <- ifelse(data$group=='Never smoker', 1, 0)


cbind(data$group,data$g0,data$g1,data$g2,data$g3)
print("Our Data after adding Dummy Variables")
print(data)

# One-way ANOVA using lm() function
m2 <- lm(data$SBP~data$g0+data$g1+data$g2, data=data)
summary(m2)

#Same result as ANOVA - F-stat is significant
#Differences between groups 0,1, and 2 with group 3

#Can also get more specific differences

require(dplyr)
data %>% group_by(group) %>% summarise(m = mean(SBP))

m3 <- lm(data$SBP~data$g1+data$g2+data$g3, data=data)
summary(m3)

#Not enough evidence for difference between group 0 and group 1

m4 <- lm(data$SBP~data$g0+data$g2+data$g3, data=data)
summary(m4)






# Install one time the "car" package. 
# install.packages('car')
# Re-run ANOVA adjusting for Age
library(car)

# ANCOVA
data$group = factor(data$group)

# Now we run ANOVA with adjusting for age 
mdl = lm(SBP ~ group + age,data=data)

#mdl = lm(SBP ~ group,data=data)
anova(mdl)

summary(mdl)
#No difference between heavy smokers and any other group after adjusting for age

Anova(lm(data$SBP ~ data$group + data$age), type=3)

# Type should be type 3 sum of squares. 
# It defines the different types of sums of squares.  Read more about it here 

# https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/ 
  

# https://cran.r-project.org/web/packages/emmeans/vignettes/transition-from-lsmeans.html
 #install.packages('emmeans')

library(emmeans)
my.model<-lm(SBP~group+age,  data = data)

emm_options(contrasts=c("contr.treatment", "contr.poly"))
emmeans(my.model, specs = "group")

# or with pairwise comparisons 
emmeans(my.model, specs = "group", contr = "pairwise")

#summary of my model shows differences between ls means



#Here is a clearer interpretation

#Compute ls mean by hand

meanAge = mean(age)

summary(my.model)

#Average blood pressure for someone of average age in the reference group
avg = 1.00950*meanAge + 89.49413

#For former smokers
avg - 0.57663


ls_light = avg + 0.07837
ls_former = avg-0.57663
ls_never = avg+0.29846

#reference group
ls_heavy = avg

#This is the least-squares mean
ls_heavy - ls_never
