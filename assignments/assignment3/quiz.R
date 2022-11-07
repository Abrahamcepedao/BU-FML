#value for t distribution
qt(0.975, df=29)

#cor.test()
#cor.test(data$explanatoryvariable, data$responsevariable,alternative=[alternative], method=[method], conf.level=[confidence level])

#linear model
#lm(data$responsevariable ~ data$explanatory)


# Calculating probability from F-statistics
# Use pf() function to calculate the area to the left of a given F-statistic
#> pf([F statistic], df1=[degree of freedom of the numerator], df2=[degree of freedom of the denominator])
# Calculating F-statistics from probability
# Use qf() function to calculate F-statistic with the specifies area to theleft
#> qf([probability], df1=[degree of freedom of the numerator], df2=[degree of freedom of the denominator])
#> 
#>  # getting the p-value out of F-value
#> pf(18.51, df1=1, df2=2)
#[1] 0.9499929  # (the area to the left)
#> pf(18.51, df1=1, df2=2, lower.tail = F) # (area to the right)
#[1] 0.05000706
# getting the F value
#> qf(0.975, df1=1, df2=29, lower.tail = T)
#> 
#> 
#> # Calculating probability from T-statistics
#> qt(0.975, df=11, lower.tail = T)
#> pt(2.04523, df=29)
#> 
#> 
#> 
#> 
data <- read.csv("ds_salaries.csv")
data <- subset(data, data$work_year==2022)

print(mean_en <- mean(subset(data, data$experience_level == "EN")$log_salary))
print(sd_en <- sd(subset(data, data$experience_level == "EN")$log_salary))
print(mean_mi <- mean(subset(data, data$experience_level == "MI")$log_salary))
print(sd_mi <- sd(subset(data, data$experience_level == "MI")$log_salary))
print(mean_se <- mean(subset(data, data$experience_level == "SE")$log_salary))
print(sd_se <- sd(subset(data, data$experience_level == "SE")$log_salary))
print(mean_ex <- mean(subset(data, data$experience_level == "EX")$log_salary))
print(sd_ex <- sd(subset(data, data$experience_level == "EX")$log_salary))

#Next, use the correct statistical test to answer the question, 
#"Do entry level data scientists (EN group) make less than experienced 
#data scientists (EX group)?" Test using a significance threshold of 0.05 
#using the 5-step procedure discussed in class (State hypothesis, choose test s
#tatistic, state decision rule, calculate test statistic, state whether we reject 
#or fail to reject the null, and finally conclude). Be sure to use the log-transformed 
#salary variable in your analysis. (6 points)

#Step 1
#H0 ∶ μ1 = μ2 (the mean of EN group is equal to EX group)
#H1 ∶ μ1 < μ2 (the mean of EN group is less than to EX group)
#α = 0.05
n1 = length(subset(data, data$experience_level == "EN")$log_salary) # N1 = 21
n2 = length(subset(data, data$experience_level == "EX")$log_salary) # N2 = 13
n = n2 # n2 < n1
df = n-1
#df = 12
#Step 2
#Use t-test as n is small and population sd is unknown

#step3
qt(0.95,12)
#Critical value = 1.782288
#Decision Rule: Reject H0 if t ≥ 1.782288
#Otherwise, do not reject H0

#step 4
t.test(subset(data, data$experience_level == "EN")$log_salary, subset(data, data$experience_level == "EX")$log_salary,
       alternative="less", conf.level=0.95)

#step 5
#t = -6.0713, df = 31.466, p-value = 4.721e-07

#
data2 <- read.csv("Health_Information.csv")
attach(data2)

library(ggplot2)
ggplot(data2, aes(x=Bone_Density)) + geom_histogram(fill="blue") +
  xlab("Bone Density") + 
  ylab("Frequency") +
  ggtitle("Distribution of Bone Density") + theme_bw(base_size=14) 

mean(Bone_Density)
median(Bone_Density)

ggplot(data2, aes(x=Age)) + geom_histogram(fill="blue") +
  xlab("Ages (years)") + 
  ylab("Frequency") +
  ggtitle("Distribution of Age") + theme_bw(base_size=14) 

mean(Age)
median(Age)

ggplot(data2,aes(x=Age,y=Bone_Density))+geom_point(aes(alpha=0.5))+
  xlab("Age (years)") + 
  ylab("Bone Density") +
  ggtitle("Scatterplot of Bone Density vs. Age") + theme_bw(base_size=14) 

male <- subset(data2, data2$Sex == "Male")

female <- subset(data2, data2$Sex == "Female")

ggplot(male,aes(x=Age,y=Bone_Density))+geom_point(aes(alpha=0.5))+
  xlab("Age (years)") + 
  ylab("Bone Density") +
  ggtitle("Scatterplot of Bone Density vs. Age in Males") + theme_bw(base_size=14) 


ggplot(female,aes(x=Age,y=Bone_Density))+geom_point(aes(alpha=0.5))+
  xlab("Age (years)") + 
  ylab("Bone Density") +
  ggtitle("Scatterplot of Bone Density vs. Age in Females") + theme_bw(base_size=14) 


print(cor_m <- cor(male$Age,male$Bone_Density))
print(cor_f <- cor(female$Age,female$Bone_Density))
#cor_f > cor_m
print(model <- lm(female$Age~female$Bone_Density))

anova(model)
summary(model)

#R^2
sprintf("R^2 = %.6f", summary(model)$r.squared)

#68 year old
print(res <- model$coefficients[1] + model$coefficients[2]*68)
#1.084588 

