df = read.csv("Datasets/A2.csv",header=T)

#IQR as measure of variability
#Slight left skew for NP


#Power curve assignment
#Q2 

#Q1
require(ggplot2)

summary(df$Calories[df$Group=="P"])
summary(df$Calories[df$Group=="NP"])


ggplot(data=df,(aes(x=Calories,fill=Group))) + 
  geom_density(position="identity",alpha=0.5) + 
  theme_bw() + 
  xlim(0,700)

ggplot(data=df,(aes(x=Calories,fill=Group))) + 
  geom_histogram(position="identity",alpha=0.5,bins=15) + 
  theme_bw() + 
  xlim(0,700)


df2 = df[df$Group=="NP",]
ggplot(data=df2,(aes(x=Calories,fill=Group))) + 
  geom_histogram(position="identity",alpha=0.5,bins=15) + 
  theme_bw() + 
  xlim(0,700)

summary(df$Calories[df$Group=="P"])

summary(df$Calories[df$Group=="NP"])

#Q2

#1. null mean = 425, alternative mean does not equal 425
#2. one-sample t-statistic
#3 Decision rule

N = sum(df$Group=="P")
crit_t = qt(0.975,N-1)
#Reject if |t| > 2.06

#4. 0.303 t statistic

t_stat = t.test(df$Calories[df$Group=="P"],mu=425)$statistic

#5. Conclusion. Fail to reject the null hypothesis. There is insufficient evidence to suggest the mean of the participants is different than 425

#Power curve - we were super unlikely to detect this effect, unless it was massive

effects = seq(1,100,0.1)
powers = c()

for(i in effects)
{
  

out = power.t.test(n=N,delta=i,sd=sd(df$Calories[df$Group=="P"]))
powers = c(powers,out$power)
             
}

plot(effects,powers)

#Q3 
t.test(df$Calories[df$Group=="P"],conf.level=0.90)
#387.77 - 475.03, 95% chance that the true mean falls in this range

#Q4

#1. Two-sample t-test, null difference in means is 0, alternative, difference in means is non-zero

#2. two-sample t-statistic

#3. Decision rule, approx 20 DoF
crit_t = qt(0.975,20)

#4. calculate t-stat
t.test(df$Calories[df$Group=="P"],df$Calories[df$Group=="NP"])

#2.82

#5. Reject the null, 
#there was a difference in mean weight loss 
#between the participants and non-pariticpants 



#5) Sample size was reasonable relative to the skew of the distributions
#No very strong outliers
#Difficult to assess if samples were measured the same way - appears so
#Samples are independent from different groups



5.2 + 1.96*1.5/7
5.2 - 1.96*1.5/7
