###Why does logistic regression model the "log odds" 
#instead of the probability 
#or the binary variable directly?

#Generate all probabilities from 0 to 1
x = seq(0.0001,0.9999,0.0001)

#Calculate the odds for each probability
y = (x/(1-x))

#Create a data frame from probabilities, odds
df = data.frame(cbind(x,y))

require(ggplot2)

#Plot relationship between probability and odds
#No probability because unbounded
#Similarly odds are strictly positive
ggplot(data=df,aes(x=x,y=y)) + geom_point() + 
  xlim(0,1) + ylim(0,10) + theme_bw() + 
  xlab("Probability") + ylab("Odds")

#Calculate log odds
y = log((x/(1-x)))

df2 = data.frame(cbind(x,y))

require(ggplot2)

#Probability and log odds are linearly associated until the extremes
ggplot(data=df2,aes(x=x,y=y)) + geom_point() + 
  xlim(0,1) + ylim(-10,10) + theme_bw() + 
  xlab("Probability") + ylab("Log Odds")

#Flipping this around generates the sigmoid curve - 
#predicting log odds bounds the probability
ggplot(data=df2,aes(x=y,y=x)) + 
  geom_point() + ylim(0,1) + 
  xlim(-10,10) + theme_bw() + ylab("Probability") + xlab("Log Odds")

