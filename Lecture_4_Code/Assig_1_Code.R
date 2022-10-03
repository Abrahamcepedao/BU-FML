#### Code solutions for Assignment 1#####

#Read the data
library(readr)
data <- read_csv("ModulHW1_Data.csv", 
                 col_names = FALSE)

#Convert to vector
data = as.numeric(unlist(data))

#create histogram


hist(data,breaks=seq(0,max(data),1),xlab="Days",
     main="Histogram of Duration of Hospital Stays")

#OPTIONAL: THIS IS HOW TO DO THIS IN GGPLOT
gg_data = data.frame(data)

require(ggplot2)



g = ggplot(gg_data,aes(x=data)) +
  geom_histogram(color="red",fill="blue") + xlab("Days") + 
  ggtitle("Histogram of Duration of Hospital Stays") + 
  theme_bw()  + ylab("Frequency") + 
  scale_x_continuous(breaks = c(0,7,10,11,13,15))
g

#Check for outliers
boxplot(data)

# The boxplot flags several points as outliers, 
# however these do not appear to be significant outliers


#Get summary statistics
summary(data)

#get standard deviation
sd(data)

IQR(data)

#Skewed distribution- median is best summary
# IQR is best measure of spread (standard dev is ok too)


# Assume mean of 5 and sd 3 
m = 5
std_dev = 3

#What percent are in the hospital for less than a week?
pnorm(7,mean=m,sd=std_dev)


sum(data<=7)/length(data)

#74.8% are in the hospital for less than a week

# 10 person-sample, what is the probability that the mean is greater than 7 days?
n = 10
std_dev_sample = std_dev/sqrt(n)

pnorm(7,mean=5,sd=std_dev_sample,lower.tail=F)

z_score = (7 - 5)/0.94868

pnorm(z_score,lower.tail=F)


#The probability is 1.8% that the mean is greater than 7 days