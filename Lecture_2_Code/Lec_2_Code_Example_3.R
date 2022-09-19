
### Code examples for demonstrating how outliers appear in the data and how boxplots detect them ###
days <- read.csv("Datasets/Example_Data.csv", header = FALSE)


# To convert and flatten data from a data frame you can use the unlist command. 
myDays = as.vector(as.matrix(days))


myDays <- unlist(days, use.names=FALSE)
# myDays is now a simple vector data type that I can work with. 

# Now plot the boxplot
boxplot(myDays)

# add manually an outlier here 
myDays <-c(myDays, 95)

# Now plot the boxplot
boxplot(myDays)

summary(myDays)

# you can turn the outlier detection off by setting outline=FALSE
boxplot(myDays, outline=FALSE)

#same example using ggplot2
require(ggplot2)

myDays = data.frame(myDays)
colnames(myDays) = "days"

#Boxplot with y-axis corresponding to the "days" variable
#X-axis from -1 to 1
ggplot(data=myDays,aes(y=days)) + 
  geom_boxplot() + 
  xlim(-1,1) + 
  theme_bw()

#Same command but ignoring outliers - and limiting y-axis to 35
ggplot(data=myDays,aes(y=days)) + 
  geom_boxplot(outlier.shape=NA) + 
  stat_boxplot(geom='errorbar') +
  ylim(0,35) + 
  xlim(-1,1) + 
  theme_bw()


#This is the outlier detection algorithm that "boxplot" uses in R
#1.5 times the interquartile range above the 75th percentile and below the 25th percentile
limit =  quantile(myDays$days,0.75) + 1.5*IQR(myDays$days)

lower = quantile(myDays$days,0.25) - 1.5*IQR(myDays$days)

#Remove detected outliers from the dataset
days_no_outlier = myDays$days[myDays$days < limit & myDays$days > lower]
