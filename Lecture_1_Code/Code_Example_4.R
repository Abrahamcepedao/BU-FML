### SIMPLE R VISUALIZATIONS ###

### Reading data into R ###

###Change the file path to match your location
days <- read.csv("Example_Data.csv", header = FALSE)

### Data with a header
golf = read.csv("GolfBals.csv",header=T)


###Summary statistics on a particular variable

#Extract just the "distance" variable from a data frame
golf$distance

#Calculate summary statistics on this distance variable

#Mean
mean(golf$distance)

#Median
median(golf$distance)

#Minimum and maximum values
min(golf$distance)
max(golf$distance)

#25th, 50th, and 75th percentile thresholds
quantile(golf$distance)

#Measures of spread - Variance and standard deviation
var(golf$distance)
sd(golf$distance)


###Skewed Mean and median

###Generate a variable with an outlier value
variable = c(rep(30,50),rep(35,30),rep(40,20),rep(45,5),50,55,60,12000)

###Add some random noise to it
variable = variable + runif(length(variable),min=0,max=7)

variable

#variable2 doesn't have the outlier
variable2 = variable[-length(variable)]

###Basic histogram
hist(variable)
hist(variable2)

###Compute the means of the variable
#Heavily affected by the single outlier
mean(variable)
mean(variable2)

###Compute the median - no major effect of the single outlier
median(variable)
median(variable2)


#How does skew affect the mean vs median?

#First generate data with no skew
data = rnorm(50000)

install.packages("sn")
Yes
require(sn)

?dsn


#Generate data with skew
data2 = rsn(50000,xi=0,omega=5,alpha=20)
hist(data2)
hist(data)

#Without skew
mean(data)
median(data)

#With skew - note that the mean gets "pulled" towards the tail 
mean(data2)
median(data2)

#With-skew IQR tends to be a better measure of the center than sd
hist(data2)
summary(data2)
mean(data2)-sd(data2)
mean(data2)+sd(data2)



###Qualitative data summaries

##Class counts
table(golf$brand)

###Pie chart of class counts
pie(table(golf$brand))

###Barplot of class counts
barplot(table(golf$brand))


###Class frequencies
table(golf$brand)/nrow(golf)

prop.table(table(golf$brand))

###Barplot of class frequencies with titles
barplot(table(golf$brand)/nrow(golf),
        main="Golf Balls by Brand",
        xlab="Brand",
        ylab="Frequency")


#Another package to produce graphs is called ggplot
if(!require(ggplot2))
{
  install.packages("ggplot2")
}

#GGplot version
ggplot(data=golf,aes(x=brand,fill=brand)) + 
  geom_bar() + 
  theme_bw() +
  ggtitle("Golf Balls by Brand") + 
  xlab("Brand") +
  ylab("Frequency")


###Summary of golf data

#Convert the "brand" variable to a character string
golf$brand = as.character(golf$brand)
summary(golf)

####Histogram of golf data
hist(golf$distance)

#Specifying the number of breakpoints to use
hist(golf$distance,breaks=5) #- evenly spaced by default

#Manually specifying the breakpoints themselves
breaks = c(255,265,275,300)
hist(golf$distance,breaks = breaks)

#GGplot version
ggplot(data=golf,aes(x=distance)) + 
  geom_histogram(breaks=breaks) + 
  theme_bw()

###Boxplot of golf data
boxplot(golf$distance)

###Ggplot version of the boxplot
ggplot(data = golf,aes(y=distance)) +
  geom_boxplot() +
  xlim(c(-0.5,0.5)) +
  theme_bw() + 
  annotate("text",x=-0.5,y=285,label="Median")

#Learn R from within R using the swirl package
if(!require(swirl))
{
  install.packages("swirl")
}
swirl()
