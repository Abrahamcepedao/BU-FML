### Central Limit Theorem and Advanced Histograms #### 

### Central Limit Theorem ####

###Let's say we have a normally distributed random variable with mean 10 and sd 2 ###
pop_mean = 10
pop_sd = 2
num_samples = 50000
var = rnorm(num_samples,mean=pop_mean,sd=pop_sd)

### The central limit theorem says if n > 30 the distribution of sample means approaches normal for larger N ###

### With mean = population mean ###

### And standard deviation equal to population sd / sqrt (sample size) ####

tmp = sample(var,30)
 
### First let's draw repeated samples of size 20 from our distribution

#Sample size
sample_size = 10

#Number of samples to take
num_draws = 10000

#Save the mean of each sample
means = c()


for(i in 1:num_draws)
{
  #Generate sample
  tmp = sample(var,sample_size)
  
  #Calculate mean
  means = c(means,mean(tmp))
}

#The means become normally distributed when sample size increases
hist(means)

#With mean equal to the population mean of the distribution
mean(means)
print(pop_mean)

#And standard deviation equal to population standard deviation / square root of n
sd(means)

sd(var) / sqrt(sample_size)


###Central limit theorem on a uniform distribution

#Generate a uniformly distributed random variable
num_samples = 50000
min_val = 0
max_val = 100
var = runif(num_samples,min=min_val,max=max_val)

#Plot a histogram
hist(var)

#Get population mean and standard deviation
pop_mean = mean(var)
pop_sd = sd(var)

#Sample size for repeated samples
sample_size = 30

#Number of samples to draw
num_draws = 10000

#Calculate the sampling distribution of the mean

means = c()
for(i in 1:num_draws)
{
  tmp = sample(var,sample_size)
  means = c(means,mean(tmp))
}

hist(means)


#This works when N is big enough!
mean(means)
print(pop_mean)

sd(means)

sd(var) / sqrt(sample_size)


### Advanced Histograms #### 

# There a lots of good data sets available in R. 
# You can get a list of them by using command data()
# data()

# You can read about each of them by using help command. 
?AirPassengers

# You can load them into the memory by using the command 
data("AirPassengers")

# first take a look at the histogram with no additional parameters. 
hist(AirPassengers)

?hist

# axes	logical. If TRUE (default), axes are draw if the plot is drawn.
# We set here axes to FALSE 
# We make a vector for the lables of x-axis. 
x <- seq(100, 700, by = 50)


#Generate a histogram and save the result
results <- hist(AirPassengers, breaks = x, 
                axes = FALSE, 
                main="Histogram for Air Passengers 1949-1960", 
                xlab = "Monthly Airline Passenger Numbers",  
                border="blue", 
                col="green", 
                ylim=c(0,25), xlim = c(100, 700)  )

# You can read some of the properties of histogram.
print(results)

# Read the documentation about axis 
?axis

# Now, we make the x-asis labels
axis(1, las=1, at = x, labels=x)

# Now, we make a vector for the y-axis lables based on our own requirments. 
ylables<- c(0, 2, 4, 5, 10, 13, 15, 20, 25)

# And this is the y-axis labels.
# axis(2, las=1, at = ylables, labels=ylables,  cex.axis = 1.2, font = 2)

# Or you can use the counts of your histogram result values 
axis(2, las=1, at = results$counts, labels=results$counts,  cex.axis = 1.2, font = 2)

# Here, we make some horizontal lines on the histogram. Just for fun :) 
# In the case that you want to have some horizontal lines on the histogram. 
abline(h = 4, col = "red" )

# a Dashed Line 
# abline(h=13,  col = "blue", lwd=1, lty=3)

abline(h=13,  col = "blue", lwd=3, lty="dashed")

# Values for lty are the strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash". 
# Alternatively, the numbers 0 to 6 can be used (0 for "blank", 1 for "solid", ...).

require(ggplot2)

#ggplot version of the same command

df = data.frame(AirPassengers)
colnames(df) = c("N")
ggplot(data=df,aes(x=N)) + 
  geom_histogram(color="blue",fill="green",breaks=seq(100,700,by=50)) + 
  theme_bw() + 
  xlab("Monthly Airline Passenger Numbers") +
  ylab("Frequency") +
  ggtitle("Histogram for Air Passengers 1949-1960") + 
  geom_hline(yintercept=13,color="blue",linetype="dashed") +
  geom_hline(yintercept=4,color="red") + 
  ylim(0,24) + 
  scale_y_continuous(breaks=c(0,4,8,13,21,24))
  

