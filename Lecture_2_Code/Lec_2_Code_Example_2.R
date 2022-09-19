#### Code examples for handling normal distributions and solving problems related to normal distributions ####

### Generate 1,000 samples from a normal distribution with mean 3500 and standard deviation 500

pop_mean = 3500
pop_sd = 500
num_samples = 1000
babyData <- rnorm(num_samples, mean = pop_mean, sd=pop_sd)

###Confirm this is normally distributed ###
hist(babyData)

# Here is the second plot command to visualize the probability density
plot(density(babyData),lwd=3,col="red",main="")

###What is the probability a baby weighs less than 4000 grams?? ###

#Compute Z-Score

sample_mean = mean(babyData)
sample_sd = sd(babyData)

#Calculate the z-score based on the mean and standard deviation for 4,000
zscore = (4000-sample_mean)/sample_sd

# Get probability less than Z from Standard Normal Distribution
pnorm(zscore)

# OR can do this directly using the mean and sd parameters
pnorm(4000,mean=pop_mean,sd=pop_sd,lower.tail=F)

?pnorm

### What is the value of the normal density curve at 3500 in the plot?
dnorm(4000,mean=pop_mean,sd=pop_sd)

###What percent of infants weigh between 3750 and 3250 grams?
pnorm(3750,mean=pop_mean,sd=pop_sd) - pnorm(3250,mean=pop_mean,sd=pop_sd)

###What percent of infants weigh ABOVE 4000 grams
pnorm(4000,mean=pop_mean,sd=pop_sd,lower.tail=T)

###What value captures x % of the distribution? 
#What value of newborn weight makes the P(X <= x) == p

#mean captures 50% of the distribution
qnorm(0.5,mean=pop_mean,sd=pop_sd)

#Default mean = 0, and default sd = 1

#68-95-99.7 rule says that 84% of observations should be less than z-score of 1
qnorm(0.84) #output is close to 1


#What value is the 75th percentile of this distribution?
qnorm(0.75,mean=pop_mean,sd=pop_sd)

pnorm(3837.245,mean=pop_mean,sd=pop_sd)


#### Let's compute one of these answers, empirically from our data?

### What percent of newborns weigh less than 4000 grams?

###Remember the pnorm answer uses an "idealized" view of the data
pnorm(4000,mean=pop_mean,sd=pop_sd)

###How many of our babies actually weighed less than 4000 grams?
babyData < 4000

#Convert to the number <4000
sum(babyData < 4000)

#Divide by total number of babies to get %
sum(babyData<4000) / length(babyData)



###These functions require normally distributed data! They won't work with other distributions

# Uniformly distributed random variable
babyData = runif(1000,min=2000,max=5000)

# Get summary stats
hist(babyData)
m = mean(babyData)
s = sd(babyData)
print(m)
print(s)

###Assume normality and find percent of babies weighing less than 4000 grams
pnorm(4000,mean=m,sd=s)

# Compute actual numbers from the data - very different from the pnorm result!
sum(babyData<4000)/length(babyData)

# No matter how big our sample size is, 
#these values will continue to be off
