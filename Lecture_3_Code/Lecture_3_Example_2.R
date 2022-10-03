#### This example illustrates how a 95% confidence interval works ####

### From Exit Question # 6 ###
n = 50 # Define the sample size of our random sample
sigma = 2 # Define the poplation standard deviation
mu = 3 #Define the population mean
confidence_level = 0.99 # Define the confidence level

#Calculate the z critical value based on 95% confidence
z_crit = qnorm((1-confidence_level)/2,lower.tail=F)

# Generate a sample of size n#
sample_taken = rnorm(n,mean=mu,sd=sigma)

# Compute the sample mean #
x_bar = mean(sample_taken)

# compute high and low "ends" of the confidence interval based on the critical value#
m = z_crit*sigma/(sqrt(n))
high_ci = x_bar + m
low_ci = x_bar - m

# Is the population mean inside the confidence interval? #
inside = ifelse(mu > low_ci & mu < high_ci,1,0)


# Let's repeat this and see 
# For what percent of random samples is the population mean inside the CI? 

num_iterations = 100000
inside = c()
for(i in 1:num_iterations)
{
  # Generate a sample #
  sample = rnorm(n,mean=mu,sd=sigma)
  
  # Compute sample mean #
  x_bar = mean(sample)
  
  # compute high and low "ends" of the confidence interval #
  m = z_crit*sigma/(sqrt(n))
  high_ci = x_bar + m
  low_ci = x_bar - m
  
  # Is the population mean inside the confidence interval? #
  inside = c(inside,ifelse(mu > low_ci & mu < high_ci,1,0))
  
}

#What percent of the time did the CI contain the population mean? 
# - about the confidence level
sum(inside)/length(inside)

