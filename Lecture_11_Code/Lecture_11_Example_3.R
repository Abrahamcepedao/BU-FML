# Simulations to show the population proportion test and how it is affected by some of the issues brought about
# number of samples in each group too small (n * p0)
# Sample size is too large a proportion of the population

# Population size

pop_size = 10000

# True proportion
p = 0.995

# Create dataset based on population size and true proportion
outcomes = c(rep(1,p*pop_size),rep(0,(1-p)*pop_size))

# Create table of proportions
prop.table(table(outcomes))

# Sample size
n = 20

# Draw a sample of size n
tmp = sample(outcomes,n)

# Check sample proportion
prop.table(table(tmp))

# Compute sampling distribution of p hat

# number of trials
B = 5000

p_hats = c()
for(i in 1:B)
{
  p_hats = c(p_hats,sum(sample(outcomes,n)==1)/n)
}


# Is the predicted standard error accurate?
sd(p_hats)
pred_sd = sqrt((p*(1-p))/n)

# Is the sampling distribution normally distributed with a mean close to p?
hist(p_hats)
mean(p_hats)
