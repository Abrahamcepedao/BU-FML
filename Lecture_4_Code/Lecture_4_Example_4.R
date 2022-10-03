#Comparison of t-test and z-test in artificial scenarios

#require(asbio)
require(BSDA)
#Define the population distribution
pop_size = 10000000


#skewed
#tmp = rbeta(pop_size,2,9)

tmp = ifelse(runif(pop_size)<0.5,rnorm(pop_size,-1.5,1),rnorm(pop_size,1.5,1))


hist(tmp)

#normal
#tmp = rnorm(1000000)

#uniform
#tmp = runif(1000000)

#Define the sample size
ss = 50

#Number of iterations to thestd
num_iter = 1000


#How to get standard deviation for z-test, from the population? or from the sample?
sample_sd = T


#Effect size to test
beta = 0.5

#Alpha
alpha = 0.05


sigma = sd(tmp)
hist(tmp)



#Calculate population mean
m2 = mean(tmp)

#Define alternate mean to test to calculate power of the test
alt_mean = m2+ beta

ps = c()
p_alt = c()

p_z = c()
p_z_alt = c()


#Calculate p-value under the null or alternate scenarios for many samples from the population
for(i in seq(1,num_iter))
{

  smp = sample(tmp,ss)
  if(sample_sd)
  {
    sigma = sd(smp)
  }
  ps = c(ps,t.test(smp,mu=m2)$p.value)
  p_alt = c(p_alt,t.test(smp,mu=alt_mean)$p.value)
  
  p_z = c(p_z,z.test(smp,mu=m2,sigma.x=sigma)$p.value)
  p_z_alt = c(p_z_alt,z.test(smp,mu=alt_mean,sigma.x=sigma)$p.value)
}


hist(ps)
hist(p_alt)

hist(p_z_alt)


#Power of the test = when there is an effect, how often do we detect it?
power_t = sum(p_alt < alpha)/length(p_alt)

#When there is not an effect, how often do we fail to reject?
null_t = sum(ps < alpha)/length(ps)

#1 - null gives the false detection rate, this should be equal to alpha


#Same calculations for the z-test
power_z = sum(p_z_alt < alpha)/length(p_z_alt)
null_z = sum(p_z < alpha)/length(p_z)


