#Detailed look at power analysis and power calculations!

require(asbio)
pop_sigma = 0.5
alpha = 0.05

power = 0.8
effect = 0.2
out = power.z.test(sigma=pop_sigma,power = power, alpha=alpha,
                   effect=effect,test="two.tail")

#If we are forced into using a particular sample size, based on constraints
#A power curve can help evaluate results if the test comes out as null
effects = seq(0,1,0.01)
powers = c()
for(i in 1:length(effects))
{
  powers = c(powers,power.z.test(sigma=pop_sigma,n=N,alpha=alpha,
                                 effect=effects[i],test="two.tail")$power)
  
}

plot(effects,powers)
#If the result is null, then the true effect size is likely smaller than 0.2



#On the flip side, 
#if we are submitting a project proposal, we want to be sure that 
#we collect enough samples for our hypothesized effect size

#Typically researchers aim for ~80% power
require(asbio)
pop_sigma = 0.5
alpha = 0.05
effect = 0.3
power = 0.8
out = power.z.test(sigma=pop_sigma,power=power, alpha=alpha,
                   effect=effect,test="two.tail")
out$n
#We need 22 samples to detect a difference in 0.3 from the null mean
#with 80% power

#This is reliant on all assumptions of z-test being TRUE!!!

#For the t-test we need 24 samples!
power.t.test(delta=effect,sd=pop_sigma,power=0.8,type=c("one.sample"))
