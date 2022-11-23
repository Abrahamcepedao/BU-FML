# Teaching module to better understand multiple testing

install.packages("TeachingDemos")
install.packages("tkrplot")
require(TeachingDemos)

##Explore the multiple testing problem
TeachingDemos::run.Pvalue.norm.sim()

#n = sample size
# mu = mean of the simulated data
# sigma = standard deviation of the simulated data
# B = number of datasets (tests) to run
# alpha = significance level

#mu0 = hypothesized mean of the data
#sigma0 = hypothesized standard deviation of the data
