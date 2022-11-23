
#This verifies that the null distribution of beta follows a t-distribution with n-2 DoF

#Verify that B0 is distributed according to t-distribution with n-2 degrees of freedom
beta_0 = c()
ts = c()
n = 70
for(i in 1:1000)
{
  #This works regardless of the distribution of the independent/dependent variables
  #data = data.frame(cbind(rbeta(n,9,3),rnorm(n)))
  data = data.frame(cbind(runif(n,0,5),rbeta(n,9,3)))
  
  colnames(data) = c("X","Y")
  
  mdl = lm(Y ~ X,data=data)
  
  #Save slope
  beta_0 = c(beta_0,mdl$coefficients[2])
  
  #Computing standard error
  curr = sum((predict(mdl,data)-data$Y)^2)
  
  curr = curr / (n-2)
  
  denom = sum((data$X - mean(data$X))^2)
  
  se_curr = sqrt(curr / denom)
  
  #Compute t-statistic
  t_stat = mdl$coefficients[2] / se_curr
  
  ts = c(ts,t_stat)
}

#The observed t-statistics match the theoretical t-distribution with n-2 DoF
hist(ts,freq=F,ylim=c(0,1))
inds = seq(-40,40,1)
points(inds,dt(inds,n-2))

