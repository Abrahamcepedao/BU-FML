
# Exploring R^2 a bit more and the potential issues!!#

# R^2 is determined by the variance of the outcome even when the linear model is the right one #
r2.0 <- function(sig,mse=F){
  x <- seq(1,10,length.out = 100)        # our predictor
  y <- 2 + 1.2*x + rnorm(100,0,sd = sig) # our response; a function of x plus some random noise
  if(mse)
  {
    return(sqrt(mean((y-x)^2)))
  }else
  {
    return(summary(lm(y ~ x))$r.squared)           # print the R-squared value
    
  }
}

sigmas <- seq(0.5,20,length.out = 20)
rout <- sapply(sigmas, r2.0)             # apply our function to a series of sigma values
mseout <- sapply(sigmas, r2.0,mse=T)             # apply our function to a series of sigma values


plot(rout ~ sigmas, type="b")
plot(mseout ~ sigmas, type="b")

# In practice, if we saw a situation like this, how would we fix it?

# R^2 can be high even when the linear model is not a good one #

set.seed(1)
x <- rexp(50,rate=0.005)                     # our predictor is data from an exponential distribution
y <- (x-1)^2 * runif(50, min=0.8, max=1.2)   # non-linear data generation
plot(x,y)				     # clearly non-linear
df = data.frame(cbind(x,y))
mdl = lm(y ~ x,data=df)
sqrt(mean((y - predict(mdl,df))^2))

summary(lm(y ~ x))$r.squared

plot(y,predict(mdl,df))



# The range of x can affect R^2 tremendously #

x <- seq(1,10,length.out = 100)
set.seed(1)
y <- 2 + 1.2*x + rnorm(100,0,sd = 0.9)
mod1 <- lm(y ~ x)
summary(mod1)$r.squared
sqrt(sum((fitted(mod1) - y)^2)/100) # Mean squared error
plot(x,y,ylim = c(0,12))


x <- seq(1,2,length.out = 100)       # new range of x
set.seed(1)
y <- 2 + 1.2*x + rnorm(100,0,sd = 0.9)
mod1 <- lm(y ~ x)
summary(mod1)$r.squared
sqrt(sum((fitted(mod1) - y)^2)/100)        # Mean squared error
plot(x,y,ylim=c(0,12))

