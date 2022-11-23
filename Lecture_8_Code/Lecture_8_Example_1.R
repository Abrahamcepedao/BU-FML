# Residual plot diagnostic example #

# Sample Size 
N = 100

# Generate Non-linear data
x = runif(N,min=0,max=80)

# Quadratic data
y = (1.2*x + rnorm(N,sd=1.5))^2

# Log transformed data
#y = log(1.2*x + rnorm(N,sd=1.5))

require(ggplot2)
#x = c(x,200)
#y = c(y, 1500)

df = data.frame(cbind(x,y))



#Visualize the data
ggplot(data=df,aes(x=x,y=y)) + geom_point() + theme_bw()

# SLR model
mdl = lm(y ~ x, data=df)

# Generate prediction and residual variable
df$prediction = predict(mdl,df)

df$residuals = df$y - df$prediction

# Plot of regression line
g = ggplot(data=df,aes(x=x,y=y)) + 
  geom_point() + 
  theme_bw() +
  geom_abline(slope=mdl$coefficients[2],intercept = mdl$coefficients[1])

# Plot of residuals
h = ggplot(data=df,aes(x=y,y=residuals)) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept=0,linetype="dashed")

require(gridExtra)

grid.arrange(g,h,nrow=1)


plot(mdl)


# Multiple linear regression example
N = 1000
w = runif(N,min=0,max=5)
x = runif(N,min=0,max=5)
y = runif(N,min=0,max=5)
z = runif(N,min=0,max=5)

w_coef = runif(1,min=-1.5,max=1.5)
x_coef = runif(1,min=-1.5,max=1.5)
y_coef = runif(1,min=-1.5,max=1.5)
z_coef = runif(1,min=-1.5,max=1.5)

#Uncomment this line for Linear data
#target = w_coef*w + x_coef*x + y_coef*y + z_coef*z + rnorm(N)

#Uncomment this line for Non-linear data
target = (w_coef*w + x_coef*x + y_coef*y + z_coef*z + rnorm(N))^2



df = data.frame(cbind(w,x,y,z,target))

require(GGally)

#df$target = sqrt(df$target)
ggpairs(df)

# MLR Model
mdl = lm(target ~ w + x + y + z,data=df)

# Generate prediction and residual variable
df$prediction = predict(mdl,df)

df$residuals = df$target - df$prediction

# Plot of residuals
ggplot(data=df,aes(x=prediction,y=residuals)) + geom_point() + theme_bw() + geom_hline(yintercept=0,linetype="dashed")


plot(mdl)


# When the distribution of the target appears exponential
hist(df$target)
df$target_transform = sqrt(df$target)
hist(df$target_transform)



# MLR Model
mdl = lm(target_transform ~ w + x + y + z,data=df)

# Generate prediction and residual variable
df$prediction = predict(mdl,df)

df$residuals = exp(df$target_transform) - exp(df$prediction)

df$residuals = scale(df$residuals)


# Plot of residuals
ggplot(data=df,aes(x=prediction,y=residuals)) + geom_point() + theme_bw() + geom_hline(yintercept=0,linetype="dashed")


