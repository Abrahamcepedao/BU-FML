#This illustrates the difficulty in deciding what to "adjust" for

#Example of collider bias

#X -> Z <- Y
x = rnorm(100)
y = rnorm(100)
z = 2*x - 3*y + rnorm(100)

df = data.frame(cbind(x,y,z))
cor(df$x,df$y)
cor(df$x,df$z)
cor(df$y,df$z)

#Here are simple linear regression coefficients
mdl1 = lm (y ~ x,data=df)

#Basically no predictive value for y
summary(mdl1)


#Collider bias makes it look like x and y are associated
mdl2 = lm(y ~ x + z,data=df)
summary(mdl2)


#In this example, a confounder makes it look like
#Two independent variables are associated

#confounder
z = rnorm(100)
x = 5*z + rnorm(100,sd=3)
y = -3*z + rnorm(100,sd=1.5)

data = data.frame(cbind(x,y,z))

#This model is confounded
mdl1 = lm(y ~ x,data=data)
summary(mdl1)

#This model corrects for the confounding
#x is not associated with y
mdl2 = lm(y ~ x + z,data=data)
summary(mdl2)
