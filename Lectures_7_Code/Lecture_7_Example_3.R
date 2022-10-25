
# Can linear regression work for us when the relationship doesn't look like a line?

# Yes! With transformations.

# Let us generate example data 
set.seed(123456789)


# Create uniformly distributed data
x <-runif(500,min=0,max=500)

# y = random coefficient * x + normally distributed error ^ 3
# The error makes the distribution of y exponential (most values are small)
y <- (runif(1, 0.1, 2.5) * x+ rnorm(50, mean = 2, sd = 12))^2

# The plot shows that this data does not have a linear pattern. 
plot(x, y)


m1 <- lm(y~x)
summary(m1)
abline(m1)

# We see also in the residual plot that the data does not show a linear pattern. 
plot(x , resid(m1))



# Now, let us transform the data to a new form. 
# We calculate the square root of y 
y1 <- sqrt(y)

# Now, data has a linear pattern. 
plot(x, y1)

m2 <- lm(y1~x)
summary(m2)

# Residual plot also shows randomly distributed errors (good!) 
plot(x , resid(m2))

# It is important to know that after transformation, 
# we need to interpret all of the prediction results using the transformation function. 


x_new <- runif(500,min=0,max=500)

df = data.frame(x_new)
colnames(df) = c("x")

df$y_hat = predict(m1,df)

#Remember to convert back to original scale of y
df$pred_y = df$y_hat^2

plot(df$x,df$pred_y)

