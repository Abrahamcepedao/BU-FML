### Code examples for visualizing normal distributions ###

###Draw 100 samples from a normal distribution with mean 3500 and standard deviation 500
babyData <- rnorm(100, mean = 3500, sd=500)


# Create two plots side by cide 
# First we set the parameters, mfrow allows you to layout plots side by side
par(mfrow=c(1,2)) # Make device region 1 by 2

# Here the first plot which is a histogram 
hist(babyData, prob=TRUE, col="cyan", main="You Type Something Here")

# Line is not a plot command so that it goes to the previous plot command. 
lines(density(babyData),lwd=3,col="red")

# This puts the density function on a new plotS
plot(density(babyData),lwd=3,col="red",main="")



# add the title to the most recent plot 
title(main="Density Plot")



#ggplot version


#Load library, install.packages("ggplot2") 
if(!require(ggplot2))
{
  install.packages("ggplot2")
}

if(!require(gridExtra))
{
  install.packages("gridExtra")
}

#Create data frame 
df = data.frame(babyData)
colnames(df) = "weight"

#Legend.position = 0 removes the legend from the plot
g = ggplot(data=df,aes(x=weight,color="red")) + 
  geom_histogram() + 
  theme_bw() + 
  theme(legend.position = 0)


h = ggplot(data=df,aes(x=weight,color="red")) + 
  geom_density() + 
  theme_bw() + 
  theme(legend.position=0)

grid.arrange(g,h,nrow=1)



# And now set back the par to the originial setting. 
# This step is a good practice so that you set the par back to what it was. 
par(mfrow=c(1,1))


# We want to visualize the normal distribution curve and color some part of it. 

# Define a vector
x <- seq(from = -3, to = 3, length.out = 100)

# Apply the distribution density function to the vector
y <- dnorm(x)

# Plot it
plot(x, y, type="l")

# Shade an area from -1 to 1: define the area by specifying points along the outer edges and then use polygon function to fill the shape
xvalues <- x[x>=-1 & x<=1]
yvalues <- y[x>=-1 & x<=1]
region.x <- c(xvalues[1], xvalues, tail(xvalues, 1))
region.y <- c(0, yvalues, 0)
# print out the region.x to see what is inside. 
region.x
region.y

polygon(region.x, region.y, col="navy")


###Another way to visualize the normal distribution curve ###

# Define a vector
x <- seq(from = -3, to = 3, length.out = 100)

# Define a vector
curve(dnorm(x), xlim=c(-3,3), main='Normal Density')

# To shade the region represented by P(-3 < X < -2). 

# The first vertex we want for our polygon is (-3,0). 
cord.x <- c(-3)
cord.y <- c(0)

# The 2nd vertex will be (-3, f(-3)), f(-3) is the normal density evaluated at -3. 
cord.x <- c(cord.x, -3) 
cord.y <- c(cord.y, dnorm(-3)) 

# The 3rd and 4th vertices are (-2,f(-2)) and (-2,0)
cord.x <- c(cord.x, -2, -2)
cord.y <- c(cord.y, dnorm(-2), 0)

# Now, we can use the polygon function and color parts in skyblue color. 
polygon(cord.x, cord.y, col="skyblue")



# ggplot visualization
# Define a vector
x <- seq(from = -3, to = 3, length.out = 100)

# Apply the distribution density function to the vector
y <- dnorm(x)
df = data.frame(x,y)
ggplot(data=df,aes(x=x,y=y,color="red")) + 
  geom_line() +
  theme_bw()

