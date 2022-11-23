# Outliers and influence points
N = 200
minX = 0
maxX = 10
x = runif(N,minX,maxX)

y = 1.3*x + rnorm(N)


df_original = data.frame(cbind(x,y))


#Uncomment the next two lines to add two outliers in the y direction
#x = c(x,9.5,1)
#y = c(y,0,-12)

#The y-axis outliers should have little effect on the regression line

#Uncomment the next two lines to add two outliers in the x direction
x = c(x,minX-30,maxX+30)
y = c(y, 0,0)

#The x-axis outliers have a substantial effect on the regression line

df_outlier = data.frame(cbind(x,y))

#Fit model without outliers
mdl_original = lm(y ~ x, data=df_original)

#Fit model with the outliers
mdl_outlier = lm(y ~ x,data=df_outlier)

# Visualize the change in the regression line
require(ggplot2)

# Original plot with regression line
g = ggplot(data=df_original,aes(x=x,y=y)) + 
  geom_point(color="red") + 
  geom_abline(slope=mdl_original$coefficients[2],intercept=mdl_original$coefficients[1],linetype="dashed") + 
  theme_bw() + xlim(-30,40)


df_outlier$outlier = factor(c(rep("No",N),rep("Yes",2)))
df_outlier$outlier_size = c(rep(1,N),rep(2,2))

# Plot with outlier
h = ggplot(data=df_outlier,aes(x=x,y=y,color=outlier,size=outlier_size)) + 
  geom_point() + 
  scale_size_continuous(range = c(2,4)) + 
  geom_abline(slope=mdl_outlier$coefficients[2],intercept=mdl_outlier$coefficients[1],linetype="dashed") + 
  theme_bw()+
  theme(legend.position="none") + xlim(-30,40)


require(gridExtra)
grid.arrange(g,h,nrow=1)


#df = df_outlier[1:200,]
# Check influence plot of the outlier model
plot(mdl_original,which=5)

plot(mdl_outlier,which=5)

