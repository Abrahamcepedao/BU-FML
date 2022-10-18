#Simple Scatterplot, correlation, and simple linear regression

# Create a vector for heights
height <- c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)

# Create a vector for bodymass 
bodymass <- c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

# Create a scatterplot 
plot(bodymass, height)

# A better scatterplot #pch = point type | cex = font size
plot(bodymass, height, 
     pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", 
     xlab = "BODY MASS (kg)",
     ylab = "HEIGHT (cm)")

#GGplot for scatter plots
require(ggplot2)
df = data.frame(cbind(bodymass,height))

ggplot(data=df,aes(x=bodymass,y=height)) + 
  geom_point()

ggplot(data=df,aes(x=bodymass,y=height)) + 
  geom_jitter(width=0.5,height=3) + 
  geom_abline(intercept=100,slope=1) + 
  theme_bw()

# Correlation 
cor(bodymass, height)

cor(height, bodymass)
height[1] = NA

cor(height,bodymass)
#Use only pairwise observations where both variables have data
cor(height,bodymass,use="pairwise.complete.obs")

#Significance Test to the population
cor.test(bodymass,height)

# A simple linear regression
mdl = lm(height ~ bodymass)
summary(mdl)


plot(bodymass,height)

# A line on the past plot
abline(98.0054, 0.9528)



# A better approach is to use variables and pass data to other functions 
m <- lm(height ~ bodymass)

# GGplot scatter plot including the regression line
ggplot(data=df,aes(x=bodymass,y=height)) + 
  geom_point() + 
  geom_abline(intercept = m$coefficients[1],
              slope=m$coefficients[2]) + 
  theme_bw()


#Using the model to predict new heights from new bodymass
mdl = lm(height ~ bodymass,data=df)
summary(mdl)

bodymass_new = c(180,190,200,300,10,24,50,80,88,90)

df2 = data.frame(bodymass_new)

#The x axis variable must have the same name in this new data frame
df2$bodymass = df2$bodymass_new

colnames(df2)

colnames(df2) = gsub("_new","",colnames(df2))

# This is our predicted heights for each new value of bodymass
new_heights = predict(mdl,df2)

df2$predicted_height = new_heights

plot(df2$predicted_height, df2$bodymass)

