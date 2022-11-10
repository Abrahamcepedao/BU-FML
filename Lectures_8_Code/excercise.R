data <- read.csv("2018.csv")
head(data)

data$Perceptions.of.corruption = as.numeric(as.character(data$Perceptions.of.corruption))

data = data[complete.cases(data),]



require(GGally)

#remove rank and country
pairs_data = data[,3:9]
#data$Overall.rank = NULL
#data = data[,c(-1,-2)]
ggpairs(pairs_data)

#No need for transformation since score is normally distributed

mdl = lm(Score ~ .,data=pairs_data)

require(car)
VIF(mdl)


data$prediction = predict(mdl,data)

residuals = data$Score - data$prediction

# Plot of regression line
g = ggplot(data=data,aes(x=prediction,y=residuals)) + 
  geom_point() + 
  theme_bw() +
  geom_hline(yintercept=0,linetype="dashed")

# Plot of residuals
h = ggplot(data=df,aes(x=y,y=residuals)) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept=0,linetype="dashed")