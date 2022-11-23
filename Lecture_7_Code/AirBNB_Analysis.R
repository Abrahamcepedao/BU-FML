### Code for exit questions about AirBNB data ###
data = read.csv("Datasets/AB_Sampled_NYC_2019.csv")

hist(data$price)
summary(data$price)

boxplot(data$price)

### Does the name have "clean" in it?
data$has_clean = ifelse(grepl("clean",tolower(data$name),
                              fixed=T),
                        1,0)

## Is it in Manhattan?
data$in_manhattan = ifelse(data$neighbourhood_group=="Manhattan",
                           1,0)

data = data[data$price < 1000,]

data = data[data$price < 4000 & data$price > 20,]

# Scatterplots
df = data[,c("price","has_clean","in_manhattan","number_of_reviews")]
require(GGally)
ggpairs(df) + theme_bw()


## Multiple linear regression
mdl = lm(log(price) ~ has_clean + in_manhattan + number_of_reviews,data=data)

summary(mdl)

confint(mdl)

#RMSE Error rate
sqrt(mean((data$price-predict(mdl,data))^2))


## F-statistic manually, 3 and n-k-1 = 350 - 3 - 1 = 346 DF
qf(0.95,3,350-3-1) # 2.631

summary(mdl)

# When we have data distributed like the price, log transform can be helpful

mdl2 = lm(log(price) ~ has_clean + in_manhattan + number_of_reviews,data=data)

#RMSE error rate
sqrt(mean((data$price-exp(predict(mdl2,data)))^2))

summary(mdl2)

# Let's compare the original model with the revised model
df = data.frame(cbind(data$price,predict(mdl2,data)))
df$X1 = log(df$X1)
colnames(df) = c("Actual","Predicted")
require(ggplot2)

ggplot(data=df,aes(x=Predicted,y=Actual)) + 
  geom_density_2d() + 
  theme_bw() + 
  xlim(4.5,5) +
  ylim(3,6)


# Let's compare the original model with the revised model
df = data.frame(cbind(data$price,predict(mdl,data)))
colnames(df) = c("Actual","Predicted")
require(ggplot2)

ggplot(data=df,aes(x=Predicted,y=Actual)) + 
  geom_density_2d() + 
  theme_bw()






