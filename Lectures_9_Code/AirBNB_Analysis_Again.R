# Read data
data = read.csv("Datasets/AB_Sampled_NYC_2019.csv")

require(dplyr)

# Remove outlier prices
data = data[data$price > 20 & data$price < 10000,]

# Get SD summary
data %>% group_by(room_type) %>% summarise(var(price))



# Histogram shows strong skew
hist(data$price)

# Use log transform
data$price_trans = log(data$price)

# Histogram is normally distributed now
hist(data$price_trans)

data %>% group_by(room_type) %>% summarise(var(price_trans))


#Anova with original price
m1 = aov(price ~ room_type,data=data)

summary(m1)

pairwise.t.test(data$price,data$room_type,p.adjust.method = "bonferroni")


# Anova with transformed price
data %>% group_by(room_type) %>% summarise(var(price_trans))


m2 = aov(price_trans ~ room_type,data=data)
pairwise.t.test(data$price_trans,data$room_type,p.adjust.method="bonferroni")


#Visualize differences
boxplot(data$price_trans ~ data$room_type)

#Linear regression modeling, same result
data$private_room = ifelse(data$room_type=="Private room",1,0)
data$shared_room = ifelse(data$room_type=="Shared room",1,0)

m3 = lm(price_trans ~ private_room + shared_room,data=data)


