# Underfitting and Overfitting

### Code for exit questions about AirBNB data ###
data = read.csv("Datasets/AB_NYC_2019.csv")


### Does the name have "clean" in it?
data$has_clean = ifelse(grepl("clean",tolower(data$name),fixed=T),1,0)

## Is it in Manhattan?
data$in_manhattan = ifelse(data$neighbourhood_group=="Manhattan",1,0)

data = data[data$price < 4000 & data$price > 20,]


# When we have data distributed like the price, log transform can be helpful

mdl = lm(log(price) ~ has_clean + in_manhattan + number_of_reviews + room_type,data=data)

# Check the error #
sqrt(mean((data$price-exp(predict(mdl,data)))^2))

# split into train and test set

split = round(0.75*nrow(data))

train = data[1:split,]
test = data[(split+1):nrow(data),]

#Develop the model on the training set
mdl = lm(log(price) ~ has_clean + in_manhattan + number_of_reviews + room_type,data=train)

#Calculate training error
sqrt(mean((train$price-exp(predict(mdl,train)))^2))

#Calculate testing error -> This is a good estimate of how wrong you will be on new data
sqrt(mean((test$price-exp(predict(mdl,test)))^2))

