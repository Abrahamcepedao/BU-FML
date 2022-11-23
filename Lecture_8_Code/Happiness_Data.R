### Happiness data parsing #
data = read.csv("Datasets/happiness/2018.csv")

data$Perceptions.of.corruption
data$Perceptions.of.corruption = as.numeric(as.character(
  data$Perceptions.of.corruption))



#Remove country 20, missing data
data = data[complete.cases(data),]
require(GGally)

pairs_data = data[,3:9]
data$Overall.rank = data$Country.or.region =  NULL
data = data[,c(-1,-2)]

ggpairs(pairs_data)

install.packages("regclass")
require(regclass)

#install.packages("car")
#require(car)
#vif(mdl)

mdl = lm(Score ~ .,data=pairs_data)
VIF(mdl)

tmp = pairs_data
tmp$GDP.per.capita = NULL

mdl_without_gdp = lm(Score ~ I(x^2).,data=tmp)
VIF(mdl_without_gdp)

mdl = mdl_without_gdp

mdl = lm(Score ~ GDP.per.capita +  Freedom.to.make.life.choices + Perceptions.of.corruption,
         data=pairs_data)


plot(mdl,which=3)





### Read in the testing data
test = read.csv("Datasets/happiness/2019.csv")

#Defining a function to calculate root mean-squared error
RMSE = function(preds,actual)
{
  return(sqrt(mean((preds-actual)^2)))
  
}

RMSE(as.numeric(predict(mdl,pairs_data)),pairs_data$Score)

RMSE(as.numeric(predict(mdl_all,pairs_data)),pairs_data$Score)

RMSE(as.numeric(predict(mdl,test)),test$Score)

RMSE(as.numeric(predict(mdl_all,test)),test$Score)

RMSE(as.numeric(predict(mdl_step,test)),test$Score)

#Root-Mean square error function

