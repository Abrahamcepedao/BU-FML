# Creating plot for Google Stock Data (Extrapolation problems)#

# Read the data
data = read.csv("Datasets/Google_Stock_Data.csv")

# Grab the stock prices from 2004 to 2009
data = data[1:55,]

# Create an index variable
data$x = rownames(data)


require(ggplot2)

#Convert this to numeric
data$numeric_x = as.numeric(data$x)

# Visualize the data before and after the financial crisis
ggplot(data=data,aes(x=numeric_x, y=Close))+ geom_point() + theme_bw() + xlab("X") + ylab("Y") + xlim(0,60)+
  ylim(50,500)

breaks = c(6,18,30,42,54)
labels = as.character(c(2005,2006,2007,2008,2009))
ggplot(data=data,aes(x=numeric_x, y=Close))+ geom_point() + ylim(50,500) + xlim(0,60) + theme_bw() + 
  scale_x_continuous(breaks = breaks,labels=labels) + xlab("Year") + ylab("Price per Share") + 
  geom_vline(xintercept=42,linetype="dashed") + annotate(geom="text",x=50,y=400,label="Financial Crisis") + ggtitle("Google Stock Price (2004-2009)")

# The dangers of extrapolation #  

# Grab the data before the financial crisis

train = data[1:40,]
plot(train$Close)

mdl = lm(Close ~ numeric_x,data=train)

test = data[41:55,]

ggplot(data=data,aes(x=numeric_x, y=Close))+ geom_point() + ylim(50,500) + xlim(0,60) + theme_bw() + 
  scale_x_continuous(breaks = breaks,labels=labels) + xlab("Year") + ylab("Price per Share") + 
  geom_vline(xintercept=42,linetype="dashed") + 
  annotate(geom="text",x=50,y=400,label="Financial Crisis") + 
  ggtitle("Google Stock Price (2004-2009)") +
geom_abline(slope=mdl$coefficients[2],intercept=mdl$coefficients[1])


predict(mdl,test)
