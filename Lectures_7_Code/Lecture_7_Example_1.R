# Creating a multiple linear regression model and 
# evalating the model using standard evaluation metrics

# Read .csv from file
data <- read.csv("CEO_salary.csv")

# Data is about CEO salaries, age , and height
summary(data)



# Examine pairwise correlations between variables in the dataset
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  
  if(missing(cex.cor)) 
    cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}



pairs(data, upper.panel = panel.cor)



### GGplot version of pairwise correlations
# You will need to install a new package for this
install.packages("GGally")
require(GGally)

ggpairs(data) + theme_bw()

#Rescale salary, per $1,000 
data$salary1 = data$salary/ 1000

#Create SLR model using age only
m = lm(salary1 ~ age,data=data)

#Create MLR model using age and height
m2 = lm(salary1 ~ age + height,data=data)
attach(data)



#Try different values for age/height in the MLR model
age = 65

height = 70

salary1 = 2.503 * age + 2.507 * height + 190.697


age = 5
height = 50

salary2 = 2.503 * age + 2.507 * height + 190.697

#Create separate SLR models based on age alone and height alone
mdl2 = lm(salary1 ~ age,data=data)

mdl3 = lm(salary1 ~ height,data=data)


# Computing R^2 by hand #
totalss = sum((data$salary1 - mean(data$salary1))^2)
regss = sum((fitted(m)-mean(salary1))^2)
resiss = sum((salary1-fitted(m))^2)
fstatistic = (regss/2)/(resiss/97)
pvalue = 1-pf(fstatistic,df1=2,df2=97)
R2 = regss/totalss

# See read the five ways to visualize pairwise comparisions
# https://www.r-bloggers.com/five-ways-to-visualize-your-pairwise-comparisons/


#MSE
MSE = mean((data$salary1 - predict(m,data))^2)


#RMSE
RMSE = sqrt(MSE)

#Get predictions from the linear model
predict(m,data)

#MAD
mean(abs(data$salary1 - predict(m,data)))

# The metrics package contains many popular methods 
# to evaluate prediction models
install.packages("Metrics")
require(Metrics)

#Sum of squared error example
Metrics::sse(data$salary1,predict(m,data))
