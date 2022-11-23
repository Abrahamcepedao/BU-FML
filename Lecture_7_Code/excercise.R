data <- read.csv("AB_Sampled_NYC_2019.csv")
attach(data)

data <- data[data$price < 1000,]

data$has_clean <- ifelse(grepl("clean", data$name, ignore.case = T),1,0)
data$in_manhattan <- ifelse(data$neighbourhood_group == "Manhattan",1,0)

m <- lm(data$price~has_clean+in_manhattan+number_of_reviews, data=data)


pairs(data, upper.panel = panel.cor)


#F-test
n <- length(data$number_of_reviews)
k <- 3

#1. Hypothesis
#H0 ∶ βhas_clean = βin_manhattan = βnumber_of_reviews = 0 
#H1 ∶ ββhas_clean ≠ 0 and/or βin_manhattan ≠ 0 and/or βnumber_of_reviews
#α = 0.05

#2. Test statistic
#F-test
#n = 350
#df1 = 3
#df2 = n - 3 - 1 = 346

#3. Decision rule
qf(0.95, df1=k, df2=n-k-1)
#Reject H0 if F ≥ 2.630716 Otherwise, do not reject H0

#4. Compute
summary(m)
#5. Conclusion
#Reject H0 since 9.365 ≥ 2.630716


confint(m, level =0.99)

