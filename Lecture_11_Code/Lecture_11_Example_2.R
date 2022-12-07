# One and Two Sample Tests for Proportion


# One Sample Tests for a Proportion

# Example:
#A competitor claims that 25% of accounts on Twitter are SPAM accounts. 
#To refute this claim, Twitter employees manually verify a random sample of 
#N = 200 accounts and find that 30/200 accounts are SPAM. 
#Does this give evidence that $25\%$ of accounts are SPAM?

# See the manual page for prop.test() function 
?prop.test

# The procedure gives a chi-square statistic which is equal to the square of the z-statistic.
# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual13.html 

# One Sample Tests for a Proportion 
prop.test (30 , 200 , p=0.25 , conf.level=0.95 , correct=TRUE,alternative="less")
prop.test (30 , 200 , p=0.25 , conf.level=0.95 , correct=FALSE,alternative="less")

p_hat = 30/200

denom = sqrt((0.25*(1-0.25))/200)
(0.25-p_hat)/denom



# Two-Sample Tests for Proportions

# Example:
# An investigator is interested in the long-term effects of preschool
# programs on low-income children. A study was conducted where by two
# groups of children were followed over time. The first group of 61
# children did not attend preschool. The second group of 62 children
# (from similar areas and with similar backgrounds of those in the
# first sample) attended preschool as 3- and 4-year-olds. The need
# for social programs as adults was the outcome of interest. Of the
# group who did not attend preschool, 49 of them needed social
# services (mainly welfare) between the ages of 18 and 30. In the
# preschool group, 38 required social services in the same age range.

# Two Sample Test for Proportion 
prop.test(c(49, 38) , c(61, 62) , conf.level =0.95 , correct=FALSE)

# Two Sample Test for Proportion with Continuity Correction
prop.test(c(10, 18) , c(20, 24) , conf.level =0.95 , correct = TRUE)




