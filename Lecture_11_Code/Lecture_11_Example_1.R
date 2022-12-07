#Table and Prop.Table to show contingency tables and quickly calculate proportions


#Generate fake data for a clinical trial
clinical.trial <-  data.frame(patient = 1:100, 
                              age = rnorm(100, mean = 60, sd = 6),
                              treatment = gl(2, 50, labels = c("Treatment", "Control")),
                              center = sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE))

## set some ages to NA (missing)
is.na(clinical.trial$age) <- sample(1:100, 20)

clinical.trial$center = factor(clinical.trial$center)

#Summary of age shows NA
summary(clinical.trial)


## a simple example of a table call
table(clinical.trial$center)



# For example 2, we need to create a logical vector indicating whether or not a patient is under 60 or not. 
# We can then pass that into the table function. Also, since there are missing ages, we might be interested 
# in seeing those in the table also. It is shown both ways by setting the “useNA” argument to table.

## A logical vector is created and passed into table
table(clinical.trial$age < 60)

## the useNA argument shows the missing values, too
table(clinical.trial$age < 60, useNA = "always")

# Look at counts between groups
table(clinical.trial$treatment,clinical.trial$center)

# "Discretize a continuous variable into a grouping variable
clinical.trial$senior = ifelse(clinical.trial$age >=65,1,0)

require(dplyr)

#Counts per old age, treatment and cente
counts = clinical.trial %>%
  group_by(senior,treatment,center) %>%
  summarise(count = n())

counts = clinical.trial %>%
  group_by(senior,treatment,center) %>%
  summarise(count = n(),variance = var(age))


## Prop.table shows proportions instead of raw counts
prop.table(table(clinical.trial$age>60))

prop.table(table(clinical.trial$center))

prop.table(table(clinical.trial$center,clinical.trial$age>60,useNA="always"))




