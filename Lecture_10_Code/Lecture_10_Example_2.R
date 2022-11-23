# Install one time the "car" package. 
# install.packages('car')

library(car)

# Exercise example
exercise <- read.csv("Datasets/exercise.csv")
exercise
attach(exercise)

# Test interactions
# Create a model to predict energy based on stretching, ankle weights,
# And the interaction between the two factors
model <- lm(Energy~PreStretch + 
              AnkleWeights + 
              PreStretch * AnkleWeights, data=exercise)

# You will get the same with the following model code. 
# model <- lm(Energy ~ PreStretch * AnkleWeights, data=exercise)
# model <- lm(Energy ~ PreStretch + AnkleWeights + PreStretch:AnkleWeights, data=exercise)

#Summary shows that the interaction term is not significant (p = 0.70)
#No need to include interaction term moving forward

#Significant impact on energy (stretching and ankle weights)
model <- lm(Energy~PreStretch + 
              AnkleWeights , data=exercise)
summary(model)


Anova(model, type=3)

# What about for walking speed?
model1 <- lm(Speed~PreStretch+AnkleWeights+PreStretch*AnkleWeights, data=exercise)

#Same result
summary(model1)

#No significant impact of either factor on walking speed
model1 <- lm(Speed~PreStretch+AnkleWeights, data=exercise)
summary(model1)

Anova(model1, type=3)

# Lastly, we try to predict oxygen consumption
model2 <- lm(Oxygen~PreStretch+AnkleWeights+PreStretch*AnkleWeights, data=exercise)
summary(model2)
Anova(model2, type=3)

#No significant interaction, run regular two-way ANOVA
model2 <- lm(Oxygen~PreStretch+AnkleWeights, data=exercise)

summary(model2)

#Both have impact on Oxygen consumption

# Generate interaction plots for each of the three outcomes
interaction.plot(PreStretch, AnkleWeights, Energy, col=1:2)
interaction.plot(PreStretch, AnkleWeights, Speed, col=1:2)
interaction.plot(PreStretch, AnkleWeights, Oxygen, col=1:2)

# If the interaction is significant, 
# then we need to stratify 
# (by one of the two factors) 
stretch <- exercise[which(PreStretch=='Stretch'),]
nostretch <- exercise[which(PreStretch=='No stretch'),]

# Energy
summary(aov(Energy~AnkleWeights, data=stretch))
summary(aov(Energy~AnkleWeights, data=nostretch))

# Speed
summary(aov(Speed~AnkleWeights, data=stretch))
summary(aov(Speed~AnkleWeights, data=nostretch))

# Oxygen
summary(aov(Oxygen~AnkleWeights, data=stretch))
summary(aov(Oxygen~AnkleWeights, data=nostretch))


model <- lm(Energy~PreStretch + AnkleWeights, data=exercise)

summary(model)
aov(model)
Anova(model, type=3)

