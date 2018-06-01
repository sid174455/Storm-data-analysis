#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : Naive Bayes
#  First Name : Sayan
#  Last Name  : Mukherjee
#  Id			    : 10430998
#  Date       : 04/20/2018
#  Comments   :

rm(list=ls())
#################################################

#### Use the Naïve Bayes methodology to develop a classification model for the target variables. 

#install.packages('e1071', dependencies = TRUE)
library(class)
library(e1071)

# Model for Injuries
nb_injuries <- naiveBayes(INJURIES~., data = training)

prediction_injuries <- predict(nb_injuries, test)
table(Prediction = prediction_injuries, Actual = test$INJURIES)

wrong <- (test[,"INJURIES"] != prediction_injuries)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.60%

# Model for Deaths
nb_deaths <- naiveBayes(DEATHS~., data = training)

prediction_deaths <- predict(nb_deaths, test)
table(Prediction = prediction_deaths, Actual = test$DEATHS)

wrong <- (test[,"DEATHS"] != prediction_deaths)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.30%

# Model for Property Damage
nb_property <- naiveBayes(DAMAGE_PROPERTY~., data = training)

prediction_property <- predict(nb_property, test)
table(Prediction = prediction_property, Actual = test$DAMAGE_PROPERTY)

wrong <- (test[,"DAMAGE_PROPERTY"] != prediction_property)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.27%



# Model for Property Damage
nb_crops <- naiveBayes(DAMAGE_CROPS~., data = training)

prediction_crops <- predict(nb_crops, test)
table(Prediction = prediction_crops, Actual = test$DAMAGE_CROPS)

wrong <- (test[,"DAMAGE_CROPS"] != prediction_crops)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.014%