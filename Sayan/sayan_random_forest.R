#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : Random Forest
#  First Name : Sayan
#  Last Name  : Mukherjee
#  Id			    : 10430998
#  Date       : 04/20/2018
#  Comments   :

rm(list=ls())
#################################################

# Convert to factors for Random Forest

library(dplyr)
storm_data_relevant=storm_data_relevant %>% mutate_if(is.character, as.factor)

# Apply Random Forest
#install.packages("randomForest")
library(randomForest)

set.seed(100) # to achieve same result consistently

index <- sort(sample(nrow(storm_data_relevant), round(.30*nrow(storm_data_relevant))))
training <- storm_data_relevant[-index,]
test <- storm_data_relevant[index,]

# RF for Injuries
rForest_Injuries <- randomForest(INJURIES~., data=training, importance=TRUE, ntree=1000)

prediction_injuries <- predict(rForest_Injuries, test)
table(Prediction = prediction_injuries, Actual = test$INJURIES)

wrong <- (test[,"INJURIES"] != prediction_injuries)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.56%

varImpPlot(rForest_Injuries)

# RF for Deaths
rForest_Deaths <- randomForest(DEATHS~., data=training, importance=TRUE, ntree=1000)

prediction_deaths <- predict(rForest_Deaths, test)
table(Prediction = prediction_deaths, Actual = test$DEATHS)

wrong <- (test[,"DEATHS"] != prediction_deaths)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.27%

varImpPlot(rForest_Deaths)

# RF for Damage Property
rForest_Property <- randomForest(DAMAGE_PROPERTY~., data=training, importance=TRUE, ntree=1000)

prediction_property <- predict(rForest_Property, test)
table(Prediction = prediction_property, Actual = test$DAMAGE_PROPERTY)

wrong <- (test[,"DAMAGE_PROPERTY"] != prediction_property)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.22%

varImpPlot(rForest_Property)

# RF for Damage Crops
rForest_Crops <- randomForest(DAMAGE_CROPS~., data=training, importance=TRUE, ntree=1000)

prediction_crops <- predict(rForest_Crops, test)
table(Prediction = prediction_crops, Actual = test$DAMAGE_CROPS)

wrong <- (test[,"DAMAGE_CROPS"] != prediction_crops)
error_rate <- sum(wrong)/length(wrong)

error_rate * 100 ### Error rate of 0.028%

varImpPlot(rForest_Crops)
