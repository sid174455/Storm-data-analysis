#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : knn
#  First Name : Siddhesh
#  Last Name  : Prabhu
#  Id			    : 10429120
#  Date       : 04/20/2018
#################################################


#Clear the environment
rm(list=ls())

#Loading the data
storm<-
  read.csv("C://Users/hp/Desktop/513_Project/Me/storm.csv",na.strings = c("","NA"))

#Remove unwanted columns
storm<-storm[,-c(1)]

#To assign integer values to factors
storm$INJURIES<-factor(storm$INJURIES,levels = c("Yes","No"),labels = c(1,2))
storm$DEATHS<-factor(storm$DEATHS,levels = c("Yes","No"),labels = c(1,2))
storm$DAMAGE_PROPERTY<-factor(storm$DAMAGE_PROPERTY,levels = c("Low","Medium","High"),labels = c(1,2,3))
storm$DAMAGE_CROPS<-factor(storm$DAMAGE_CROPS,levels = c("Low","Medium","High"),labels = c(1,2,3))

##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

#Create Normalized data
storm_normalized<-as.data.frame (         
  cbind( BEGIN_DAY=mmnorm(storm[,1],min(storm[,1]),max(storm[,1]))
         , BEGIN_TIME=mmnorm(storm[,2],min(storm[,2]),max(storm[,2] ))
         , END_DAY=mmnorm(storm[,3],min(storm[,3]),max(storm[,3] ))
         , END_TIME=mmnorm(storm[,4],min(storm[,4]),max(storm[,4] ))
         , STATE=mmnorm(storm[,5],min(storm[,5]),max(storm[,5] ))
         , MONTH_NAME=mmnorm(storm[,6],min(storm[,6]),max(storm[,6] ))
         , EVENT_TYPE=mmnorm(storm[,7],min(storm[,7]),max(storm[,7] ))
         , INJURIES=storm[,8]
         , DEATHS=storm[,9]
         , DAMAGE_PROPERTY=mmnorm(storm[,10],min(storm[,10]),max(storm[,10] ))
         , DAMAGE_CROPS=mmnorm(storm[,11],min(storm[,11]),max(storm[,11] ))
         , MAGNITUDE=mmnorm(storm[,12],min(storm[,12]),max(storm[,12] ))
         
  )
)


#Creating test & train dataset
idx<-sort(sample(nrow(storm),as.integer(.70*nrow(storm))))
# train dataset
training<-storm[idx,]
# test dataset
test<-storm[-idx,]

#Load relevant package
library(class)

# preparing datasets for knn for injuries
test_injuries_dataset <- test[, -c(8)];
train_injuries_dataset <- training[, -c(8)];
classified_injuries_dataset <- training[, c(8)];

# output for k = 1
predict_injuries_k1 <- knn(train_injuries_dataset, test_injuries_dataset, classified_injuries_dataset, k=1);
prediction_injuries_k1 <- table(predict_injuries_k1, test[, 8])
accuracy_injuries_1 <-(sum(diag(prediction_injuries_k1)))/sum(prediction_injuries_k1)
error_injuries_rate1 <- (1-accuracy_injuries_1)*100
error_injuries_rate1

# output for k = 2
predict_injuries_k2 <- knn(train_injuries_dataset, test_injuries_dataset, classified_injuries_dataset, k=2);
prediction_injuries_k2 <- table(predict_injuries_k2, test[, 8])
accuracy_injuries_2 <-(sum(diag(prediction_injuries_k2)))/sum(prediction_injuries_k2)
error_injuries_rate2 <- (1-accuracy_injuries_2)*100
error_injuries_rate2

# output for k = 5
predict_injuries_k5 <- knn(train_injuries_dataset, test_injuries_dataset, classified_injuries_dataset, k=5);
prediction_injuries_k5 <- table(predict_injuries_k5, test[, 8])
accuracy_injuries_5 <-(sum(diag(prediction_injuries_k5)))/sum(prediction_injuries_k5)
error_injuries_rate5 <- (1-accuracy_injuries_5)*100
error_injuries_rate5

# output for k = 10
predict_injuries_k10 <- knn(train_injuries_dataset, test_injuries_dataset, classified_injuries_dataset, k=10);
prediction_injuries_k10 <- table(predict_injuries_k10, test[, 8])
accuracy_injuries_10 <-(sum(diag(prediction_injuries_k10)))/sum(prediction_injuries_k10)
error_injuries_rate10 <- (1-accuracy_injuries_10)*100
error_injuries_rate10



# preparing datasets for knn for deaths
test_deaths_dataset <- test[, -c(9)];
train_deaths_dataset <- training[, -c(9)];
classified_deaths_dataset <- training[, c(9)];

# output for k = 1
predict_deaths_k1 <- knn(train_deaths_dataset, test_deaths_dataset, classified_deaths_dataset, k=1);
prediction_deaths_k1 <- table(predict_deaths_k1, test[, 9])
accuracy_deaths_1 <-(sum(diag(prediction_deaths_k1)))/sum(prediction_deaths_k1)
error_deaths_rate1 <- (1-accuracy_deaths_1)*100
error_deaths_rate1

# output for k = 2
predict_deaths_k2 <- knn(train_deaths_dataset, test_deaths_dataset, classified_deaths_dataset, k=2);
prediction_deaths_k2 <- table(predict_deaths_k2, test[, 9])
accuracy_deaths_2 <-(sum(diag(prediction_deaths_k2)))/sum(prediction_deaths_k2)
error_deaths_rate2 <- (1-accuracy_deaths_2)*100
error_deaths_rate2

# output for k = 5
predict_deaths_k5 <- knn(train_deaths_dataset, test_deaths_dataset, classified_deaths_dataset, k=5);
prediction_deaths_k5 <- table(predict_deaths_k5, test[, 9])
accuracy_deaths_5 <-(sum(diag(prediction_deaths_k5)))/sum(prediction_deaths_k5)
error_deaths_rate5 <- (1-accuracy_deaths_5)*100
error_deaths_rate5

# output for k = 10
predict_deaths_k10 <- knn(train_deaths_dataset, test_deaths_dataset, classified_deaths_dataset, k=10);
prediction_deaths_k10 <- table(predict_deaths_k10, test[, 9])
accuracy_deaths_10 <-(sum(diag(prediction_deaths_k10)))/sum(prediction_deaths_k10)
error_deaths_rate10 <- (1-accuracy_deaths_10)*100
error_deaths_rate10



# preparing datasets for knn for DAMAGE_PROPERTY
test_damage_property_dataset <- test[, -c(10)];
train_damage_property_dataset <- training[, -c(10)];
classified_damage_property_dataset <- training[, c(10)];

# output for k = 1
predict_damage_property_k1 <- knn(train_damage_property_dataset, test_damage_property_dataset, classified_damage_property_dataset, k=1);
prediction_damage_property_k1 <- table(predict_damage_property_k1, test[, 10])
accuracy_damage_property_1 <-(sum(diag(prediction_damage_property_k1)))/sum(prediction_damage_property_k1)
error_damage_property_rate1 <- (1-accuracy_damage_property_1)*100
error_damage_property_rate1

# output for k = 2
predict_damage_property_k2 <- knn(train_damage_property_dataset, test_damage_property_dataset, classified_damage_property_dataset, k=2);
prediction_damage_property_k2 <- table(predict_damage_property_k2, test[, 10])
accuracy_damage_property_2 <-(sum(diag(prediction_damage_property_k2)))/sum(prediction_damage_property_k2)
error_damage_property_rate2 <- (1-accuracy_damage_property_2)*100
error_damage_property_rate2

# output for k = 5
predict_damage_property_k5 <- knn(train_damage_property_dataset, test_damage_property_dataset, classified_damage_property_dataset, k=5);
prediction_damage_property_k5 <- table(predict_damage_property_k5, test[, 10])
accuracy_damage_property_5 <-(sum(diag(prediction_damage_property_k5)))/sum(prediction_damage_property_k5)
error_damage_property_rate5 <- (1-accuracy_damage_property_5)*100
error_damage_property_rate5

# output for k = 10
predict_damage_property_k10 <- knn(train_damage_property_dataset, test_damage_property_dataset, classified_damage_property_dataset, k=10);
prediction_damage_property_k10 <- table(predict_damage_property_k10, test[, 10])
accuracy_damage_property_10 <-(sum(diag(prediction_damage_property_k10)))/sum(prediction_damage_property_k10)
error_damage_property_rate10 <- (1-accuracy_damage_property_10)*100
error_damage_property_rate10



# preparing datasets for knn for DAMAGE_CROPS
test_damage_crops_dataset <- test[, -c(11)];
train_damage_crops_dataset <- training[, -c(11)];
classified_damage_crops_dataset <- training[, c(11)];

# output for k = 1
predict_damage_crops_k1 <- knn(train_damage_crops_dataset, test_damage_crops_dataset, classified_damage_crops_dataset, k=1);
prediction_damage_crops_k1 <- table(predict_damage_crops_k1, test[, 11])
accuracy_damage_crops_1 <-(sum(diag(prediction_damage_crops_k1)))/sum(prediction_damage_crops_k1)
error_damage_crops_rate1 <- (1-accuracy_damage_crops_1)*100
error_damage_crops_rate1

# output for k = 2
predict_damage_crops_k2 <- knn(train_damage_crops_dataset, test_damage_crops_dataset, classified_damage_crops_dataset, k=2);
prediction_damage_crops_k2 <- table(predict_damage_crops_k2, test[, 11])
accuracy_damage_crops_2 <-(sum(diag(prediction_damage_crops_k2)))/sum(prediction_damage_crops_k2)
error_damage_crops_rate2 <- (1-accuracy_damage_crops_2)*100
error_damage_crops_rate2

# output for k = 5
predict_damage_crops_k5 <- knn(train_damage_crops_dataset, test_damage_crops_dataset, classified_damage_crops_dataset, k=5);
prediction_damage_crops_k5 <- table(predict_damage_crops_k5, test[, 11])
accuracy_damage_crops_5 <-(sum(diag(prediction_damage_crops_k5)))/sum(prediction_damage_crops_k5)
error_damage_crops_rate5 <- (1-accuracy_damage_crops_5)*100
error_damage_crops_rate5

# output for k = 10
predict_damage_crops_k10 <- knn(train_damage_crops_dataset, test_damage_crops_dataset, classified_damage_crops_dataset, k=10);
prediction_damage_crops_k10 <- table(predict_damage_crops_k10, test[, 11])
accuracy_damage_crops_10 <-(sum(diag(prediction_damage_crops_k10)))/sum(prediction_damage_crops_k10)
error_damage_crops_rate10 <- (1-accuracy_damage_crops_10)*100
error_damage_crops_rate10