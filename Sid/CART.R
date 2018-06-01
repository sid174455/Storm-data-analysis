#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : Create classification tree with rpart
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

#Creating test & train dataset
index<-sort(sample(nrow(storm),round(.30*nrow(storm))))
# train dataset
training<-storm[-index,]
# test dataset
test<-storm[index,]

#Install relevant packages

#install.packages("rpart")
#install.packages("rpart.plot")     
#install.packages("rattle")         
#install.packages("RColorBrewer")

#Load relevant packages
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#Classification Tree with rpart:

#Grow the tree for injuries
CART_class_1<-rpart(INJURIES~.,data=training,method="class",control =rpart.control(minsplit =200,minbucket=1, cp=0))
printcp(CART_class_1) # display the results
#plotcp(CART_class_1) # visualize cross-validation results
summary(CART_class_1) # detailed summary of splits

# plot tree
#rpart.plot(CART_class_1)
prp(CART_class_1)
#fancyRpartPlot(CART_class_1,cex=1)

# calculating error rate
CART_predict_1<-predict(CART_class_1,test, type="class")
CART_wrong_1<-sum(test[,8]!=CART_predict_1)
CART_error_rate_1<-CART_wrong_1/length(test[,8])*100
CART_error_rate_1



#Grow the tree for deaths
CART_class_2<-rpart(DEATHS~.,data=training,method="class",control =rpart.control(minsplit =200,minbucket=1, cp=0)) 

printcp(CART_class_2) # display the results
#plotcp(CART_class_2) # visualize cross-validation results
summary(CART_class_2) # detailed summary of splits

# plot tree
#rpart.plot(CART_class_2)
prp(CART_class_2)
#fancyRpartPlot(CART_class_2,cex=1)

# calculating error rate
CART_predict_2<-predict(CART_class_2,test, type="class")
CART_wrong_2<-sum(test[,9]!=CART_predict_2)
CART_error_rate_2<-CART_wrong_2/length(test[,9])*100
CART_error_rate_2




#Grow the tree for damage_property
CART_class_3<-rpart(DAMAGE_PROPERTY~.,data=training,method="class",control =rpart.control(minsplit =80,minbucket=1, cp=0))

printcp(CART_class_3) # display the results
#plotcp(CART_class_3) # visualize cross-validation results
summary(CART_class_3) # detailed summary of splits

# plot tree
#rpart.plot(CART_class_3)
prp(CART_class_3)
#fancyRpartPlot(CART_class_3,cex=1)

# calculating error rate
CART_predict_3<-predict(CART_class_3,test, type="class")
CART_wrong_3<-sum(test[,10]!=CART_predict_3)
CART_error_rate_3<-CART_wrong_3/length(test[,10])*100
CART_error_rate_3



#Grow the tree for damage_crops
CART_class_4<-rpart(DAMAGE_CROPS~.,data=training,method="class",control =rpart.control(minsplit =80,minbucket=1, cp=0))

printcp(CART_class_4) # display the results
#plotcp(CART_class_4) # visualize cross-validation results
summary(CART_class_4) # detailed summary of splits

# plot tree
#rpart.plot(CART_class_4)
prp(CART_class_4)
#fancyRpartPlot(CART_class_4,cex=1)

# calculating error rate
CART_predict_4<-predict(CART_class_4,test, type="class")
CART_wrong_4<-sum(test[,11]!=CART_predict_4)
CART_error_rate_4<-CART_wrong_4/length(test[,11])*100
CART_error_rate_4