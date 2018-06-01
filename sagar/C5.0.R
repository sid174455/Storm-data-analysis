#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : knn
#  First Name : Sagar
#  Last Name  : Jain
#  Id			    : 10429097
#  Date       : 04/20/2018
#################################################
#cleaning data
rm(list = ls())

#read data
storm <- read.csv("/Users/sagarjain/Desktop/kddm/project/sagar/storm.csv")
storm <- storm[,-c(1)]
#View(storm)

#diving data into test and training data set
index<-sort(sample(nrow(storm),round(.25*nrow(storm))))
training<-storm[-index,]
test<-storm[index,]

#install.packages("rpart")
#install.packages('C50')
library('C50')
library(rpart)
library('rpart.plot')


#set.seed(123)

#C5.0 methodology to develop a classification model for the INJURIES
C50_INJURIES<-C5.0(INJURIES~.,data=training)
C50_INJURIES
summary(C50_INJURIES )
plot(C50_INJURIES,main="INJURIES")
C50_INJURIES_predict<-predict( C50_INJURIES ,test , type="class" )
table(actual=test[,8],C50=C50_INJURIES_predict)
wrong_INJURIES<- (test[,8]!=C50_INJURIES_predict)
wrong_INJURIES
c50_rate_INJURIES<-sum(wrong_INJURIES)/length(test[,8])
c50_rate_INJURIES
#C5imp(C50_INJURIES,metric='usage')

#set.seed(123)
#C5.0 methodology to develop a classification model for the DEATHS
C50_DEATHS<-C5.0(DEATHS~.,data=training)
C50_DEATHS
summary(C50_DEATHS )
plot(C50_DEATHS,main="DEATH")
C50_DEATHS_predict<-predict( C50_DEATHS ,test , type="class" )
table(actual=test[,9],C50=C50_DEATHS_predict)
wrong_DEATHS<- (test[,9]!=C50_DEATHS_predict)
wrong_DEATHS
c50_rate_DEATHS<-sum(wrong_DEATHS)/length(test[,9])
c50_rate_DEATHS
C5imp(C50_DEATHS,metric='usage')


#C5.0 methodology to develop a classification model for the property damage
C50_DAMAGES_p<-C5.0(DAMAGE_PROPERTY~.,data=training)
C50_DAMAGES_p
summary(C50_DAMAGES_p )
plot(C50_DAMAGES_p,main="Damage Property")
C50_DAMAGES_p_predict<-predict( C50_DAMAGES_p ,test , type="class" )
table(actual=test[,10],C50=C50_DAMAGES_p_predict)
wrong_p_DAMAGES<- (test[,10]!=C50_DAMAGES_p_predict)
wrong_p_DAMAGES
c50_rate_DAMAGES_p<-sum(wrong_p_DAMAGES)/length(test[,10])
c50_rate_DAMAGES_p

#C5.0 methodology to develop a classification model for the crops damage
C50_DAMAGES_c<-C5.0(DAMAGE_CROPS~.,data=training)
C50_DAMAGES_c
summary(C50_DAMAGES_c )
plot(C50_DAMAGES_c,main="Damage Crops")
C50_DAMAGES_c_predict<-predict( C50_DAMAGES_c ,test , type="class" )
table(actual=test[,11],C50=C50_DAMAGES_c_predict)
wrong_c_DAMAGES<- (test[,11]!=C50_DAMAGES_c_predict)
wrong_c_DAMAGES
c50_rate_DAMAGES_c<-sum(wrong_c_DAMAGES)/length(test[,11])
c50_rate_DAMAGES_c

