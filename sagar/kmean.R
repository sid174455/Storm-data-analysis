#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : knn
#  First Name : Sagar
#  Last Name  : Jain
#  Id			    : 10429097
#  Date       : 04/20/2018
#################################################

#Clear the environment
rm(list=ls())

#Loading the data
storm <- read.csv("/Users/sagarjain/Desktop/kddm/project/sagar/storm.csv")

#Remove unwanted columns
storm<-storm[,-c(1,13)]

#To assign integer values to factors
storm$INJURIES<-factor(storm$INJURIES,levels = c("No","Yes"),labels = c(1,2))
storm$DEATHS<-factor(storm$DEATHS,levels = c("No","Yes"),labels = c(1,2))
storm$DAMAGE_PROPERTY<-factor(storm$DAMAGE_PROPERTY,levels = c("Low","Medium","High"),labels = c(1,2,3))
storm$DAMAGE_CROPS<-factor(storm$DAMAGE_CROPS,levels = c("Low","Medium","High"),labels = c(1,2,3));
?kmeans
#calculating Kmeans for Injurues
kmeans_injuries<- kmeans(storm[,-c(8)],2,nstart = 10)
kmeans_injuries
kmeans_injuries$cluster
table(kmeans_injuries$cluster,storm[,8])

#calculating kmeans for Death
kmeans_death<- kmeans(storm[,-c(9)],2,nstart = 10)
kmeans_death$cluster
table(kmeans_death$cluster,storm[,8])

#calculating Kmeans for Damage property
kmeans_property<- kmeans(storm[,-c(10)],3,nstart = 10)
kmeans_property$cluster
table(kmeans_property$cluster,storm[,10])

#calculating Kmeans for damage crops
kmeans_crops<- kmeans(storm[,-c(11)],3,nstart = 10)
kmeans_crops$cluster
table(kmeans_crops$cluster,storm[,11])
?kmeans
