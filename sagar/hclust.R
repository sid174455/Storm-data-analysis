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
storm$INJURIES<-factor(storm$INJURIES,levels = c("No","Yes"),labels = c(0,1))
storm$DEATHS<-factor(storm$DEATHS,levels = c("No","Yes"),labels = c(0,1))
storm$DAMAGE_PROPERTY<-factor(storm$DAMAGE_PROPERTY,levels = c("Low","Medium","High"),labels = c(0,1,2))
storm$DAMAGE_CROPS<-factor(storm$DAMAGE_CROPS,levels = c("Low","Medium","High"),labels = c(0,1,2));

storm<- storm[sample(1:nrow(storm),nrow(storm)/2),]

#install.packages("factoextra")
library(ggplot2)
library(factoextra)
?dist
?cutree
#hclust for injuries
Injuries_dist<-dist( storm[,-c(8)])
Injuries_hclust<-hclust(Injuries_dist,method="complete")
plot(Injuries_hclust, cex = 0.6, hang = -1)
Injuries_cutree<-cutree(Injuries_hclust,2) 
Injuries_cutree
table(Injuries_cutree,storm[,8])

#hclust for death
Deaths_dist<-dist( storm[,-c(9)])
Death_hclust<-hclust(Deaths_dist,method="average")
plot(Death_hclust, cex = 0.6, hang = -1) 
Death_cutree<-cutree(Death_hclust,2) 
table(Death_cutree,storm[,9])

#hclust for damage property
Damage_property_dist<-dist( storm[,-c(10)])
Damage_property_hclust<-hclust(Damage_property_dist,method="complete")
plot(Damage_property_hclust, cex = 0.6, hang = -1)
Damage_cutree<-cutree(Damage_property_hclust,2) 
table(Damage_cutree,storm[,10])

#hclust for damage crops
Damage_crops_dist<-dist( storm[,-c(11)])
Damage_crops_hclust<-hclust(Damage_crops_dist,method="complete")
plot(Damage_crops_hclust, cex = 0.6, hang = -1)
Damage_crops_cutree<-cutree(Damage_crops_hclust,2) 
table(Damage_crops_cutree,storm[,11])
