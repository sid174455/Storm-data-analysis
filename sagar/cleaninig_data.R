#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : knn
#  First Name : Sagar
#  Last Name  : Jain
#  Id			    : 10429097
#  Date       : 04/20/2018
#################################################
#Cleaning Data
rm(list=ls())

#Read file
storm<-
  read.csv("/Users/sagarjain/Desktop/kddm/project/2017.csv", na.strings=c(""),stringsAsFactors = FALSE)

#Remove unwanted columns
storm<-storm[,-c(1,4,7,8,10,11,14, 15, 16, 17, 18, 20, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 42, 43, 45, 46, 47, 48, 49, 50, 51)]

#Label MONTH_NAME
storm$MONTH_NAME<-factor(storm$MONTH_NAME, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"), labels=c(1,2,3,4,5,6,7,8,9,10,11,12))

#Label EVENT_TYPE
storm$EVENT_TYPE<-factor(storm$EVENT_TYPE,levels=c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
                                                   "Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought",
                                                   "Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill",
                                                   "Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud",
                                                   "Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind",
                                                   "Hurricane","Ice Storm","Lake-Effect Snow","Lakeshore Flood","Lightning",
                                                   "Marine Hail","Marine High Wind","Marine Hurricane/Typhoon",
                                                   "Marine Strong Wind","Marine Thunderstorm Wind","Marine Tropical Depression",
                                                   "Marine Tropical Storm","Rip Current","Sleet","Sneakerwave","Storm Surge/Tide",
                                                   "Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm",
                                                   "Waterspout","Wildfire","Winter Storm","Winter Weather"), labels=c(1,2,3,4,5,6,7,8,9,
                                                                                                                      10,11,12,13,14,15,
                                                                                                                      16,17,18,19,20,21,
                                                                                                                      22,23,24,25,26,27,
                                                                                                                      28,29,30,31,32,33,
                                                                                                                      34,35,36,37,38,39,
                                                                                                                      40,41,42,43,44,45,
                                                                                                                      46,47,48,49))

#Label STATE

storm$STATE<-(factor(storm$STATE))
levels(storm$STATE)<-list("1"=c("ALABAMA"),"2"=c("ALASKA"),"3"=c("ARIZONA"),"4"=c("ARKANSAS"),"5"=c("CALIFORNIA"),
                          "6"=c("COLORADO"),"7"=c("CONNECTICUT"),"8"=c("DELAWARE"),"9"=c("FLORIDA"),"10"=c("GEORGIA"),
                          "11"=c("HAWAII"),"12"=c("IDAHO"),"13"=c("ILLINOIS"),"14"=c("INDIANA"),
                          "15"=c("IOWA"),"16"=c("KANSAS"),"17"=c("KENTUCKY"),"18"=c("LOUISIANA"),
                          "19"=c("MAINE"),"20"=c("MARYLAND"),"21"=c("MASSACHUSETTS"),"22"=c("MICHIGAN"),
                          "23"=c("MINNESOTA"),"24"=c("MISSISSIPPI"),"25"=c("MISSOURI"),"26"=c("MONTANA"),
                          "27"=c("NEBRASKA"),"28"=c("NEVADA"),"29"=c("NEW HAMPSHIRE"),"30"=c("NEW JERSEY"),
                          "31"=c("NEW MEXICO"),"32"=c("NEW YORK"),"33"=c("NORTH CAROLINA"),"34"=c("NORTH DAKOTA"),
                          "35"=c("OHIO"),"36"=c("OKLAHOMA"),"37"=c("OREGON"),"38"=c("PENNSYLVANIA"),
                          "39"=c("RHODE ISLAND"),"40"=c("SOUTH CAROLINA"),"41"=c("SOUTH DAKOTA"),
                          "42"=c("TENNESSEE"),"43"=c("TEXAS"),"44"=c("UTAH"),"45"=c("VERMONT"),"46"=c("VIRGINIA"),
                          "47"=c("WASHINGTON"),"48"=c("WEST VIRGINIA"),"49"=c("WISCONSIN"),"50"=c("WYOMING"),
                          "51"=c("AMERICAN SAMOA","ATLANTIC NORTH","ATLANTIC SOUTH","DISTRICT OF COLUMBIA","E PACIFIC","GUAM","GULF OF MEXICO","HAWAII WATERS","LAKE ERIE","LAKE HURON","LAKE MICHIGAN","LAKE ONTARIO","LAKE ST CLAIR","LAKE SUPERIOR","PUERTO RICO","ST LAWRENCE R","VIRGIN ISLANDS"))

#Label MAGNITUDE_TYPE
storm$MAGNITUDE_TYPE<-factor(storm$MAGNITUDE_TYPE, levels=c("EG","ES","MG","MS"),labels=c(1,2,3,4))

#Combining INJURIES_DIRECT+INJURIES_INDIRECT
colnames(storm)[10]<-"INJURIES"                         #Rename operation
storm$INJURIES<-storm$INJURIES_DIRECT+storm$INJURIES

#Combining DEATHS_DIRECT+DEATHS_INDIRECT
colnames(storm)[11]<-"DEATHS"                           #Rename operation
storm$DEATHS<-storm$DEATHS+storm$DEATHS_INDIRECT

#To assign 0.00K to NA values in DAMAGE_PROPERTY & DAMAGE_CROPS
storm[is.na(storm$DAMAGE_PROPERTY),"DAMAGE_PROPERTY"]<-"0.00K"
storm[is.na(storm$DAMAGE_CROPS),"DAMAGE_CROPS"]<-"0.00K"

#To assign (Low,Medium,High)levels to DAMGE_CROPS & DAMAGE_PROPERTY
storm$DAMAGE_CROPS <- gsub(".*K.*", "Low", storm$DAMAGE_CROPS)
storm$DAMAGE_CROPS <- gsub(".*M.*", "Medium", storm$DAMAGE_CROPS)
storm$DAMAGE_CROPS <- gsub(".*B.*", "High", storm$DAMAGE_CROPS)

storm$DAMAGE_PROPERTY <- gsub(".*K.*", "Low", storm$DAMAGE_PROPERTY)
storm$DAMAGE_PROPERTY <- gsub(".*M.*", "Medium", storm$DAMAGE_PROPERTY)
storm$DAMAGE_PROPERTY <- gsub(".*B.*", "High", storm$DAMAGE_PROPERTY)

storm$DAMAGE_CROPS<- factor(storm$DAMAGE_CROPS)
storm$DAMAGE_PROPERTY<- factor(storm$DAMAGE_PROPERTY)
#Injuries Yes,No
storm$INJURIES<-factor(storm$INJURIES)
levels(storm$INJURIES)<-list(No=c("0"), Yes=c("1","2","3","4","5","6","7","8","10","11","12","14","15","20","24","25","29","30","31","32","33","45","51","56","64","500"))

#Deaths Yes,No
storm$DEATHS<-factor(storm$DEATHS)
levels(storm$DEATHS)<-list(No=c("0"), Yes=c("1","2","3","4","5","6","7","8","10","14","18","20","21","38"))

#To assign 0 to NA values IN MAGNITUDE
storm$MAGNITUDE[is.na(storm$MAGNITUDE)]<-0
#storm$TOTAL_DAMAGE[is.na(storm$TOTAL_DAMAGE)]<-0

#Remove unwanted columns
storm<-storm[,-c(8,9,12,15,17,18,19)]
View(storm)

#To write storm dataset to csv file
write.csv(storm, file = "/Users/sagarjain/Desktop/kddm/project/final/storm.csv")
