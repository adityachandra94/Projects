library('stringr')
library('dplyr')
library('ggplot2')
data <- read.csv('data/TransportCSOfile.csv')
str(data)

#To shorten the length of headers from column 10 to 22, split it into 13 parts and take the last part 
for(x in 10:22){
  names(data)[x] <- str_split_fixed(names(data)[x],"_", 13)[,13]
}

#To shorten the length of headers from column 23 to 32, split it into 15 parts and concatenate necessary parts
for(x in 23:32){
  newname <- str_split_fixed(names(data)[x],"_", 15)
  names(data)[x] <- str_c(newname[6],"_",newname[7],"_",newname[15])
}

#To shorten the length of headers from column 33 to 40, split it into 15 parts and concatenate necessary parts
for(x in 33:40){
  newname <- str_split_fixed(names(data)[x],"_", 12)
  names(data)[x] <- str_c(newname[6],"_",newname[7],"_",newname[12])
}

sum(is.na(data))
sapply(data, function(x) sum(is.na(x)))   #To check NA values in each column

#Columns Total_2011 and Time_Leaving_After_0930_2011 have 1 missing value each
#Since there are only two missing values, we can omit them
data <- na.omit(data)


#Outliers
summary(data) 
boxplot(data, plot = FALSE)$out
boxplot(data, outline = FALSE)

#Based on the summary stats for each column, we can decide a upper limit for values in each column
#In this way we can reduce the number of outliers
data <-
  subset(data, On_Foot_2011 < 50 &
      Bicycle_2011 < 10 &
      Bus_Minibus_Coach_2011 < 40 &
      Train_Dart_Luas_2011 < 10 & 
      Motorcycle_Scooter_2011 < 5 &
      Car_Driver_2011 < 100 &
      Car_Passenger_2011 < 50 &
      Van_2011 < 20 &
      Other_2011 < 20 &
      Soft_Modes_Comb_2011 < 60 &
      Public_Transport_Comb_2011 < 50 &
      Private_Transport_Comb_2011 < 170 &
      Total_2011 < 300  &
      Time_Leaving_Before_0630_2011 < 20 &
      Time_Leaving_0630_0700_2011 < 30  &
      Time_Leaving_0701_0730_2011 < 30 &
      Time_Leaving_0731_8000_2011 < 50  &
      Time_Leaving_0801_0830_2011 < 50 &
      Time_Leaving_0831_0900_2011 < 60 &
      Time_Leaving_0901_0930_2011 < 30  &
      Time_Leaving_After_0930_2011 < 30 &
      Time_Leaving_Not_Stated_2011 < 15 &
      Time_Leaving_Total_2011 < 250 &
      Journey_Time_Under_15_mins_2011 < 100 &
      Journey_Time_Quarter_To_Under_Half_Hour_2011 < 75 &
      Journey_Time_Half_Hour_To_Under_Three_Quarter_Hours_2011 < 50 &
      Journey_Time_Three_Quarter_Hours_To_Under_One_Hour_2011 < 30  &
      Journey_Time_One_Hour_To_Under_One_Hour_Thirty_Mins_2011 < 20 &
      Journey_Time_One_And_Half_Hours_And_Over_2011 < 10 & 
      Journey_Time_Not_Stated_2011 < 20 & 
      Journey_Time_Total_2011 < 250
  )

#An entry in Planning.Region column is named South which should be Southern
print(unique(data$Planning.Region))
data$Planning.Region[data$Planning.Region == "South"] <- "Southern"
print(unique(data$Planning.Region))

#Q1 Most popular transport mode nationally
mean_transport_mode <- list()
index <- 10
for(i in 1:10){
  mean_transport_mode[i] <- mean(data[,index])
  index <- index + 1
}
mean_transport_mode

#Most popular transport mode is the one with maximum mean
max_mode <- max(unlist(mean_transport_mode))
print(max_mode)

for(i in 10:20){
  if (mean(data[,i]) == max_mode)
  print(str_c('The most popular mode of transport in Ireland is ', names(data[i])))
}

#Q2 Most popular transport mode in assigned county i.e., Donegal
data2 <- filter(data, data$County == 'Donegal')     #Filter data related to Donegal only

mean_trans_mode_Donegal <- list()
index <- 10
for(i in 1:10){
  mean_trans_mode_Donegal[i] <- mean(data2[,index])
  index <- index + 1
}
print(mean_trans_mode_Donegal)

#Most popular transport mode is the one with maximum mean
max_mode_Donegal <- max(unlist(mean_trans_mode_Donegal))
print(max_mode_Donegal)

for(i in 10:20){
  if (mean(data2[,i]) == max_mode_Donegal)
    print(str_c('The most popular mode of transport in Donegal is ', names(data2[i])))
}


#Q3 Difference in choice of transportation between cities and other regions
#For cities
data_cities = data %>% filter(grepl("City",data$County)) %>% summarise_all(funs(mean))
data_cities = t(data_cities[10:21])
data_cities = data.frame(data_cities)
data_cities <- cbind(transportMode = rownames(data_cities), data_cities)

#Plot for transport mode in cities
ggplot(data=data_cities, aes(x=transportMode, y=data_cities)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#For other regions
data_other = data %>% filter(!grepl("City",data$County)) %>% summarise_all(funs(mean))
data_other = t(data_other[10:21])
data_other = data.frame(data_other)
data_other <- cbind(transportMode = rownames(data_other), data_other)

#Plot for transport modes in other regions
ggplot(data=data_other, aes(x=transportMode, y=data_other)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



#Q4 Proportion of commuters leaving outside 8-9am
totalCommuter <- sum(data[,"Time_Leaving_Total_2011"])
print(totalCommuter)

commuter_OutRush <- totalCommuter - sum(sum(data[,"Time_Leaving_0801_0830_2011"]), 
                                        sum(data[,"Time_Leaving_0831_0900_2011"]))

print(commuter_OutRush)
prop <- commuter_OutRush/totalCommuter
print(str_c("Proportion of commuters leaving outside 8-9am is ", round(prop,2)))



#Q5 Commuters likely to travel more than 45 minutes in assigned county
totalCommuterDonegal <- sum(data2[,"Journey_Time_Total_2011"])
print(str_c("Total number of people who commute in Donegal are ", totalCommuterDonegal))
comm_more_than_45 <- sum(sum(data2[,"Journey_Time_Three_Quarter_Hours_To_Under_One_Hour_2011"]),
                         sum(data2[,"Journey_Time_One_Hour_To_Under_One_Hour_Thirty_Mins_2011"]),
                         sum(data2[,"Journey_Time_One_And_Half_Hours_And_Over_2011"]))
print(str_c("Total number of people in Donegal who travel for more than 45 minutes are ", comm_more_than_45))
prop_exceed_45 <- comm_more_than_45/totalCommuterDonegal
print(str_c("The proportion of people in Donegal who travel longer than 45 minutes is ", 
            round(prop_exceed_45, 3), " which is quite less."))



#Q6 Commuters likely to travel longer than 45 minutes in all counties for same NUTSIII region as Donegal
nut <- data2$NUTS_III[1]          #To determine NUTSIII region for assigned county (Donegal)
data3 <- filter(data, data$NUTS_III == nut)   #Filter data for Border NUTSIII region
counties <- data3 %>% distinct(County)

totalCommuterNut <- list()       #List for total number of commuters

for(u in 1:6){
  totalCommuterNut[u] <- sum(filter(data3, data3$County == counties[u,])[,"Journey_Time_Total_2011"])
}
totalCommuterNut

commuterExceed_Nut <- list()       #List for commuters who exceed 45 min
for(county in 1:6){
  commuterExceed_Nut[county] <- sum(sum(filter(data3, data3$County == counties[county,])[,"Journey_Time_Three_Quarter_Hours_To_Under_One_Hour_2011"]),
                       sum(filter(data3, data3$County == counties[county,])[,"Journey_Time_One_Hour_To_Under_One_Hour_Thirty_Mins_2011"]),
                       sum(filter(data3, data3$County == counties[county,])[,"Journey_Time_One_And_Half_Hours_And_Over_2011"]))
}
commuterExceed_Nut

propExceedNut <- list()
for(n in 1:6){
  propExceedNut <- unlist(commuterExceed_Nut[n])/unlist(totalCommuterNut[n])
  print(str_c("Proportion of commuters who travel for more than 45 minutes in ", counties[n,], " is ", 
              round(propExceedNut, 3)))
}


#Q7 The residents of five counties who experience the longest commute times
data_by_county <- data %>% group_by(County) %>% summarise(sum(Journey_Time_One_And_Half_Hours_And_Over_2011))
colnames(data_by_county)[2] <- "Journey_Time_One_And_Half_Hours_And_Over_2011"

top5 <- tail(sort(data_by_county$Journey_Time_One_And_Half_Hours_And_Over_2011),5)

print("The below five Counties experience the longest commute time")

for(i in 1:33){
  for(j in 1:5){
    if(data_by_county$Journey_Time_One_And_Half_Hours_And_Over_2011[i]==top5[j]){
      print(as.character(data_by_county$County[i]))
    }
  }
}


#Q8 Proportion of cars used in the morning commute that contain only one person
carUser_Total <- data %>% transmute(data$Car_Driver_2011 + data$Car_Passenger_2011)
colnames(carUser_Total)[1] <- "Total Car Users"

one_person_car <- sum(nrow(subset(data,data$Car_Passenger_2011 == 1)),
                    nrow(subset(data,data$Car_Driver_2011 == 1)))

one_person_car_proportion <- one_person_car/sum(carUser_Total)

print(str_c("The proportion of cars used in the morning commute that contains only one person is ", 
      one_person_car_proportion))



#Q10 Electoral division prioritisation
PlanningRegionList <- data %>% distinct(Planning.Region)
PlanningRegionList <- PlanningRegionList[!apply(PlanningRegionList == "", 1, all),]
PlanningRegionList

for(x in 1:3) {
  sum_public_transport = data %>% 
    filter(Planning.Region == PlanningRegionList[x]) %>%
    group_by(Electoral.Division.Name) %>%
    summarise(sum(Public_Transport_Comb_2011))
  
  sum_total = data %>% filter(Planning.Region == PlanningRegionList[x]) %>%
    group_by(Electoral.Division.Name)  %>%
    summarise(sum(Total_2011))
  public_proportion = sum_public_transport$`sum(Public_Transport_Comb_2011)`/sum_total$`sum(Total_2011)`
  
  print(str_c("For Planning Region ",PlanningRegionList[x], 
              ", it is proposed to invest in public transportation in ", 
              sum_public_transport$Electoral.Division.Name[which.min(public_proportion)],
              " since the proportion of public transport is ", round(min(public_proportion),2), 
              " which is least within this planning region"))
}





