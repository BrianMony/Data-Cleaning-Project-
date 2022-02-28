#Loading the data into R
iot <- read.table("C:\\Users\\User\\Desktop\\IOT-temp.csv", sep=',', strip.white = T)

#Viewing the loaded data
head (iot)

#Inspecting the Data
names(iot)
str(iot)

#Appropriately renaming the five variables
library(tidyverse)

iot <- iot%>% rename(log_id=V1, room_id=V2, noted_date=V3,temp=V4, out_in=V5)

#Deleting the first row
iot <- subset(iot,iot$log_id!='id')
head(iot)

#Simplifying the id column and removing the room_id variable because of its irrelevance to future analysis

iot <- iot %>% mutate(log_id=gsub('\\__export__.temp_log_','', log_id)) %>% select(-room_id)

#Separating the noted_date variable into noted_date and noted_time

noted_time <- str_sub(iot$noted_date, start=11, end=18)

iot <- iot %>% mutate(noted_time)

iot <- iot %>% mutate(noted_date = str_sub(noted_date, start=0, end=10))


head (iot)


#Rectifying data types and making them suitable for analysis

iot <-iot %>% mutate(out_in = as.factor(out_in)) %>% mutate(temp = as.numeric(temp))

str(iot)

head (iot)

#Removing duplicates

iot <- iot %>% distinct(log_id, .keep_all = T)

head (iot)

#Checking invalid values

summary (iot)

#Checking for missing values

sum(is.na(iot))

apply (iot, 2, function(temp) sum(is.na(temp))/length(temp))

#The iot data is now clean and ready for analysis. Writing it into a transferable .csv file

#Importing the cleaned iot data set for analysis

iotc <- read.table("C:\\Users\\User\\Desktop\\iot.csv", header=T, sep=',',strip.white=T)

head(iotc)

#Obtaining the data set's descriptives

summary (iotc)

#Checking the data set for normality via a histogram of temperature

library(ggplot2)


plt <- ggplot(data=iotc, aes(x=temp)) +
  
  geom_histogram(aes(y=..density..), bins=20, col='black', fill= 'blue') + 
  
  stat_function(fun=dnorm, args=list(mean= mean(iotc$temp), sd=sd(iotc$temp)), col='red', lwd=1) +
  
  labs(title='Histogram of Temperture Changes Inside and Outside a Room Over Time', subtitle="India's Data", x='Temperature')

#Generating a box plot for temperature comparisons in and outside the room

plt_2 <- ggplot(data=iotc, aes(x= out_in, y=temp)) +
  
  geom_boxplot(varwidth =T, aes(fill= out_in, alpha=0.5)) +
  
  labs(title='A Boxplot for Temperature Comparisons In and Outside Rooms in India Over Time', subtitle ='Indian Bureau Data', x='Room-Relative Collecion Point', y= 'Tempmerature', colour='Collecion Point')
  
#Rendering both plots

par(mfrow=c(2,1))

plt

plt_2


