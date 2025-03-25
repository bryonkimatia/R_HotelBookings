#To clean data, we install the required packages and load the respective libraries
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)

#We import data for this case in excel format
hotel_bookings <- read_excel("D:/Portfolio/R PROGRAMMING/hotel_bookings.xls")

#Getting to know our dat using functions
head(hotel_bookings)
colnames(hotel_bookings)
str(hotel_bookings)

#We can arrange our data and create a new data frame
hotel_bookings_v2 <- hotel_bookings %>% 
  arrange(desc(lead_time))

head(hotel_bookings_v2)

#Maximum lead time
max(hotel_bookings$lead_time)
#Maximum lead time = 737

#Minimum lead time
min(hotel_bookings$lead_time)
#Minimum lead time = 0

#Mean lead time
mean(hotel_bookings$lead_time)
#Mean lead time = 104.1062

#We can create new dat containing only city hotels
hotel_bookings_city <- 
  filter(hotel_bookings, hotel_bookings$hotel=="City Hotel")

head(hotel_bookings_city)

#Mean lead time for city hotels therefore
mean(hotel_bookings_city$lead_time)
#Mean lead time for city hotels = 122.0809

#Summarizing the average, minimum and maximum lead time with respect to hotel
hotel_summary <- hotel_bookings %>%
  group_by(hotel) %>%
  summarise(average_lead_time=mean(lead_time), min_lead_time=min(lead_time), max_lead_time=max(lead_time))

head(hotel_summary)
#hotel        average_lead_time min_lead_time max_lead_time
#<chr>                    <dbl>         <dbl>         <dbl>
#1 City Hotel               122.            0           629
#2 Resort Hotel             92.7            0           737

#Creating visuals 
ggplot(data = hotel_bookings ) + geom_point(mapping = aes(x = hotel, y = lead_time))

