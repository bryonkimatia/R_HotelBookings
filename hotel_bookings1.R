# Setting up my environment
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)
library(readxl)

#We import data for this case in excel format
hotel_bookings <- read_excel("D:/Portfolio/R PROGRAMMING/hotel_bookings.xls")

#Getting to know our data using functions
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
ggplot(data = hotel_bookings ) + 
  geom_point(mapping = aes(x = hotel, y = lead_time, color=hotel))

#Plotting stays in weekend nights vs children
ggplot(data = hotel_bookings ) +
  geom_point(mapping = aes(x = children, y = stays_in_weekend_nights))

ggplot(data = hotel_bookings ) + geom_bar(mapping = aes(x = hotel,fill=hotel))

#We can visualise different visuals for the customer lead_time with respect to customer type
ggplot(data=hotel_bookings, aes(x = customer_type, y = lead_time)) + 
  geom_point(aes(color=customer_type)) +
  facet_wrap(~customer_type) +
  theme(axis.text.x = element_text(angle = 45))

#To find out which distribution channel has the most number of bookings
ggplot(data = hotel_bookings) +   
  geom_bar(mapping = aes(x = distribution_channel, fill=distribution_channel)) +   
  facet_wrap(~deposit_type) + 
  labs(title="Number of Bookings vs Distribution Channel") +
  theme(axis.text.x = element_text(angle = 45))

#What is the lead time in the first row created in a new data frame when you filter hotel as City Hotel and Market Segment as Online TA
onlineta_city_hotels_v2 <- hotel_bookings %>%
  filter(hotel=="City Hotel") %>%
  filter(market_segment=="Online TA")
#The value is 88

#We can compare market segments market segments by hotel type for hotel bookings
#From 2015 to 2017
min(hotel_bookings$arrival_date_year)
max(hotel_bookings$arrival_date_year)

mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment, fill=hotel)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", mindate, " to ", maxdate))
