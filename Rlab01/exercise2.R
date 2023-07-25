nyc_flights = flights

# first let's create a column corresponding to the full dates of the flights of the date type data

nyc_flights = unite(nyc_flights , date , year , month , day , sep = "-" )
nyc_flights$date = ymd(nyc_flights$date)

# First we need to group by the airports, summarizing by counting the number of flights:

# next we group by the dates and summarize by counting the number of flights for each date

flight_count_summary = nyc_flights %>% 
  group_by(date , origin) %>%
  summarize(flight_counts = n())

ggplot(data = flight_count_summary, aes(x = date, y = flight_counts, color = origin)) +
  labs(x = "Date", y = "Flight Counts per Date") +
  geom_line()+
  scale_x_date(date_breaks = "20 day", date_labels = "%D") 
  

ggsave("flights per day.png", width = 15, height = 6)

# to plot the flights per week, first we create a column containing the week number

flights_week = flight_count_summary
flights_week = flights_week %>%
  mutate(week = week(date))

#then we add a column containing the name of the day of the given date 

flights_week = flights_week %>%
  mutate(day = wday(date, label = TRUE))

# Next we filter the weekend
flights_workdays = flights_week %>% 
  filter(!day %in% c("Sat", "Sun"))

# Finally we average over the weekdays and create a column
flights_weekdays_avg = flights_workdays %>% 
  group_by(week, origin) %>% 
  summarize(avg_flights = mean(flight_counts))

ggplot(flights_weekdays_avg, aes(x = week, y = avg_flights, fill = origin)) +
  geom_col(position = "dodge") +
  labs(x = "Week Number", y = "Average Number of Flights in Weekdays") +
  theme_bw() +
  scale_x_continuous(breaks = flights_weekdays_avg$week)

ggsave("average flight in weekdays.png", width = 15, height = 6)

# To extract the same information for the the weekends this time we exclude the workingdays from the original data:

flights_weekends = flights_week %>% 
  filter(!day %in% c("Mon", "Tue" , "Wed" , "Thu" , "Fri"))

flights_weekends_avg = flights_weekends %>% 
  group_by(week, origin) %>% 
  summarize(avg_flights = mean(flight_counts))

ggplot(flights_weekends_avg, aes(x = week, y = avg_flights, fill = origin)) +
  geom_col(position = "dodge") +
  labs(x = "Week Number", y = "Average Number of Flights in Weekends") +
  theme_bw() +
  scale_x_continuous(breaks = flights_weekdays_avg$week)

ggsave("average flight in weekends.png", width = 15, height = 6)

# Now we use the group by method again to extract the minimum and maximum and average departure delay of the flights in each day

flight_delay_summary = nyc_flights %>% 
  group_by(date , origin) %>%
  summarize(minimum_delay = min(dep_delay,na.rm = TRUE) , maximum_delay = max(dep_delay,na.rm = TRUE), average_delay = mean(dep_delay,na.rm = TRUE))
  
# Now we can plot our data

ggplot(data = flight_delay_summary, aes(x = date, y = minimum_delay, color = origin)) +
  labs(x = "Date", y = "Minimum Delay Time by Airport(mins)") +
  geom_line()+
  scale_x_date(date_breaks = "20 day", date_labels = "%D") 

ggsave("minimum delay by airport.png", width = 15, height = 6)

ggplot(data = flight_delay_summary, aes(x = date, y = maximum_delay, color = origin)) +
  labs(x = "Date", y = "Maximum Delay Time by Airport(mins)") +
  geom_line()+
  scale_x_date(date_breaks = "20 day", date_labels = "%D") 

ggsave("maximum delay by airport.png", width = 15, height = 6)

ggplot(data = flight_delay_summary, aes(x = date, y = average_delay, color = origin)) +
  labs(x = "Date", y = "Average Delay Time by Airport(mins)") +
  geom_line()+
  scale_x_date(date_breaks = "20 day", date_labels = "%D") 

ggsave("average delay by airport.png", width = 15, height = 6)

# Next we calculate the average speed of each plane, and then average over for each day

flights_speed = nyc_flights %>%
  mutate(speed = distance / air_time / 60)

flights_speed_summary = flights_speed %>%
  group_by(date) %>%
  summarize(average_speeds = mean(speed,na.rm = TRUE))

ggplot(data = flights_speed_summary, aes(x = date, y = average_speeds)) +
  labs(x = "Date", y = "Average Speed of the Flights by Date of Departure (mile/hour)") +
  geom_line()+
  scale_x_date(date_breaks = "20 day", date_labels = "%D") 

ggsave("average speed of the flights.png", width = 15, height = 6)

# To get the information about the airlines, we group by the carrier and dates first

flights_airlines_daysummary = nyc_flights %>%
  group_by(carrier , date) %>%
  summarize(flight_count = n())

#now to get the average number of flights per day of an airline, we group by airline again and this time we average over the flights count to summarize

airlines_average_day = flights_airlines_daysummary %>%
  group_by(carrier) %>%
  summarize(average_flight_perday = mean(flight_count , na.rm = TRUE))

ggplot(airlines_average_day , aes(x = carrier, y = average_flight_perday , fill = carrier)) +
  geom_col(position = "dodge") +
  labs(x = "Airline", y = "Average Number of Flights per Day by Airline") +
  theme_bw()+
  theme(legend.position = "none")

ggsave("average flights per day by airline.png", width = 15, height = 6)

# To get the same data for weeks, we add a week column to the initial tibble

flights_airlines_weeksummary = nyc_flights %>%
  mutate(week = week(date))

flights_airlines_weeksummary = flights_airlines_weeksummary %>%
  group_by(carrier , week) %>%
  summarize(flights_count = n())

airlines_average_week = flights_airlines_weeksummary %>%
  group_by(carrier) %>%
  summarize(average_flight_perweek = mean(flights_count , na.rm = TRUE))

ggplot(airlines_average_week , aes(x = carrier, y = average_flight_perweek , fill = carrier)) +
  geom_col(position = "dodge") +
  labs(x = "Airline", y = "Average Number of Flights per Week by Airline") +
  theme_bw()+
  theme(legend.position = "none")

ggsave("average flights per week by airline.png", width = 15, height = 6)

# The same can be done for months:


flights_airlines_monthsummary = nyc_flights %>%
  mutate(month = month(date))

flights_airlines_monthsummary = flights_airlines_monthsummary %>%
  group_by(carrier , month) %>%
  summarize(flights_count = n())

airlines_average_month = flights_airlines_monthsummary %>%
  group_by(carrier) %>%
  summarize(average_flight_permonth = mean(flights_count , na.rm = TRUE))

ggplot(airlines_average_month , aes(x = carrier, y = average_flight_permonth , fill = carrier)) +
  geom_col(position = "dodge") +
  labs(x = "Airline", y = "Average Number of Flights per Month by Airline") +
  theme_bw()+
  theme(legend.position = "none")

ggsave("average flights per month by airline.png", width = 15, height = 6)

# to get information about distance we return to the original tibble

airlines_distance_per_month = nyc_flights %>%
  mutate(month = month(date))

airlines_distance_summary = airlines_distance_per_month %>%
  group_by(carrier , month) %>%
  summarize(distance_travelled = sum(distance))

airlines_distance_per_month = airlines_distance_per_month %>%
  group_by(carrier) %>%
  summarize(average_distance_travelled = mean(distance))

ggplot(airlines_distance_per_month, aes(x = carrier, y = average_distance_travelled , fill = carrier)) +
  geom_col(position = "dodge") +
  labs(x = "Airline", y = "Average Distance Travelled per Month by Airline") +
  theme_bw()+
  theme(legend.position = "none")

ggsave("average flights per month by airline.png", width = 15, height = 6)

