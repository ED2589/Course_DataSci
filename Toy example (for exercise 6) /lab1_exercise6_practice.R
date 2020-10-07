library(dplyr)
library(tidyverse)
library(lubridate)

##################################
### calculating census volumes  ##
##################################
# REFERENCE: https://science.data.blog/2018/10/10/calculating-volumes-from-timestamps-using-r/

ed_data <- read_csv('raw_ed_data.csv')
dim(ed_data)
glimpse(ed_data)

# sample
ed_data %>%
  filter(ed_start_time <= ymd_hms('2019-09-15 10:00:00'),
         ed_end_time >= ymd_hms('2019-09-15 10:00:00')) %>%
  nrow()

######################### legit data #################################
# ANSWER: This is following the tutorial on calculating the census volumes (LINK tutorial) to get the following dataframe.
# with the timestamps arranged by chronological order, and 'counter' column is the number of new patients that arrived or departed
# the ED at that hour, and 'volume' column being the  cumulative number of patients (i.e. census volumes) at each hour.
# Since we want to find the *average* census volumes at each hour, we need to do some additional data manipulation.

ed_data <- ed_data[complete.cases(ed_data[ , 4:5]), ] # remove rows for which the arrival or departure time columns are NA
# round all arrival and departure times to nearest hour
floor_arrive <- floor_date(ed_data$ed_start_time, unit = 'hour')
floor_depart <- floor_date(ed_data$ed_end_time, unit = 'hour')
ed_data <- ed_data %>%
  mutate(ed_start_time = floor_arrive, ed_end_time = floor_depart)
#glimpse(ed_data)
# counter 1 for each ED arrival timestamp
arrive <- ed_data %>%
  select(time_mark = ed_start_time ) %>%
  mutate(counter = 1)
# counter -1 for each ED departure timestamp
depart <- ed_data %>%
  select(time_mark = ed_end_time) %>%
  mutate(counter = -1)

# census volumes
volumes <- arrive %>%
bind_rows(depart) %>%  # combine arrive/depart timestamps and counters (+1 or -1)
  arrange(time_mark, counter) %>%  # sort by ascending timestamp, and counters
  slice(-1) %>% # remove 1st row (year 2018 record - data quality issue)
  mutate(volume = cumsum(counter)) # cumulative sum each counter to get patient volume at given time

# sequence of consecutive dates by hour
min_time <- min(volumes$time_mark)
max_time <- max(volumes$time_mark)
seq_time_mark <- tibble(time_mark = seq(min_time, max_time, by = 'hour'))

# carry down census volume for timestamps with no arrival or departure
volumes <- volumes %>%
  right_join(seq_time_mark,
             by = 'time_mark') %>%
  arrange(time_mark) %>%
  fill(volume, .direction = 'down')

volumes
# try plotting
# volumes %>%
#   ggplot(aes(time_mark, volume)) +
#   geom_line() +
#   xlab('Date') +
#   ggtitle('Emergency Department Census',
#           subtitle = 'In hourly intervals')

## ANSWER: Our goal is to find the *average* ED patient volume at each hour, and thie data frame above is not enough to accomplish this.
## We need to sum up the `counter` column for each hour and for each date to get the hourly change in volume for that date in that hourly timeframe.
## Then, we would obtain the cumulative sum of the hourly change in volume, which gives the ED volume at that hour on a specific date.
## To find the average volume by hour, we would aggregate by hour and calculate the mean for each hour. The code is below.
## Note that since we do not have records available from the last day of 2018, we would assume an initial ED volume of 0.

# WHERE PROBLEM BEGINS
counter_sum <- volumes$counter %>%
              aggregate(by = list(Time_mark = volumes$time_mark), FUN = sum) %>% # sum counters for EACH hourly timeframe for each consecutive date
              rename(hourly_change = x) %>% # rename column
              mutate(hourly_volume = cumsum(hourly_change)) %>% # cumulative sum hourly change to get hourly volume
              mutate(Hour = format(ymd_hms(counter_sum$Time_mark), '%H:%M:%S' )) # extract hour from `time_mark` column for aggregation

# calculate average hourly volume
hourly_volume <- counter_sum$hourly_volume %>%
                aggregate( by = list(Hour = counter_sum$Hour), FUN = mean, na.rm = TRUE ) %>% # calculate avergae volume by hour
                rename(average_volume = x) %>% # rename
                mutate(average_volume = floor(average_volume)) %>%    # round down to nearest integer
                arrange(desc(average_volume)) # sort volume by descending order
                knitr::kable()

## ANSWER: The data frame above shows average hourly census in the ED from January 2019 to January 2020.
                # The 'Hour' column actually indicates *hourly* timeframes, rather than at that specific time.
                # For example, the `20:00:00` Hour actually denotes the time frame from 8pm to 9pm, rather than exactly at
                # 8pm.
                # **From the output we see the highest average census occurs from 8pm to 11pm, with there being an average of 74 patients in the ED per hour.**


 # ignore below
# !! use this to create a new column with the hour, then aggregate by hour for all dates, then find mean (i.e. avg # at that hour)
format(ymd_hms(ed_data$ed_start_time), '%H:%M:%S')


######################### toy example #################################

toy <- read_csv("simulated_ed_data.csv")
glimpse(toy)

### METHOD 1 ###

floor_arrival2 <- floor_date(toy$arrival_times, unit = 'minute')
floor_depart2 <- floor_date(toy$depart_times, unit = 'minute')

# rouud all arrival and departure times to nearest min
toy <- toy %>%
  mutate(arrival_times = floor_arrival2, depart_times = floor_depart2)
glimpse(toy)

# arrival timestamps , counter = 1 for each
arrive2 <- toy %>%
  select(timestamp = arrival_times)%>% # rename and extract `arrival_time` column
 mutate(counter = 1) # append column of 1's

depart2 <- toy %>%
  select(timestamp = depart_times) %>% # rename and extract `depart_time` column
  mutate(counter = -1) # append column of -1's

census_volumes2 <- arrive2 %>%
  bind_rows(depart2) %>%
  arrange(timestamp, counter) %>%  # arrange by time
  mutate(volume = cumsum(counter)) # take cumulative sum of the counter

start2 <- min(census_volumes2$timestamp) # earliest timestamp (out of all arrival and depart times)
end2 <- max (census_volumes2$timestamp) # latest timestamp ('')
fuLL_window2 <- tibble(timestamp = seq(start2, end2, by = 'mins')) # a vector of consecutive time window by minute

census_vol2 <- census_volumes2 %>%
     right_join(fuLL_window2, by = 'timestamp') %>% # bind with the full time window to fill gaps
     arrange(timestamp) %>%
    fill(volume, .direction = 'down')
