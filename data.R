library(dplyr)
library(ggplot2) 
library(nycflights13)
library(tidyverse)
library(mice)
library(reshape2)
library(magrittr)


#missing values

md.pattern(flights)
new_flights <- flights %>%
  drop_na(dep_time) %>%
  drop_na(arr_time) 

new_flights <- new_flights %>%
  mutate(
    arr_delay = ifelse(is.na(arr_delay), arr_time- sched_arr_time, arr_delay),
    air_time = ifelse(is.na(air_time),arr_time - dep_time, air_time)
  ) %>%
  filter(air_time > 0 )

md.pattern(new_flights)
#outliers
# new_flights %>% melt() %>%
#   ggplot(aes(NULL,value)) +
#   geom_boxplot(aes(fill = variable)) +
#   facet_wrap(variable ~. , scales = 'free_y')

new_flights <- new_flights[!new_flights$arr_delay < -1000,]

View(new_flights)

#1.new_flights
new_flights <- new_flights %>%
  mutate(weekdays = weekdays(as.Date(paste(new_flights$year,new_flights$month,new_flights$day,sep = "-"))),
    delay_band = case_when(  arr_delay <15 ~ "on_time", 
                             arr_delay >=15 & arr_delay < 45 ~ "medium_delay",
                             arr_delay >=45  ~ "large_delay"),
    if_delay = case_when(delay_band == "on_time" ~ 0, 
                         TRUE ~ 1))

View(new_flights)

#2.yearly
##2.1 summary
delay_flights <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  summarise(
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = mean(arr_delay,na.rm = FALSE)
            )

all_flights_summary <- new_flights %>%
  dplyr::summarise(all_count = n())   %>% 
  mutate(categary = "summary" ,
         round(delay_flights["delay_med_min"],digits =1),
         round(delay_flights["delay_avg_min"],digits =1),
         on_time = length(which(new_flights["arr_delay"]<15)),
         medium_delay = length(which(new_flights["arr_delay"]>=15 & new_flights["arr_delay"]<45)),
         large_delay = length(which(new_flights["arr_delay"]>=45 )),
         on_time_rate = round((on_time / all_count)*100,1),
         `name` = "",
         sub = ""
         )


View(all_flights_summary) 


##2.2 summary by carriers
delay_flights_carriers <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(carrier) %>%
  summarise(
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1)) 
 

all_flights_carriers_summary <- new_flights %>%
  dplyr::group_by(carrier) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(delay_flights_carriers, by = "carrier")
  
all_flights_carriers_summary_band <- new_flights %>%
  dplyr::group_by(carrier ,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")
 
all_flights_carriers_summary <- all_flights_carriers_summary %>%
  inner_join(all_flights_carriers_summary_band, by = "carrier") %>%
  inner_join(airlines, by = "carrier")  %>%
  mutate(categary = "carrier" ,
         on_time_rate = round((on_time/all_count)*100,1)) %>%
  dplyr::rename(sub = carrier)

View(all_flights_carriers_summary)



##2.3 summary by origin
delay_flights_origin <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(origin) %>%
  summarise( 
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

all_flights_origin_summary <- new_flights %>%
  dplyr::group_by(origin) %>%
  summarise(all_count = n())   %>% 
  inner_join(delay_flights_origin, by = "origin")

all_flights_origin_summary_band <- new_flights %>%
  dplyr::group_by(origin,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

all_flights_origin_summary <- all_flights_origin_summary %>%
  inner_join(all_flights_origin_summary_band, by = "origin") %>%
  left_join(select(airports, faa,name), by = c("origin"="faa"))  %>%
  mutate(categary = "origin" ,
         on_time_rate = round((on_time/all_count)*100,1)) %>%
  dplyr::rename(sub = origin)

View(all_flights_origin_summary)


##2.4 summary by dest
delay_flights_dest <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(dest) %>%
  summarise(delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

all_flights_dest_summary <- new_flights %>%
  dplyr::group_by(dest) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(delay_flights_dest, by = "dest")

all_flights_dest_summary_band <- new_flights %>%
  dplyr::group_by(dest,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

all_flights_dest_summary <- all_flights_dest_summary %>%
  inner_join(all_flights_dest_summary_band, by = "dest") %>% 
  left_join(select(airports, faa,name), by = c("dest"="faa"))  %>%
  mutate(categary = "dest" ,
         on_time_rate = round((on_time/all_count)*100,1))%>%
  dplyr::rename(sub = dest)

View(all_flights_dest_summary)


##2.5 summary by origin&dest
# delay_flights_od <- new_flights %>%
#   filter(if_delay == 1)  %>%
#   group_by(origin,dest) %>%
#   summarise( 
#             delay_med_min = median(arr_delay,na.rm = FALSE),
#             delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))
# 
# all_flights_od_summary <- new_flights %>%
#   group_by(origin,dest) %>%
#   summarise(all_count = n())   %>% 
#   inner_join(delay_flights_od, by = c("origin","dest"))
# 
# all_flights_od_summary_band <- new_flights %>%
#   group_by(origin,dest,delay_band) %>%
#   summarise(count = n())   %>%
#   pivot_wider(names_from = "delay_band",
#               values_from = "count")
# 
# all_flights_od_summary <- all_flights_od_summary %>%
#   inner_join(all_flights_od_summary_band, by = c("origin","dest")) %>%
#   mutate(on_time_rate = round((on_time/all_count)*100,1))
# 
# View(all_flights_od_summary)

#merge
all_summary <- rbind(all_flights_summary,
                     all_flights_carriers_summary,
                     all_flights_dest_summary,
                     all_flights_origin_summary)
View(all_summary)


#3.monthly
##3.1 month summary
month_delay_flights <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(month) %>%
  summarise( 
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_summary <- new_flights %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(month_delay_flights, by = "month")

month_flights_summary_band <- new_flights %>%
  dplyr::group_by(month,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_summary <- month_flights_summary %>%
  inner_join(month_flights_summary_band, by = "month") %>%
  mutate(categary = "summary" ,
         on_time_rate = round((on_time/all_count)*100,1),
         `name` = "",
         sub = "")
 
View(month_flights_summary) 

##3.2 month summary by carriers
month_delay_flights_carriers <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(month,carrier) %>%
  summarise( 
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_carriers_summary <- new_flights %>%
  dplyr::group_by(month,carrier) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_carriers, by = c("month","carrier"))

month_flights_carriers_summary_band <- new_flights %>%
  dplyr::group_by(month,carrier,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_carriers_summary <- month_flights_carriers_summary %>%
  inner_join(month_flights_carriers_summary_band, by = c("month","carrier")) %>%
  inner_join(airlines, by = "carrier")  %>%
  mutate(on_time_rate = ifelse(is.na(on_time),0,round((on_time/all_count)*100,1)),
         categary = "carrier" ,) %>%
  dplyr::rename(sub = carrier)

View(month_flights_carriers_summary)


##3.3 month summary by origin
month_delay_flights_origin <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(month,origin) %>%
  summarise( 
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_origin_summary <- new_flights %>%
  dplyr::group_by(month,origin) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_origin, by = c("month","origin"))

month_flights_origin_summary_band <- new_flights %>%
  dplyr::group_by(month,origin,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_origin_summary <- month_flights_origin_summary %>%
  inner_join(month_flights_origin_summary_band, by = c("month","origin")) %>% 
  left_join(select(airports, faa,name), by = c("origin"="faa"))  %>%
  mutate(on_time_rate = round((on_time/all_count)*100,1),
         categary = "origin") %>%
  dplyr::rename(sub = origin)

View(month_flights_origin_summary)

##3.4 month summary by dest
month_delay_flights_dest <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(month,dest) %>%
  summarise(
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_dest_summary <- new_flights %>%
  dplyr::group_by(month,dest) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_dest, by = c("month","dest"))

month_flights_dest_summary_band <- new_flights %>%
  dplyr::group_by(month,dest,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_dest_summary <- month_flights_dest_summary %>%
  inner_join(month_flights_dest_summary_band, by = c("month","dest")) %>%
  left_join(select(airports, faa,name), by = c("dest"="faa"))  %>%
  mutate(on_time_rate = ifelse(is.na(on_time),0,round((on_time/all_count)*100,1)),
         categary = "dest") %>%
  dplyr::rename(sub = dest)

View(month_flights_dest_summary)

##3.5 month summary by carriers&origin
month_delay_flights_carriers_origin <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(month,carrier,origin) %>%
  summarise( 
    delay_med_min = median(arr_delay,na.rm = FALSE),
    delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_carriers_origin_summary <- new_flights %>%
  dplyr::group_by(month,carrier,origin) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_carriers_origin, by = c("month","carrier","origin"))

month_flights_carriers_origin_summary_band <- new_flights %>%
  dplyr::group_by(month,carrier,origin,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_carriers_origin_summary <- month_flights_carriers_origin_summary %>%
  inner_join(month_flights_carriers_origin_summary_band, by = c("month","carrier","origin")) %>%
  inner_join(airlines, by = "carrier")  %>%
  left_join(select(airports, faa,name), by = c("origin"="faa"))  %>%
  mutate(on_time_rate = ifelse(is.na(on_time),0,round((on_time/all_count)*100,1)),
         categary = "carrier_origin" ,) %>%
  dplyr::rename(sub = carrier, carrier_name = name.x, origin_name = name.y)

View(month_flights_carriers_origin_summary)


##merge----------------------------------------------
month_summary <- rbind(month_flights_summary,
                       month_flights_carriers_summary,
                       month_flights_origin_summary,
                       month_flights_dest_summary)
View(month_summary)




#4. daily summary
##4.1 daily summary
day_delay_flights <- new_flights %>%
  dplyr::filter(if_delay == 1)  %>%
  dplyr::group_by(month,day) %>%
  summarise( 
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

day_flights_summary <- new_flights %>%
  dplyr::group_by(month,day) %>%
  dplyr::summarise(all_count = n())   %>% 
  inner_join(day_delay_flights, by = c("month","day"))

day_flights_summary_band <- new_flights %>%
  dplyr::group_by(month,day,delay_band) %>%
  dplyr::summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

day_flights_summary <- day_flights_summary %>%
  inner_join(day_flights_summary_band, by =  c("month","day")) %>%
  mutate(categary = "summary" ,
         on_time_rate = ifelse(is.na(on_time),0,round((on_time/all_count)*100,1)),
         `name` = "",
         sub = "") 

View(day_flights_summary) 

#5. flights_with_weather
flights_with_weather <- new_flights %>%
  inner_join(weather, by = c("origin", "month", "day","hour")) %>%
  drop_na(temp)  

flights_with_weather <- flights_with_weather %>%
  mutate(id = rownames(flights_with_weather)) %>%
  select(id, month, day, hour, dep_delay, arr_delay, carrier, origin, dest, if_delay, delay_band,
         temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib)

flights_with_weather$wind_dir[c(is.na(flights_with_weather$wind_dir) == TRUE)] <- 0
flights_with_weather$wind_speed[c(is.na(flights_with_weather$wind_speed) == TRUE)] <- 0 
flights_with_weather$wind_gust[c(is.na(flights_with_weather$wind_gust) == TRUE)] <- 0

View(flights_with_weather) 
 
##save all data to file ----------------------
save(new_flights, all_summary, 
     month_summary,month_flights_carriers_origin_summary,
     day_flights_summary, flights_with_weather,
     file = "data.Rdata")

 