library(dplyr)
library(ggplot2) 
library(nycflights13)
library(tidyverse)

#1.明细表
new_flights <- flights %>%
  mutate(weekdays = weekdays(as.Date(paste(flights$year,flights$month,flights$day,sep = "-"))),
    delay_band = case_when( is.na(arr_delay) ~ "unknown",
                                 arr_delay <=0 ~ "on time",
                                 arr_delay >0 & arr_delay <= 15 ~ "small_delay",
                                 arr_delay >15 & arr_delay <= 45 ~ "medium_delay",
                                 arr_delay >45 & arr_delay <= 180 ~ "large_delay",
                                 arr_delay >180 ~ "huge_delay"))

View(new_flights)

#2.年度汇总表
##2.1 总体summary
delay_flights <- new_flights %>%
  filter(arr_delay > 0)  %>%
  summarise(count = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = mean(arr_delay,na.rm = FALSE)
            )

all_flights_summary <- new_flights %>%
  summarise(all_c = n())   %>% 
  mutate(delay_per = round((1-delay_flights$count / all_c)*100,1),
         round(delay_flights["delay_med_min"],digits =1),
         round(delay_flights["delay_avg_min"],digits =1),
         delay_flights= delay_flights$count,
         on_time = length(which(new_flights["arr_delay"]<=0)),
         small_delay = length(which(new_flights["arr_delay"]>0 & new_flights["arr_delay"]<=15)),
         medium_delay = length(which(new_flights["arr_delay"]>15 & new_flights["arr_delay"]<=45)),
         large_delay = length(which(new_flights["arr_delay"]>45 & new_flights["arr_delay"]<=180)),
         huge_delay = length(which(new_flights["arr_delay"]>180))
         )

View(all_flights_summary) 


##2.2 summary by carriers
delay_flights_carriers <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(carrier) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))
  
all_flights_carriers_summary <- new_flights %>%
  group_by(carrier) %>%
  summarise(all_count = n())   %>% 
  inner_join(delay_flights_carriers, by = "carrier")
  
all_flights_carriers_summary_band <- new_flights %>%
  group_by(carrier,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")
 
all_flights_carriers_summary <- all_flights_carriers_summary %>%
  inner_join(all_flights_carriers_summary_band, by = "carrier") %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(all_flights_carriers_summary)



##2.3 summary by origin
delay_flights_origin <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(origin) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

all_flights_origin_summary <- new_flights %>%
  group_by(origin) %>%
  summarise(all_count = n())   %>% 
  inner_join(delay_flights_origin, by = "origin")

all_flights_origin_summary_band <- new_flights %>%
  group_by(origin,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

all_flights_origin_summary <- all_flights_origin_summary %>%
  inner_join(all_flights_origin_summary_band, by = "origin") %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(all_flights_origin_summary)


##2.4 summary by dest
delay_flights_dest <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(dest) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

all_flights_dest_summary <- new_flights %>%
  group_by(dest) %>%
  summarise(all_count = n())   %>% 
  inner_join(delay_flights_dest, by = "dest")

all_flights_dest_summary_band <- new_flights %>%
  group_by(dest,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

all_flights_dest_summary <- all_flights_dest_summary %>%
  inner_join(all_flights_dest_summary_band, by = "dest") %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(all_flights_dest_summary)


##2.5 summary by origin&dest
delay_flights_od <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(origin,dest) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

all_flights_od_summary <- new_flights %>%
  group_by(origin,dest) %>%
  summarise(all_count = n())   %>% 
  inner_join(delay_flights_od, by = c("origin","dest"))

all_flights_od_summary_band <- new_flights %>%
  group_by(origin,dest,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

all_flights_od_summary <- all_flights_od_summary %>%
  inner_join(all_flights_od_summary_band, by = c("origin","dest")) %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(all_flights_od_summary)


#3.月度汇总表
##3.1 month summary
month_delay_flights <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(month) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_summary <- new_flights %>%
  group_by(month) %>%
  summarise(all_count = n())   %>% 
  inner_join(month_delay_flights, by = "month")

month_flights_summary_band <- new_flights %>%
  group_by(month,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_summary <- month_flights_summary %>%
  inner_join(month_flights_summary_band, by = "month") %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))
 
View(month_flights_summary) 

##3.2 month summary by carriers
month_delay_flights_carriers <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(month,carrier) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_carriers_summary <- new_flights %>%
  group_by(month,carrier) %>%
  summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_carriers, by = c("month","carrier"))

month_flights_carriers_summary_band <- new_flights %>%
  group_by(month,carrier,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_carriers_summary <- month_flights_carriers_summary %>%
  inner_join(month_flights_carriers_summary_band, by = c("month","carrier")) %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(month_flights_carriers_summary)


##3.3 month summary by origin
month_delay_flights_origin <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(month,origin) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_origin_summary <- new_flights %>%
  group_by(month,origin) %>%
  summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_origin, by = c("month","origin"))

month_flights_origin_summary_band <- new_flights %>%
  group_by(month,origin,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_origin_summary <- month_flights_origin_summary %>%
  inner_join(month_flights_origin_summary_band, by = c("month","origin")) %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(month_flights_origin_summary)

##3.4 month summary by dest
month_delay_flights_dest <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(month,dest) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

month_flights_dest_summary <- new_flights %>%
  group_by(month,dest) %>%
  summarise(all_count = n())   %>% 
  inner_join(month_delay_flights_dest, by = c("month","dest"))

month_flights_dest_summary_band <- new_flights %>%
  group_by(month,dest,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

month_flights_dest_summary <- month_flights_dest_summary %>%
  inner_join(month_flights_dest_summary_band, by = c("month","dest")) %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(month_flights_dest_summary)

#4.日汇总表
##4.1 daily summary
day_delay_flights <- new_flights %>%
  filter(arr_delay > 0)  %>%
  group_by(month,day) %>%
  summarise(delay_flights = n(),
            delay_med_min = median(arr_delay,na.rm = FALSE),
            delay_avg_min = round(mean(arr_delay,na.rm = FALSE),1))

day_flights_summary <- new_flights %>%
  group_by(month,day) %>%
  summarise(all_count = n())   %>% 
  inner_join(day_delay_flights, by = c("month","day"))

day_flights_summary_band <- new_flights %>%
  group_by(month,day,delay_band) %>%
  summarise(count = n())   %>%
  pivot_wider(names_from = "delay_band",
              values_from = "count")

day_flights_summary <- day_flights_summary %>%
  inner_join(day_flights_summary_band, by =  c("month","day")) %>%
  mutate(delay_per = round((1-delay_flights/all_count)*100,1))

View(day_flights_summary) 


save(new_flights, all_flights_summary, all_flights_carriers_summary, 
     all_flights_origin_summary, all_flights_dest_summary, all_flights_od_summary, 
     month_flights_summary, month_flights_carriers_summary, 
     month_flights_origin_summary, month_flights_origin_summary, 
     day_flights_summary, 
     file = "data.Rdata")
