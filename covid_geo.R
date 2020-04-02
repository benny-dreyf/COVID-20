### covid daily reports compiled
library(tidyverse)
library(lubridate)

### daily report compilation

file_list<- list.files('/Users/BenDreyfuss/drepo/COVID-20/csse_covid_19_data/csse_covid_19_daily_reports/', pattern = '*.csv')
file_list
map_list<- map(.x= ' ', paste0, 
               '/Users/BenDreyfuss/drepo/COVID-20/csse_covid_19_data/csse_covid_19_daily_reports/', file_list, 
               collapse = "")  %>% 
  str_split(pattern = ' ')

# covid_all<- tibble()
# for (i in seq_along(map_list[[1]])){
#   tmp<- read_csv(map_list[[1]][i]) 
#   covid_all<- bind_rows(tmp, covid_all)
# }
# covid_all
map_list

# break data into two lists due to date formatting for files '2020-02-01' and prior
prior_list<- map_list[[1]][2:12]
prior_list
mid_list<- map_list[[1]][13:62]
mid_list
new_list<- map_list[[1]][-1:-62]
new_list

# combine files that parse `last_update` as chr
covid_char<-map(.x = prior_list, .f = read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names(case= 'snake') %>% 
  mutate(last_update= mdy_hm(last_update)) %>% 
  rename(date= last_update) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(date= as_date(date))
glimpse(covid_char)

# combine files that parse `last_update` as datetime
covid_datetime<- map(.x = mid_list, .f = read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names(case= 'snake') %>% 
  rename(date= last_update) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(date= as_date(date)) %>% 
  select(province_state, country_region, date, confirmed, deaths, recovered)
glimpse(covid_datetime)

# combine the latest update with better dimensions... 
covid_state<- map(.x = new_list, .f = read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names(case= 'snake') %>% 
  rename(date= last_update, city= combined_key) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(date= as_date(date)) %>% 
  select(province_state, country_region, city, date, confirmed, deaths, recovered) 
glimpse(covid_state)

# map(.x = new_list, .f = read_csv) %>% 
#   bind_rows() %>% 
#   glimpse()

# combine these guys
covid_all<- bind_rows(covid_char, covid_datetime, covid_state) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  select(country_region, province_state, city, everything())
glimpse(covid_all)

# create US dataframe
covid_us<- covid_all %>% 
  filter(country_region== 'US')



###### experiment with long-lat
# things

geo_1<- map(.x = mid_list, .f = read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names(case= 'snake') %>% 
  rename(date= last_update, city= combined_key) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(date= as_date(date)) %>% 
  select(province_state, country_region, city, admin2, long, lat, date, confirmed, deaths, recovered)

geo_2<- map(.x = new_list, .f = read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names(case= 'snake') %>% 
  rename(date= last_update, city= combined_key) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(date= as_date(date)) %>% 
  select(province_state, country_region, city, admin2, long, lat, date, confirmed, deaths, recovered)

geo_final<- bind_rows(geo_1, geo_2) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
glimpse(geo_final)
tail(geo_final)

geo_us<- geo_final %>% 
  filter(country_region== 'US' & lat != 0) %>% 
  select(province_state, city, long, lat, date, confirmed, deaths) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
glimpse(geo_us)


geo_us %>% 
  filter(lat== 0) %>% 
  select(province_state, date) %>% 
  unique() 
