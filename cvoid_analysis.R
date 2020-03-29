library(tidyverse)
library(lubridate)

confirmed<- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv') %>% 
  # glimpse()
  gather(key= date, value= count, -c('Province/State', 'Country/Region', 'Lat', 'Long')) %>% 
  janitor::clean_names(case= 'snake') %>% 
  mutate(date= as.Date.character(date, '%m/%d/%y'), metric= 'confirmed') %>% 
  select(province_state, country_region, lat, long, date, metric, count)
glimpse(confirmed)

deaths<- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv') %>% 
  # glimpse()
  gather(key= date, value= count, -c('Province/State', 'Country/Region', 'Lat', 'Long')) %>% 
  janitor::clean_names(case= 'snake') %>% 
  mutate(date= as.Date.character(date, '%m/%d/%y'), metric= 'deaths') %>% 
  select(province_state, country_region, lat, long, date, metric, count)
glimpse(deaths)

recovered<- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv') %>% 
  # glimpse()
  gather(key= date, value= count, -c('Province/State', 'Country/Region', 'Lat', 'Long')) %>% 
  janitor::clean_names(case= 'snake') %>% 
  mutate(date= as.Date.character(date, '%m/%d/%y'), metric= 'recovered') %>% 
  select(province_state, country_region, lat, long, date, metric, count)
glimpse(recovered)

covid_world<- bind_rows(confirmed, recovered, deaths)
glimpse(covid_world)

covid_world %>% 
  select(country_region) %>% 
  filter(str_detect(country_region, pattern = 'US')) %>% 
  unique()

covid_us<- covid_world %>% 
  select(-c(lat, long)) %>% 
  filter(str_detect(country_region, pattern = 'US')) %>% 
  group_by(country_region, province_state, date, metric) %>% 
  summarise_if(is.numeric, sum)
glimpse(covid_us)


covid_us %>% 
  filter(str_detect(province_state, pattern= 'New York County, NY') & str_detect(metric, pattern= 'confirmed')) %>%
  group_by(date, metric) %>%
  summarise_if(is.numeric, sum) %>%
  # glimpse() %>% 
  ggplot(aes(x= date, y= count, color= metric)) + geom_line()

covid_world %>% 
  filter(str_detect(country_region, pattern = "US") & str_detect(province_state, pattern = 'NY')) %>% 
  group_by(metric) %>% 
  summarise_if(is.numeric, sum)

recovered %>% 
  filter(str_detect(country_region, pattern = "US") & str_detect(province_state, pattern = 'NY')) %>% 
  group_by(metric) %>% 
  summarise_if(is.numeric, sum)

read_csv('/Users/BenDreyfuss/drepo/corona/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-20-2020.csv') %>% 
  janitor::clean_names(case= 'snake') %>% 
  filter(str_detect(province_state, pattern = 'New York County')) %>% 
  summarise_if(is.numeric, sum)


### daily report compilation

file_list<- list.files('/Users/BenDreyfuss/drepo/corona/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/', pattern = '*.csv')
file_list
map_list<- map(.x= ' ', paste0, 
               '/Users/BenDreyfuss/drepo/corona/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/', file_list, 
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
new_list<- map_list[[1]][-1:-12]
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
covid_datetime<- map(.x = new_list, .f = read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names(case= 'snake') %>% 
  rename(date= last_update) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(date= as_date(date)) %>% 
  select(province_state, country_region, date, confirmed, deaths, recovered, active)
glimpse(covid_datetime)

# combine these guys
covid_all<- bind_rows(covid_char, covid_datetime) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
glimpse(covid_all)

# checking work... "NEW YORK" is wonky... 
covid_all %>% 
  filter(country_region== 'US') %>% 
  select(province_state, confirmed, recovered, deaths) %>% 
  group_by(province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  arrange(desc(confirmed))

covid_all %>% 
  filter(province_state== 'New York' & date== '2020-03-21') %>% 
  select(province_state, confirmed, recovered, deaths) %>% 
  group_by(province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  arrange(desc(confirmed))

# looks linear, not a ton of data for NY
covid_all %>% 
  filter(date > '2020-03-04') %>% 
  filter(str_detect(province_state, pattern = 'New York') | str_detect(province_state, pattern = 'NY') ) %>% 
  select(date, confirmed, deaths) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(confirm_ln= log(confirmed)) %>% 
  gather(key= metric, value= count, -date) %>% 
  filter(metric== 'confirm_ln') %>% 
  ggplot(aes(x= date, y= count, color= metric)) + geom_line() + geom_smooth(method = 'lm' ,formula= y~x)


# col chart showing total volume of confirmed by top 10 populous states
covid_all %>% 
  filter(country_region== 'US') %>% 
  filter(province_state== 'California' | province_state== 'Illinois' |
           province_state== 'New York' | province_state== 'Florida' | province_state== 'Texas' |
           province_state== 'Pennsylvania' | province_state== 'Ohio' | province_state== 'Georgia' |
           province_state== 'North Carolina' | province_state== 'Michigan') %>%
  select(date, province_state, confirmed, deaths) %>% 
  group_by(province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  # arrange(desc(confirmed)) %>% 
  mutate(province_state= fct_reorder(.f= province_state, .x= confirmed, .desc= TRUE)) %>% 
  ggplot(aes(x= province_state, y= confirmed, fill= 'red')) + geom_col() +
  labs(title= 'Confirmed COVID-19 Cases- Highest Pop. States', subtitle= 'March 2020-Current', y= 'Confirmed COVID-19 Cases', x= 'State') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'none')

# top six most populous states over time
covid_all %>% 
  filter(country_region== 'US' & date > '2020-03-09') %>% 
  filter(province_state== 'California' | province_state== 'Illinois' |
           province_state== 'New York' | province_state== 'Florida' | province_state== 'Texas' |
           province_state== 'Pennsylvania') %>%
  select(date, province_state, confirmed, deaths) %>% 
  group_by(date, province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(ln_confirmed= log(confirmed)) %>%
  gather(key= metric, value= count, -date, -province_state) %>% 
  # arrange(desc(confirmed)) %>% 
  filter(metric== 'confirmed') %>% 
  ggplot(aes(x= date, y= count, color= province_state)) + geom_line() +
  labs(title= 'Confirmed COVID-19 Cases- Highest Pop. States', 
       subtitle= 'March 2020-Current', y= 'Confirmed COVID-19 Cases', x= 'Week') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right') +
  scale_color_manual(values = c('blue', 'grey', 'grey', '#ff0000', 'purple', 'grey'))

glimpse(covid_all)

covid_all %>% 
  select(province_state, confirmed, deaths) %>% 
  gather(key= metric, value= count, -province_state)
  
covid_all %>% 
  filter(country_region == 'France' | country_region == 'Italy' | country_region == 'US' | 
           country_region == 'Spain' | country_region == 'United Kingdom') %>% 
  select(country_region, confirmed) %>% 
  group_by(country_region) %>% 
  summarise_if(is.numeric, .funs = sum) %>% 
  mutate(country_region= fct_reorder(country_region, confirmed, .desc= TRUE)) %>% 
  ggplot(aes(x= country_region, y= confirmed, fill= country_region)) + geom_col() + geom_text(aes(label=confirmed), vjust= -1) +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'none') +
  scale_fill_manual(values = c('lightgreen', 'red', 'navy', 'darkblue', 'darkred'))
  # arrange(desc(confirmed))

