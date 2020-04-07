### covid daily reports compiled
library(tidyverse)
library(lubridate)
library(gganimate)

### daily report compilation


# mapping on personal comp
# file_list<- list.files('/Users/BenDreyfuss/drepo/COVID-20/csse_covid_19_data/csse_covid_19_daily_reports/', pattern = '*.csv')
# file_list
# map_list<- map(.x= ' ', paste0, 
#                '/Users/BenDreyfuss/drepo/COVID-20/csse_covid_19_data/csse_covid_19_daily_reports/', file_list, 
#                collapse = "")  %>% 
#   str_split(pattern = ' ')
# map_list


# mapping on work comp
file_list<- list.files('/Users/ben.dreyfuss/Desktop/repos/COVID-20/csse_covid_19_data/csse_covid_19_daily_reports/', pattern = '*.csv')
file_list
map_list<- map(.x= ' ', paste0, 
               '/Users/ben.dreyfuss/Desktop/repos/COVID-20/csse_covid_19_data/csse_covid_19_daily_reports/', file_list, 
               collapse = "")  %>% 
  str_split(pattern = ' ')
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

# col chart showing total volume of confirmed by top 10 populous states
covid_all %>% 
  filter(country_region== 'US') %>% 
  filter(province_state== 'California' | province_state== 'Illinois' |
           province_state== 'New York' | province_state== 'Florida' | province_state== 'Texas' |
           province_state== 'Pennsylvania' | province_state== 'Ohio' | province_state== 'Georgia' |
           province_state== 'North Carolina' | province_state== 'Michigan') %>%
  select(date, province_state, confirmed, deaths) %>% 
  group_by(province_state) %>% 
  summarise_if(is.numeric, max) %>% 
  # arrange(desc(confirmed)) %>% 
  mutate(province_state= fct_reorder(.f= province_state, .x= confirmed, .desc= TRUE)) %>% 
  ggplot(aes(x= province_state, y= confirmed, fill= 'red')) + geom_col() +
  geom_text(aes(label= confirmed, vjust= -1), size = 3) +
  labs(title= 'Confirmed COVID-19 Cases- Highest Pop. States', subtitle= 'March 2020-Current', y= 'Confirmed COVID-19 Cases', x= 'State') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')

# confirmed cases top six most populous states over time
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
  filter(metric== 'confirmed') %>% 
  ggplot(aes(x= date, y= count, color= province_state)) + geom_line() + 
  annotate(geom= 'line', x= as.Date('2020-03-24'), y= 25681, color= 'white', size= 0) +
  annotate(geom= 'text', x=as.Date('2020-03-24'), y=25681, label= '25,681', hjust= 1.2) + 
  labs(title= 'Confirmed COVID-19 Cases- Highest Pop. States', 
       subtitle= 'March 2020-Current', y= 'Confirmed COVID-19 Cases', x= 'Week') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right') +
  scale_color_manual(values = c('blue', 'grey', 'grey', '#ff0000', 'purple', 'grey'))

# deaths (US dataframe)
plotly::ggplotly(
covid_us %>% 
  filter(country_region== 'US' & date > '2020-03-09') %>% 
  filter(province_state== 'California' | province_state== 'Illinois' |
           province_state== 'New York' | province_state== 'Florida' | province_state== 'Texas' |
           province_state== 'Pennsylvania' | province_state== 'Michigan') %>%
  select(date, province_state, confirmed, deaths) %>% 
  group_by(date, province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(ln_confirmed= log(confirmed), ln_deaths= log(deaths)) %>%
  gather(key= metric, value= count, -date, -province_state) %>% 
  # arrange(desc(confirmed)) %>% 
  filter(metric== 'deaths') %>% 
  ggplot(aes(x= date, y= count, color= province_state)) + geom_line() + 
  annotate(geom= 'text', x=max(as.Date(covid_us$date)), y= max(covid_us$deaths), label= as.character(max(covid_us$deaths)), vjust= -2) +
  # annotate(geom= 'text', x=as.Date('2020-03-25'), y=50, label= '65', vjust= -1) +
  labs(title= 'COVID-19 Deaths- Highest Pop. States', 
       subtitle= 'March 2020-Current', y= 'COVID-19 Deaths', x= 'Week') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right') +
  scale_color_manual(values = c('blue', 'grey', 'grey', 'purple', 'red', 'grey', 'grey'))
)


### EDA / QA
covid_us %>% 
  filter(province_state== 'New York' & date > '2020-03-09') %>% 
  select(date, deaths) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, .funs= sum) %>% 
  select(deaths) %>% 
  max()


# deaths logged by state
covid_all %>% 
  filter(country_region== 'US' & date > '2020-03-09') %>% 
  filter(province_state== 'California' | province_state== 'Illinois' |
           province_state== 'New York' | province_state== 'Florida' | province_state== 'Texas' |
           province_state== 'Pennsylvania' | province_state== 'Michigan') %>%
  select(date, province_state, confirmed, deaths) %>% 
  group_by(date, province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(ln_confirmed= log(confirmed), ln_deaths= log(deaths)) %>%
  gather(key= metric, value= count, -date, -province_state) %>% 
  # arrange(desc(confirmed)) %>% 
  filter(metric== 'deaths') %>% 
  ggplot(aes(x= date, y= count, color= province_state)) + geom_line(size= 1) + 
  scale_color_brewer(palette = 'Paired') +
  annotate(geom= 'text', x=as.Date('2020-03-29'), y=1550, label= '1550') +
  annotate(geom= 'text', x=as.Date('2020-03-30'), y= 259, label= '259', vjust= -1) +
  annotate(geom= 'text', x=as.Date('2020-03-31'), y= 150, label= '173', vjust= 1) +
  labs(title= 'COVID-19 Deaths- Highest Pop. States- Log Scale',
       subtitle= 'March 2020-Current', y= 'COVID-19 Deaths (log scale)', x= 'Week') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right', legend.title = element_blank()) +
  # scale_color_manual(values = c('red', 'orange', 'grey', 'gold', 'navy', 'pink', 'green')) +
  scale_y_log10()

# time aligning states (fix annotation!)
covid_us %>% 
  filter(country_region== "US") %>% 
  select(date, province_state, deaths) %>% 
  filter(deaths> 10 & province_state== 'New York' |
           deaths> 10 & province_state == 'California' |
           deaths> 10 & province_state == 'Texas'|
           deaths> 10 & province_state == 'Michigan'|
           deaths> 10 & province_state == 'Pennsylvania'|
           deaths> 10 & province_state == 'Florida' |
           deaths> 10 & province_state == 'Illinois' 
  ) %>%
  group_by(province_state, date) %>% 
  summarise_at(.vars= vars(deaths), max) %>% 
  # ungroup() %>% 
  arrange(province_state, date) %>% 
  mutate(day = row_number()) %>%
  group_by(date, day, province_state) %>% 
  summarise_at(.vars= vars(deaths), max) %>% 
  ggplot(aes(x= day, y= deaths, color= province_state)) + geom_line(size= 1) +
  # annotate(geom= 'text', x= max(covid_us$day), y=max(covid_us$deaths), label= 'max') +
  labs(title= 'COVID-19 Deaths by State- Log Scale',
       subtitle= 'March 2020-Current', y= 'COVID-19 Deaths (log scale)', x= 'Days Since 10th Death') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right', legend.title = element_blank()) +
  scale_y_log10()

# time aligning all countries to a single first day

covid_all %>% 
  select(date, country_region, deaths) %>% 
  filter(deaths> 10 & country_region== 'France' |
           deaths> 10 & country_region == 'US' |
           deaths> 10 & country_region == 'Italy'|
           deaths> 10 & country_region == 'Japan'|
           deaths> 10 & country_region == 'South Korea'|
           deaths> 10 & country_region == 'Spain' |
           deaths> 10 & country_region == 'Germany' 
  ) %>%
  group_by(country_region, date) %>% 
  summarise_at(.vars= vars(deaths), max) %>% 
  # ungroup() %>% 
  arrange(country_region, date) %>% 
  mutate(day = row_number()) %>%
  group_by(date, day, country_region) %>% 
  summarise_at(.vars= vars(deaths), max) %>% 
  ggplot(aes(x= day, y= deaths, color= country_region)) + geom_line(size= 1) +
  labs(title= 'COVID-19 Deaths by Country- Log Scale',
       subtitle= 'March 2020-Current', y= 'COVID-19 Deaths (log scale)', x= 'Day Number') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right', legend.title = element_blank()) +
  scale_y_log10() 

