### covid daily reports compiled
library(tidyverse)
library(lubridate)
library(gganimate)

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

# checking work... "NEW YORK" is wonky... 
covid_all %>% 
  filter(country_region== 'US') %>% 
  select(province_state, confirmed, recovered, deaths) %>% 
  group_by(province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  arrange(desc(confirmed))

unique(covid_all$date)

covid_all %>% 
  filter(province_state== 'New York' & date== '2020-03-24') %>% 
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
  geom_text(aes(label= confirmed, vjust= -1), size = 3) +
  labs(title= 'Confirmed COVID-19 Cases- Highest Pop. States', subtitle= 'March 2020-Current', y= 'Confirmed COVID-19 Cases', x= 'State') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
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

as.character(max(covid_us$deaths))
?annotate


### EDA / QA
covid_us %>% 
  filter(province_state== 'New York' & date > '2020-03-09') %>% 
  select(date, deaths) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, .funs= sum) %>% 
  select(deaths) %>% 
  max()

max(covid_us %>% 
  filter(date== '2020-03-27') %>% 
  select(deaths))

# deaths log_scale
plotly::ggplotly(
  covid_all %>% 
    filter(country_region== 'US' & date > '2020-03-15') %>% 
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
    annotate(geom= 'text', x=max(as.Date(covid_all$date)), y=385, 
             # annotate("text", x=floor_date(max(sa_dat1$variable), "month") - months(12)
             # label = paste0("April 2017\n", paste("$",round(max(sa_dat1$value)
             label= '385', hjust= -.5) +
    annotate(geom= 'text', x=as.Date('2020-03-26'), y=61, label= '61', vjust= -1) +
    annotate(geom= 'text', x=as.Date('2020-03-26'), y=81, label= '81', vjust= -1) +
    labs(title= 'COVID-19 Deaths- Highest Pop. States', 
         subtitle= 'March 2020-Current', y= 'COVID-19 Deaths', x= 'Week') +
    theme(panel.background = element_rect(fill = 'white'), 
          axis.text.x = element_text(angle = 90),
          legend.position = 'right') +
    scale_color_manual(values = c('blue', 'grey', 'grey', 'purple', 'red', 'grey', 'grey')) +
    scale_y_log10()
)


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
  scale_y_log10() +
  gganimate::transition_reveal(along= date)

# deaths by state
covid_us %>% 
  filter(country_region== 'US' & date > '2020-03-09') %>% 
  filter(province_state== 'New York' | province_state== 'Illinois' | 
           # province_state== 'Florida' | province_state== 'Texas' |
           province_state== 'Michigan') %>%
  select(date, province_state, confirmed, deaths) %>% 
  group_by(date, province_state) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(ln_confirmed= log(confirmed), ln_deaths= log(deaths)) %>%
  gather(key= metric, value= count, -date, -province_state) %>% 
  # arrange(desc(confirmed)) %>% 
  filter(metric== 'deaths') %>% 
  ggplot(aes(x= date, y= count, color= province_state)) + geom_line(size= 1) + 
  # scale_color_brewer(palette = 'Paired') +
  annotate(geom= 'text', x=as.Date('2020-03-30'), y=1550, label= '1550') +
  annotate(geom= 'text', x=as.Date('2020-03-30'), y= 259, label= '259', vjust= -1) +
  annotate(geom= 'text', x=as.Date('2020-03-31'), y= 150, label= '173', vjust= 1) +
  labs(title= 'COVID-19 Deaths- Highest Pop. States- Log Scale',
       subtitle= 'March 2020-Current', y= 'COVID-19 Deaths (log scale)', x= 'Week') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right', legend.title = element_blank()) +
  scale_color_manual(values = c('blue', 'gold', 'red')) +
  # scale_y_log10() +
  gganimate::transition_reveal(along= date)

# cases by county
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

covid_all %>% 
  filter(country_region== 'Japan')

# japan
covid_all %>% 
  filter(country_region == 'Japan' | country_region == 'Italy' | country_region == 'US' | 
           country_region == 'Spain' | country_region == 'United Kingdom') %>% 
  filter(date > '2020-03-01') %>% 
  select(country_region, date, deaths) %>% 
  group_by(country_region, date) %>% 
  summarise_if(is.numeric, .funs = sum) %>% 
  # mutate(ln_deaths= log(deaths)) %>%
  gather(key= metric, value= count, -date, -country_region) %>% 
  filter(metric== 'deaths') %>% 
  ggplot(aes(x= date, y= count, color= country_region)) + geom_line(size= 1) + 
  scale_color_brewer(palette = 'Paired') +
  # annotate(geom= 'text', x=as.Date('2020-03-29'), y=1550, label= '1550') +
  # annotate(geom= 'text', x=as.Date('2020-03-30'), y= 259, label= '259', vjust= -1) +
  # annotate(geom= 'text', x=as.Date('2020-03-31'), y= 150, label= '173', vjust= 1) +
  labs(title= 'COVID-19 Deaths- Countries- Log Scale',
       subtitle= 'March 2020-Current', y= 'COVID-19 Deaths (log scale)', x= 'Week') +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x = element_text(angle = 90),
        legend.position = 'right', legend.title = element_blank())
  # scale_color_manual(values = c('red', 'orange', 'grey', 'gold', 'navy', 'pink', 'green')) +


