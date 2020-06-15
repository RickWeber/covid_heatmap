library(tidyverse)
library(zoo)
data <- read_csv("time_series_covid19_confirmed_US.csv") %>%
pivot_longer(cols=contains("/"),names_to="date",values_to="cases") %>% 
mutate(date = as.Date(date, format ="%m/%d/%y")) %>%
rename(State = Province_State) %>%
group_by(State,date) %>% 
summarize(cases = sum(cases)) %>%
ungroup() %>%
group_by(State) %>%
arrange(State,date) %>% 
mutate(new_cases = rollapply(cases,2,diff,align='right',fill=NA)) %>%
mutate(moving_avg = rollapply(new_cases,7,mean,align='right',fill=NA)) %>%
mutate(normalized = scale(moving_avg))

# cluster the data
clustered <- data %>% ungroup %>%
select(date,State,normalized) %>%
filter(!is.na(normalized)) %>%
spread(date,normalized) %>%
as.matrix %>% dist %>% hclust %>% plot

# plots
time_series_plot <- data %>% ggplot(aes(date,moving_avg)) + 
 geom_line(aes(col=State)) + 
 theme(legend.position="none")

heat_map_plot <- data %>% ggplot(aes(date,State)) + 
 geom_tile(aes(fill=normalized)) + 
 theme(legend.position="none")
