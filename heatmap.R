library(tidyverse)
library(readxl)
library(zoo)
pop <- read_excel("nst-est2019-01.xlsx",skip = 3) %>%
    select(1,13)
colnames(pop) <- c("state","population")
pop <- pop %>%
    mutate(state = str_remove(state,"[.]")) %>%
    filter(!is.na(population))

'%ni%' <- Negate('%in%')
# I owe this person a shrubbery: https://stackoverflow.com/posts/46867726/revisions

data <- read_csv("time_series_covid19_confirmed_US.csv") %>% 
    pivot_longer(cols=contains("/"),names_to="date",values_to="cases") %>% 
    mutate(date = as.Date(date, format ="%m/%d/%y")) %>%
    rename(state = Province_State) %>%
    filter(state %ni% c("Virgin Islands","Northern Mariana Islands","Guam","Grand Princess","Diamond Princess","American Samoa")) %>%
    group_by(state,date) %>% 
    summarize(cases = sum(cases)) %>%
    ungroup() %>%
    group_by(state) %>%
    arrange(state,date) %>% 
    mutate(new_cases = rollapply(cases,2,diff,align='right',fill=NA)) %>%
    left_join(pop) %>%
    mutate(cases =  cases/population,
           new_cases = new_cases/population) %>%
    mutate(moving_avg = rollapply(new_cases,7,mean,align='right',fill=NA)) %>%
    mutate(moving_avg = moving_avg/population) %>%
    mutate(normalized = scale(moving_avg))

# cluster the data
clustered <- data %>%
    ungroup %>%
    select(date,state,normalized) %>%
    filter(!is.na(normalized)) %>%
    spread(date,normalized) %>%
    as.matrix %>% dist %>% hclust

# plots
time_series_plot <- data %>% ggplot(aes(date,moving_avg)) + 
    geom_line(aes(col=state)) + 
    theme(legend.position="none")

heat_map_plot1 <- data %>% ggplot(aes(date,state)) + 
    geom_tile(aes(fill=normalized)) + 
    theme(legend.position="none") +
    scale_fill_brewer(palette = "RdYlBu") # not working

## heat_map_plot2 <-
png("heatmap.png",width = 2000, height = 2000)
    data %>%
    ungroup %>%
    select(date,state,normalized) %>%
    filter(!is.na(normalized)) %>%
    spread(date,normalized) %>%
    select(-state) %>%
    as.matrix %>%
    heatmap(.,Colv = NA,labRow = unique(data$state))
dev.off()
