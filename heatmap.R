rm(list=ls())
pacman::p_load(tidyverse,lubridate,zoo,gganimate,transformr)
# Let's download the data directly instead of pulling their github repository for every update...
case_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
'%ni%' <- Negate('%in%')
# I owe this person a shrubbery: https://stackoverflow.com/posts/46867726/revisions
# Map data
county_map <- map_data("county")
state_map <- map_data("state")
# Pull and process data from Johns Hopkins
case_data <- read_csv(case_url) %>%
    pivot_longer(contains("/"),
                 values_to = "cases",
                 names_to = "date") %>%
    mutate(date = mdy(date),
           region = tolower(Province_State)) # add region column for work with map_data

death_data <- read_csv(death_url) %>%
    pivot_longer(contains("/"),
                 values_to = "deaths",
                 names_to = "date") %>%
    mutate(date = mdy(date),
           region = tolower(Province_State))

county_data <- 
    full_join(case_data,death_data) %>%
        filter(Province_State %ni%   # Drop some locations
                   c("Virgin Islands","Northern Mariana Islands",
                     "Guam","Grand Princess","Diamond Princess",
                     "American Samoa")) %>%
    group_by(FIPS) %>%
    arrange(FIPS, date) %>%
    mutate(cases_percap = cases / Population,
           deaths_percap = deaths / Population,
           new_cases = rollapply(cases,
                                  2,
                                  diff,
                                  na.pad = TRUE,
                                  align = 'right'),
           new_deaths = rollapply(deaths,
                                   2,
                                   diff,
                                   na.pad = TRUE,
                                   align = 'right'),
           
           new_cases_percap = new_cases / Population,
           new_deaths_percap = new_deaths / Population,
           new_cases_7day = rollmean(new_cases_percap,
                                     7,
                                     na.pad = TRUE,
                                     align = 'right'),
           new_deaths_7day = rollmean(new_deaths_percap,
                                     7,
                                     na.pad = TRUE,
                                     align = 'right'),
           normalized_cases = scale(new_cases_7day),
           normalized_deaths = scale(new_deaths_7day)) %>%
    ungroup

state_data <- county_data %>%
    group_by(date,region) %>%
    summarize(cases = sum(cases,na.rm=T),  # add up counties
              Population = sum(Population,na.rm=T),
              deaths = sum(deaths,na.rm=T)) %>% 
    ungroup %>% 
    arrange(region, date) %>%
    mutate(cases_percap = cases / Population,
           deaths_percap = deaths / Population,
           new_cases = rollapply(cases,
                                 2,
                                 diff,
                                 na.pad = TRUE, # this format is depricated. Fix later
                                 align = 'right'),
           new_deaths = rollapply(deaths,
                                  2,
                                  diff,
                                  na.pad = TRUE,
                                  align = 'right'),
           new_cases_percap = new_cases / Population,
           new_deaths_percap = new_deaths / Population,
           new_cases_7day = rollmean(new_cases,
                                     7,
                                     na.pad = TRUE,
                                     align = 'right'),
           new_deaths_7day = rollmean(new_deaths,
                                      7,
                                      na.pad = TRUE,
                                      align = 'right'),
           normalized_cases = scale(new_cases_7day),
           normalized_deaths = scale(new_deaths_7day)) %>%
    ungroup

### Make some maps
yesterday <- now() %>% as.Date() - days(1)
state_normalized_deaths_map <- 
state_data %>% filter(date == yesterday) %>%
    right_join(state_map) %>%
    ggplot(aes(long,lat,group = region)) +
    geom_polygon(aes(fill = normalized_deaths))
state_normalized_cases_map <- 
    state_data %>% filter(date == yesterday) %>%
    right_join(state_map) %>%
    ggplot(aes(long,lat,group = region)) +
    geom_polygon(aes(fill = normalized_cases))
# County maps
# county_data %>% filter(date == yesterday) %>%
#     right_join(county_map) %>%
#     arrange(order) %>%
#     ggplot(aes(long,lat,group = subregion)) +
#     geom_polygon(aes(fill = normalized_deaths))
# # Okay, something's wrong here. But I don't need to fix it now.


# plots
time_series_plot_cases <- state_data %>% 
    filter(date > '2020-03-01') %>%
    ggplot(aes(date,normalized_cases)) + 
    geom_line(aes(col=region)) + 
    theme(legend.position="none")

time_series_plot_deaths <- state_data %>% 
    filter(date > '2020-03-01') %>%
    ggplot(aes(date,normalized_deaths)) + 
    geom_line(aes(col=region)) + 
    theme(legend.position="none")

state_data %>% 
    filter(date > '2020-03-01') %>% ggplot(aes(date,region)) +
    geom_tile(aes(fill = normalized_deaths))

heat_map_plot1 <- state_data %>% 
    filter(date > '2020-03-01') %>% ggplot(aes(date,region)) +
    geom_tile(aes(fill=normalized_cases)) +
    theme(legend.position="none") 

#### Heatmap: cases
heatmap_cases <- state_data %>%
    filter(date > '2020-03-01') %>%
    select(date,region,normalized_cases) %>%
    mutate(normalized_cases = -normalized_cases) %>%
    filter(!is.na(normalized_cases)) %>%
    spread(date,normalized_cases) %>%
    select(-region) %>%
    as.matrix %>%
    heatmap(.,
            Colv = NA,
            labRow = unique(state_data$region),
            col = hcl.colors(12, palette = "RdYlBu"))

png("heatmap_cases.png",width = 2000, height = 2000)
state_data %>%
    filter(date > '2020-03-01') %>%
    select(date,region,normalized_cases) %>%
    mutate(normalized_cases = -normalized_cases) %>%
    filter(!is.na(normalized_cases)) %>%
    spread(date,normalized_cases) %>%
    select(-region) %>%
    as.matrix %>%
    heatmap(.,
            Colv = NA,
            labRow = unique(state_data$region),
            col = hcl.colors(12, palette = "RdYlBu"))
dev.off()

#### Heatmap: deaths
heatmap_deaths <-  state_data %>%
    filter(date > '2020-03-01') %>%
    select(date,region,normalized_deaths) %>%
    mutate(normalized_deaths = -normalized_deaths) %>%
    filter(!is.na(normalized_deaths)) %>%
    spread(date,normalized_deaths) %>%
    select(-region) %>%
    as.matrix %>%
    heatmap(.,
            Colv = NA,
            labRow = unique(state_data$region),
            col = hcl.colors(12, palette = "RdYlBu"))


png("heatmap_deaths.png",width = 2000, height = 2000)
state_data %>%
    filter(date > '2020-03-01') %>%
    select(date,region,normalized_deaths) %>%
    mutate(normalized_deaths = -normalized_deaths) %>%
    filter(!is.na(normalized_deaths)) %>%
    spread(date,normalized_deaths) %>%
    select(-region) %>%
    as.matrix %>%
    heatmap(.,
            Colv = NA,
            labRow = unique(state_data$region),
            col = hcl.colors(12, palette = "RdYlBu"))
dev.off()

### gganimate

# # animated_map <- 
#     state_data %>%
#     full_join(state_map) %>%
#     arrange(order) %>%
#     ggplot(aes(long,lat,group = region)) +
#     geom_polygon(aes(fill = normalized_cases)) +
#     transition_time(date) + ease_aes('linear')
#     
# anim_save("state_cases.gif")
#     
