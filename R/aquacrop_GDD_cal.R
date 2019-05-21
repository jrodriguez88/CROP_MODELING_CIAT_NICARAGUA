#### Aquacrop calculation GDD
# By https://github.com/jrodriguez88
# Author: Rodriguez-Espinoza J.
# 2019


## load packages and scripts
library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)

# Read data
path <- paste0(getwd(), "/data")
files <- list.files(path, pattern = ".txt")

names <- files %>% str_remove(".txt")

## Function to read import data , weather txt file    
read_weather_data <- function(path, file) {
    
    fread(paste0(path, "/", file), col.names = c("rain", "srad", "tmax", "tmin")) %>% as_tibble() %>%
        mutate(date = seq.Date(make_date(1998, 1, 1),
                               make_date(2018, 12, 31), "days")) %>%
        select(date, everything())

}

data <- map(files, ~read_weather_data(path = path, file = .x)) %>% set_names(names)

all_data <- data %>% bind_rows(.id = "id") %>% nest(-id) %>%
    separate(id, c("Loc", "Region"), sep="_") 



### plot all data
all_data %>% unnest(data) %>% 
    gather(var, value, -c(Loc, Region, date)) %>% filter(value < 100) %>%
    ggplot(aes(value)) +
    geom_histogram(color="gray", binwidth = 1) +
    facet_grid(Region+Loc ~var, scales = "free") +
    theme_classic()
    

    
    all_data %>% unnest(data) %>% select(rain) %>% ggplot(aes(rain)) +
        geom_histogram()
    
    all_data %>% unnest(data) %>% filter(rain>170)

#tidy_data <- data[[2]]  %>%
#    gather(var, value, -date)
#
#tidy_data %>% ggplot(aes(value)) +
#    geom_histogram(color="gray") + #geom_density(aes(y=0.5*..count..), color="red") 
#    facet_wrap(~var, scales = "free") +
#    theme_classic() +
#    labs(title = "")


# function to calculate HUH _ tbase, topt,and thigh depends of crop

HUH_cal <- function(tmax, tmin, tbase = 8, topt = 30, thigh = 42.5) {
    
    tav <- (tmin + tmax)/2
    
    h <- 1:24
    
    Td <- tav + (tmax - tmin)*cos(0.2618*(h - 14))/2 
    
    huh <- Td %>% enframe(name = NULL, "td") %>%
    mutate(HUH = case_when(td <= tbase | td >= thigh ~ 0,
                            td > tbase | td <= topt ~ (td - tbase)/24,
                            td > topt | td < thigh ~ (topt-(td - topt)*(topt - tbase)/(thigh - topt))/24))
    
    sum(huh$HUH)   
    
} 


#HUH_cal(29.6, 24.7)
# data list class

### Function to calculate growing degree days in historic 
cropfile <- paste0(path, "/GRDB15.CRO")
crop_duration <- 110
sowing_dates <- c(make_date(month = 5, day=15),
                  make_date(month = 6, day=30) + crop_duration)

get_param_gdd <- function(data , cropfile, sowing_dates) {
    
    gdd_data <- data %>% 
        mutate(GDD1 = ((tmax + tmin)/2) - 8,
               GDD2 = pmap(list(tmax=.$tmax, tmin = .$tmin), HUH_cal) %>% 
                   flatten_dbl())
    
    
    sow_dates <- gdd_data %>% filter(yday(date) >= yday(sowing_dates[1]),
                                     yday(date) <= yday(sowing_dates[2]))
    
    #summary(sow_dates)
    
    gdd_by <- median(sow_dates$GDD1)
    
    read_lines(cropfile) %>% str_subset("Calendar Days|(days)") %>% .[-c(1,2,10)] %>%
        str_split_fixed(":", 2) %>% str_trim() %>% matrix(ncol = 2) %>% 
        as_tibble() %>%
        mutate(V1 = as.numeric(V1)*gdd_by) %>% setNames(c("GDD", "grow_param")) %>%
        select(grow_param, GDD)
    
    
    
    
    
}

data_params <- all_data %>%
    mutate(gdd_param = map(data, ~get_param_gdd(.x, cropfile, sowing_dates)))


data_params %>% select(Loc, Region, gdd_param) %>%
    unnest(gdd_param) %>%
    group_by(Region, grow_param) %>%
    summarise(gdd_value = mean(GDD))



#gdd_data %>% select(date, GDD1, GDD2) %>% gather("GDD_method", "GDD", -date) %>% 
#    ggplot(aes(date, GDD, color= GDD_method, shape= GDD_method)) + 
#    geom_point(alpha = 0.7) +
#    scale_color_viridis_d() + 
#    theme_classic()




#gdd_data %>% select(date, GDD1, GDD2) %>% gather("GDD_method", "GDD", -date) %>%
#    t.test(GDD ~ GDD_method, data= .)


all_data %>% unnest(data) %>% select(-c(Region)) %>% group_by(Loc) %>%
    skim()
