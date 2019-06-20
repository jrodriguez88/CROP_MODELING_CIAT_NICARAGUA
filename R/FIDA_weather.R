### data FIDA

library(tidyverse)
library(data.table)
library(skimr)
library(lubridate)

setwd("C:/Users/jrespinosa/OneDrive - CGIAR/Projects/ICDF/RAW_DATA/data_clima_fida/") 


files <- list.files(pattern = "Poso")

names <- read.table(files[1], sep=";", header=T, na.strings = "--") %>% names
p2018 <- read.table(files[1], sep=";", header=T, na.strings = "--")
p2019 <- read_csv(files[2], na = "--", col_names = T)


colnames(p2019) <- names

data <- bind_rows(p2018, p2019)

str(data)

names(data)

daily_data <- data %>% 
    select(1,4:8,17,18, 20, 23) %>% 
    setNames(c("datetime", "tmax", "tmin", "rhum", "dewp", "wvel", "bpress", "rain", "srad", "ET")) %>% 
    mutate(datetime = dmy_hm(datetime)) %>% 
    group_by(date= date(datetime)) %>%
    summarise(tmax = max(tmax, na.rm = T), 
              tmin = min(tmin, na.rm = T),
              rh_min = min(rhum, na.rm = T),
              rh_max = max(rhum, na.rm = T),
              dewp = mean(dewp, na.rm = T),
              wvel = mean(wvel, na.rm = T),
              pbar = mean(bpress, na.rm = T),
              rain = sum(rain, na.rm = T),
              rain_days = sum(rain>1, na.rm = T),
              sun_hour = sum(srad>5),
              srad = sum(srad, na.rm = T)*0.0864/24,
              ET = sum(ET, na.rm = T))

daily_data %>% select(data, tmax, tmin, rain, srad, wvel) %>%
    ggplot(aes(date, ))
    


