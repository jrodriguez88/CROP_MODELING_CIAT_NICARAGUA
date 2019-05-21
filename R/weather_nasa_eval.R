#### Script to eval nasa_power - INETER-Nicaragua 
### Rodriguez-Espinoza J
### 2019

library(naniar)
library(qmap)
library(tidyverse)

load("data_ineter_rain.RData")

load("data_ineter_tmax.RData")
data_ineter_tmax <- data_ineter_temp
rm(data_ineter_temp)

load("data_ineter_tmin.RData")
data_ineter_tmin <- data_ineter_temp
rm(data_ineter_temp)


tmax <- data_ineter_tmax %>% left_join(data_ineter_rain %>% select(1:6)) %>%
    mutate(idate = map(data, ~ min(.x$date)) %>% do.call("c", .),
           fdate = map(data, ~ max(.x$date)) %>% do.call("c", .),
           years = time_length(fdate-idate, "years"),
           na_percent = map(data, ~ pct_miss(.x$value)) %>% flatten_dbl())

tmin <- data_ineter_tmin %>% left_join(data_ineter_rain %>% select(1:6)) %>%
    mutate(idate = map(data, ~ min(.x$date)) %>% do.call("c", .),
           fdate = map(data, ~ max(.x$date)) %>% do.call("c", .),
           years = time_length(fdate-idate, "years"),
           na_percent = map(data, ~ pct_miss(.x$value)) %>% flatten_dbl())

format(tmax$fdate[[1]], "%Y%m%d")

### get NASA-Power
params <- c("PRECTOT" , 
            "ALLSKY_SFC_SW_DWN", 
            "RH2M",
            "T2M_MAX",
            "T2M_MIN",
            "WS2M")
ini_date <- 19900101
end_date <- 20101231
lat <- 12.6
lon <- -87.1

test_tmax <- tmax[2,2][[1]][[1]]
test_tmin <- tmin[10,2][[1]][[1]]
test_nasa <- get_data_nasapower(params, ini_date, end_date, lat, lon)

###TMAX
test_nasa %>% select(date, T2M_MAX) %>% left_join(test_tmax) %>% 
    ggplot(aes(value, T2M_MAX)) + geom_point() + geom_smooth(method = "lm")

test_nasa %>% select(date, T2M_MAX) %>% left_join(test_tmax) %>% 
    ggplot(aes(date, T2M_MAX)) + 
    geom_line() + 
    geom_line(aes(y=value), color="blue", alpha=0.4) + 
    theme_bw()

### TMIN
test_nasa %>% select(date, T2M_MIN) %>% left_join(test_tmin) %>% 
    ggplot(aes(value, T2M_MIN)) + geom_point() + geom_smooth(method = "lm")

test_nasa %>% dplyr::select(date, T2M_MIN) %>% left_join(test_tmin) %>% 
    ggplot(aes(date, T2M_MIN)) + 
    geom_line() + 
    geom_line(aes(y=value), color="blue", alpha=0.4) + 
    theme_bw()

test_nasa %>% dplyr::select(date, T2M_MIN) %>% left_join(test_tmin) %>%
    mutate(diff=value-T2M_MIN,
           tmin_nasa_adj = T2M_MIN -3.25) %>% 
    ggplot(aes(date, doQmapRQUANT(a$T2M_MIN, mod))) + 
    geom_line() + 
    geom_line(aes(y=value), color="blue", alpha=0.4) + 
    theme_bw()


mod <- fitQmapRQUANT(a$value, a$T2M_MIN)

b <- doQmapRQUANT(a$T2M_MIN, mod)



