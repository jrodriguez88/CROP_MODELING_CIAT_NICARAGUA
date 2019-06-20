#### Script to eval nasa_power - INETER-Nicaragua 
### Rodriguez-Espinoza J
### 2019

library(naniar)
library(qmap)
library(tidyverse)
library(lubridate)
library(tictoc)

load("data_ineter_rain.RData")

load("data_ineter_tmax.RData")
data_ineter_tmax <- data_ineter_temp
rm(data_ineter_temp)

load("data_ineter_tmin.RData")
data_ineter_tmin <- data_ineter_temp
rm(data_ineter_temp)


data_ineter <- (data_ineter_tmax %>% rename(tmax = data)) %>%
    full_join((data_ineter_tmin %>% rename(tmin = data))) %>%
    left_join(data_ineter_rain) %>% rename( rain = data) %>%
    select(id, id_name, st_type, lat, lon, elev, tmax, tmin, rain)




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

data_nicaragua <- data_ineter %>% 
    mutate(nasa_power = map2(.x = lat, .y = lon,
                             ~get_data_nasapower(
                                 params = c("PRECTOT" ,
                                            "ALLSKY_SFC_SW_DWN", 
                                            "RH2M",
                                            "T2M_MAX",
                                            "T2M_MIN",
                                            "WS2M"),
                                 ini_date = 19850101,
                                 end_date = 20190228,
                                 lat = .x, lon = .y)))

save(data_nicaragua, file = "data_nicaragua_nasa.RData")


a <- data_nicaragua %>% slice(1) %>%
    mutate(nasa_power = map(nasa_power, ~.x %>% 
                                as_tibble %>% mutate(srad = ALLSKY_SFC_SW_DWN*3.6,
                                                 rain = PRECTOT, 
                                                 rhum = RH2M,
                                                 tmax = T2M_MAX,
                                                 tmin = T2M_MIN,
                                                 wvel = WS2M) %>%
                                select(date, tmax, tmin, rain, srad, rhum, wvel) %>%
                                replace_with_na_all(condition = ~.x < 0) %>%
                                replace_with_na_all(condition = ~.x == -999))) #slice(1) %>% pull(nasa_power)
    
mod <- fitQmapRQUANT(a$value, a$T2M_MIN)

doQmapRQUANT(a$T2M_MIN, mod)    

b <- a %>% 
    mutate(rain = map(rain, ~.x %>% rename(value = rain)),
           tmax_ = map2(.x = tmax, .y = nasa_power,
                        ~.x %>% filter(date > "1985-01-01", date <= "2009-12-31") %>%
                                left_join(.y %>% select(date, tmax), by = "date")),
           tmin_ = map2(.x = tmin, .y = nasa_power,
                        ~.x %>% filter(date > "1985-01-01", date <= "2009-12-31") %>%
                            left_join(.y %>% select(date, tmin), by = "date")),
           rain_ = map2(.x = rain, .y = nasa_power,
                        ~.x %>% filter(date > "1985-01-01", date <= "2009-12-31") %>%
                            left_join(.y %>% select(date, rain), by = "date")),
           mod_tmax = map(.x = tmax_, ~fitQmapRQUANT(.x$value, .x$tmax)),
           mod_tmin = map(.x = tmin_, ~fitQmapRQUANT(.x$value, .x$tmin)),
           mod_rain = map(.x = rain_, ~fitQmapRQUANT(.x$value, .x$rain))) %>%
    select(-c(tmax_, tmin_, rain_)) %>%
    mutate(tmax_np = map(.x = nasa_power, ~.x %>% select(date, tmax)),
           tmin_np = map(.x = nasa_power, ~.x %>% select(date, tmin)),
           rain_np = map(.x = nasa_power, ~.x %>% select(date, rain)),
           tmax_adj = map2(.x = tmax_np, .y = mod_tmax,  ~doQmapRQUANT(.x %>% pull(tmax), .y)),
           tmin_adj = map2(.x = tmin_np, .y = mod_tmin,  ~doQmapRQUANT(.x %>% pull(tmin), .y)),
           rain_adj = map2(.x = rain_np, .y = mod_rain,  ~doQmapRQUANT(.x %>% pull(rain), .y)))

 c <- b %>% 
    mutate(
        tmax_adj = map2(.x = tmax_np, .y = mod_tmax,  ~doQmapRQUANT(.x %>% pull(tmax), .y)),
        tmin_adj = map2(.x = tmin_np, .y = mod_tmin,  ~doQmapRQUANT(.x %>% pull(tmin), .y)),
        rain_adj = map2(.x = rain_np, .y = mod_rain,  ~doQmapRQUANT(.x %>% pull(rain), .y)))

 
 c %>% mutate(data_adj = )
     
   

data_nicaragua %>% slice(2) %>% pull(nasa_power)




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

a <- test_nasa %>% dplyr::select(date, T2M_MIN) %>% left_join(test_tmin) %>%
    mutate(diff=value-T2M_MIN,
           tmin_nasa_adj = T2M_MIN -3.25) %>% 
    ggplot(aes(date, doQmapRQUANT(a$T2M_MIN, mod))) + 
    geom_line() + 
    geom_line(aes(y=value), color="blue", alpha=0.4) + 
    theme_bw()


mod <- fitQmapRQUANT(a$value, a$T2M_MIN)

doQmapRQUANT(a$T2M_MIN, mod)


test_nasa %>% replace_with_na_all(condition = ~.x == -999) %>% write_csv("data_chin.csv")

test_nasa

data %>% mutate(srad = ALLSKY_SFC_SW_DWN*3.6,
                rain = PRECTOT, 
                rhum = RH2M,
                tmax = T2M_MAX,
                tmin = T2M_MIN,
                wvel = WS2M) %>%
    select(date, tmax, tmin, rain, srad, rhum, wvel) %>% filter(srad<1)
    replace_with_na_all(condition = ~.x == -999) %>% select(-date) %>%
    write.table("chinandega.txt", row.names = F, col.names = F, sep = "\t")


data %>% names

    

