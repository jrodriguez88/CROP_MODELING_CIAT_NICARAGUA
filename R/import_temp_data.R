#### Script to import climate data - INETER-Nicaragua 
### Rodriguez-Espinoza J
### 2019


## Load packages
library(tidyverse)
library(lubridate)
library(readxl)

path <- "X:/observed/weather_station/nic-ineter/daily-raw/_primary_files/Temperatura/TEMPERATURAS"
path_list <- list.files(path, full.names = T, pattern="Temperaturas") 

setwd("D:/2019/Temperaturas Minimas")

xls_files <-list.files(pattern = "xls")

read_excel_ineter <- function(file) {
    sheets <- readxl::excel_sheets(file) %>% .[.!="RESUMEN"]
    x <-    lapply(sheets, function(X) readxl::read_excel(file, range = "A1:N46", sheet = X, na = "-"))
    names(x) <- map(x, ~.x[12,1]) %>% 
        bind_rows() %>% flatten_chr() %>%
        str_extract("[0-9]+")
    x
}

safe_function <- possibly(read_excel_ineter, NULL)

raw_data <- map(xls_files, ~safe_function(.)) 

extract_temp_data <- function(data) {
    
    list_data <- data[names(data)!="NA"] %>% compact()
    
    ##Extract id. ## Extract id stations...it allow detect problems
    id_station <- list_data %>% 
        map(., ~.x[11,1]) %>% 
        bind_rows() %>% flatten_chr() %>%
        str_split_fixed(": ", 2)  %>%
        as_tibble() %>% select(V2) %>% 
        deframe() %>% str_replace_all(" ", "") 
    
    #Extract data, nest by id
    list_data %>%
        map(., ~.x %>% slice(15:45) %>% 
                select(-14) %>%
                setNames(c("day", 1:12)) %>%
                gather("month", "value", -day) %>%
                mutate(value = as.numeric(str_trim(value)))) %>%
        bind_rows(.id = "year") %>%
        mutate(date = make_date(year, month, day)) %>% 
        select(year, date, value) %>%
        filter(!is.na(date)) %>% 
        nest(-year) %>% 
        mutate(id_station = id_station) %>%
        select(id_station, data) %>% 
        nest(-id_station) %>%
        mutate(data = map(data, ~.x[["data"]]) %>% map(., ~bind_rows(.x)))
    
}

safe_function2 <- possibly(extract_temp_data, NULL)

data <- raw_data %>% map(., ~safe_function2(.))


#Error: 
#filepath: FORMATO Tem Max Absol MASATEPE 110407.xls
#libxls error: Unable to open file 

# some list have duplicate date 
#d[["data"]][[4]] %>% group_by(date) %>% summarise(n=n()) %>% arrange(desc(n))

data_ineter_temp <- data %>% bind_rows() %>% 
    filter(id_station!="") %>%
    unnest(data) %>% distinct %>% nest(-id_station) %>% 
    rename(id=id_station)


data_ineter_temp %>% left_join(data_ineter_rain %>% select(1:6))

save(data_ineter_temp, file = "data_ineter_tmin.RData")
#load("data_ineter_rain.RData")
#### Some station of temeprature have different "id". 
## 69129 Campos azules- Masatepe will change to 69049 MASATEPE
## 69132 Raul Gonzales- San Isidro will change to 69029 SAN ISIDRO DE BARBACOA

data_ineter_temp[2,1] <- 69049
data_ineter_temp[16,1] <- 69029






