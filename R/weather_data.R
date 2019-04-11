#### Script to import climate data - INETER-Nicaragua 
### Rodriguez-Espinoza J
### 2019


#####
library(tidyverse)
library(XML)
library(xml2)
library(rvest)
library(sp)

### 
library(naniar)
#library(skimr)


## Test data precipitacion. Test diferent name format
#file <- "45005 Quilali 2005.htm"
#file <- "55043 (90).htm"
#file_lines <- read_lines("45005 Quilali 2005.htm")
#page <- read_html(file)

## `import_htm` Function to import htm files. 
# path argument: getwd() format
# file argument: htm format

import_htm <- function(path, file){
    
    # Read htm
    page <- read_html(paste0(path, "/", file))
    
    #get info 
    info <- html_nodes(page, xpath = "//td/b") %>% 
        html_text() %>%
        enframe(name = NULL) %>% 
        separate(value, c("var", "value"), sep = ": ") %>%
        spread(var, value)
    
    #Conditional. some files don't have data table 
    
    if(page %>% html_nodes("table") %>% length() < 2){
        values = tibble(date=NA, rain=NA)
    } else {
        values = suppressWarnings(
            page %>% 
                html_nodes("table") %>% .[2] %>% 
                html_nodes("td") %>%
                html_text() %>%
                matrix(ncol = 14, byrow = T) %>% .[1:31,-14] %>% 
                as_tibble() %>% 
                setNames(c("day", 1:12)) %>%
                gather("month", "rain", -day) %>% 
                mutate(rain = as.numeric(str_trim(rain)),
                       year= info[[1]], 
                       date = lubridate::make_date(year, month, day)) %>%
                filter(!is.na(date)) %>%
                select(date, rain))
    }
                    
    
    bind_cols(info, nest(values)) 
    
}

#import_htm(path_list[[51]], file)

# Folder path in dapadfs
path <- "X:/observed/weather_station/nic-ineter/daily-raw/_primary_files/Precipitacion"
path_list <- list.dirs(path, recursive = F) # Each represent a weather station

###  Some paths needs change/ Fonder into foldel
path_list[19] <- "X:/observed/weather_station/nic-ineter/daily-raw/_primary_files/Precipitacion/45050 condega rrr/45050 1983-2008"
path_list[42] <- "X:/observed/weather_station/nic-ineter/daily-raw/_primary_files/Precipitacion/55020 Jinotega 1952-2009/precipitacion"


# `get_ineter_data`  Function batch `import_htm` 
get_ineter_data <- function(path) {
    
    files_by_path <-  list.files(path, pattern = ".htm", full.names = F) %>% 
        enframe(name=NULL) %>%
        mutate(nc = nchar(value)) %>%
        filter(!(nc > mean(nc)+2), 
               !str_detect(value, "anual"), 
               !str_detect(value, "ANUAL")) %>%
        select(value) %>% deframe()
    
    pmap(list(path = path, file=files_by_path), import_htm) %>% 
        bind_rows()
    
}

# first step, detect problems by run on safely or possibly mode
safe_get_data <- possibly(get_ineter_data, otherwise = "something wrong here")

data_ineter_raw <- map(path_list, safe_get_data)

id <- str_extract_all(path_list, "Precipitacion/(.*?) ") %>% 
            str_extract_all("[0-9]+") %>% flatten_chr


################
################ Handle data ## goal: 
#  table_names <- c("id", "lat", "lon", "elev", "st_type", "data")


#data_ineter_raw[[1]] %>% select(2:8) %>% unique
# 
## raw_data filter , .[[58]] don't have data. 
raw_data <- data_ineter_raw %>% set_names(id) %>%
    discard(., ~length(.)==0)

# Some files with diferent info 
raw_data[["61009"]] %>% select(2:8) %>% View()
raw_data[["61009"]] <- raw_data[["61009"]] %>% 
    filter(!str_detect(`Estación`, "RAMA"))
 

## Extract general info 
info <- map(raw_data, ~.x %>% select(2:8) %>% unique %>% .[1,]) %>% 
    bind_rows(.id = "id") %>% 
    mutate(elev = as.numeric(str_extract(`Elevación`, "[0-9]+")),
           st_type = Tipo,
           id_name = str_extract(`Estación`, '(?<=- ).*(?= /)'),
           lat = str_replace(Latitud, "°", "d") %>% 
               char2dms() %>% 
               as.numeric(),
           lon = str_replace(Longitud, "°", "d") %>% 
               char2dms() %>% 
               as.numeric()) %>%
    select("id", "id_name", "lat", "lon", "elev", "st_type")
    
## Tidy data in tibble
data <- raw_data %>% map(., ~select(.x, data)) %>% 
    map(., ~bind_rows(.x[["data"]])) %>%
    bind_rows(.id = "id") %>%
    filter(!is.na(date)) %>% nest(-id)

###Get ineter info from webpage
ineter_web_raw <- read_html("https://servmet.ineter.gob.ni//Meteorologia/RedEstacionesNac/Catalogo.php") %>%
    html_nodes("table") %>% .[3] %>%
    html_table() %>% bind_rows() %>% 
    as_tibble()

name_iw <- ineter_web_raw %>% slice(2) %>% unlist(., use.names=FALSE)

ineter_cat <- ineter_web_raw %>%
    slice(3:19) %>% 
    mutate(X1 = as.numeric(X1)) %>%
    setNames(name_iw)
    
## getMetrics calculate Na percent, extract time of use (years)
info2 <- data %>%
         mutate(idate = map(data, ~ min(.x$date)) %>% do.call("c", .),
               fdate = map(data, ~ max(.x$date)) %>% do.call("c", .),
               years = time_length(fdate-idate, "years"),
               na_percent = map(data, ~ pct_miss(.x$rain)) %>% flatten_dbl()) %>%
         select(-data) %>% View()


data_f


