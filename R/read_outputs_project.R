## Read Aquacrop plugin outputs
# Simulacion Aquacrop ICDF
# By https://github.com/jrodriguez88 
# 2019

library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)

path <- "C:/ACsaV60/OUTP/"

setwd(path)

# season files
files <- list.files(path, pattern = "season")


read_aquacrop_season <- function(file, path){
    
    
    names <- read_lines(paste0(path, file))[[3]] %>%
                                    str_trim() %>%
                                    str_split(pattern = "[ ]+") %>%
                                    flatten_chr() %>% 
                                    c(., "File")
    
  data <- fread(paste0(path, file), skip = 4) %>%
        setNames(names)
  
  return(data)
    
}

read_aquacrop_day <- function(file, path){

    day_file <- read_lines(paste0(path, file))
    
    find_run <- day_file %>%
        str_detect("Run:") %>%
        which()+2
    
    nlines <- c(find_run[-1], length(day_file))-find_run-2
    
    arg_list <- list(file= file,
                     skip = find_run, 
                     nrows = nlines)
    
    # Read_pmap
    dat <- suppressWarnings(pmap(arg_list, fread))%>%
        bind_rows(.id = "run")

   return(dat)
    
}    

###Name variables 

name_var <- c("location", "pdate", "crop_sys")

#read_aquacrop_season(file, path)

data <- pmap(list(files, path), read_aquacrop_season) %>%
    bind_rows() %>% 
    mutate(File = str_replace(File, ".PRM", "")) %>%
    separate(File, name_var, sep = "_")

### Histogram summary
data %>% select(Yield, BioMass, Cycle, Rain) %>% gather("var", "value") %>% ggplot(aes(value)) +
    geom_histogram(bins = 10, color="grey") + facet_wrap(var ~., scales = "free") + 
    theme_classic()


data %>% group_by(location) %>%
    skim()


### plots

#data %>%
#    ggplot(aes(Year1, Yield, color = pdate)) + 
#    geom_line() +
#    facet_grid(location ~ pdate) +
#    theme_bw() +
#    theme(legend.position="bottom")

data$pdate <- factor(data$pdate, levels = c("March15","April15", "April30", "May15","May30", "June15", "June30"))

data %>% select(Year1, Day1, Month1, pdate, location, crop_sys, Yield, BioMass, Cycle, Rain) %>%
    mutate(date = make_date(Year1, Month1, Day1), 
           jdate = yday(date)) %>%
    gather("Sim_variable", "Value", -c(1:6, 11, 12)) %>%
    ggplot(aes(pdate,  Value, fill=location)) + 
#    scale_x_date(labels = function(x) format(x, "%B-%d")) +
    geom_boxplot(outlier.shape=NA) +
#    facet_wrap(riego ~. ) +
    theme_bw() +
    theme(legend.position="bottom", legend.title = element_blank()) +
    labs(title = "Guyana Rice Production") +
    facet_grid(Sim_variable~crop_sys, scales = "free") 
#    scale_fill_viridis_d()


mean_yield <- data %>% select(Year1, Day1, Month1, pdate, location, crop_sys, Yield, BioMass, Cycle) %>%
    mutate(date = make_date(Year1, Month1, Day1), 
           jdate = yday(date)) %>%
    gather("Sim_variable", "Value", -c(1:6, 10, 11)) %>% 
    filter(Sim_variable=="Yield") %>% group_by(crop_sys, location, Year1) %>% 
    summarise(Value=mean(Value)) %>%
    ungroup()

data %>% select(Year1, Day1, Month1, pdate, location, crop_sys, Yield, BioMass, Cycle) %>%
    mutate(date = make_date(Year1, Month1, Day1), 
           jdate = yday(date)) %>%
    gather("Sim_variable", "Value", -c(1:6, 10, 11)) %>% 
    filter(Sim_variable=="Yield") %>%
    ggplot(aes(Year1,  Value)) + 
    #    scale_x_date(labels = function(x) format(x, "%B-%d")) +
    geom_point() + geom_line(data=mean_yield, aes(x=Year1, y=Value), color = "red") + 
    
    #    facet_wrap(riego ~. ) +
    theme_bw() +
    theme(legend.position="bottom", legend.title = element_blank()) +
    labs(title = "Guyana Rice Production", 
         y="Yield (Tonnes/ha)",
         x = "Year") +
    facet_grid(location~crop_sys, scales = "free_y") 
