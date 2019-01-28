#### Exploratory Data Analysis for RCM ICDF-data
##


## Load libraries and paths
library(tidyverse)
library(lubridate)
library(readxl)

loc_path <- "C:/Users/jrespinosa/OneDrive - CGIAR/Projects"


## Read data

raw_data <- read_excel(paste0(loc_path, "/ICDF/RAW_DATA/Prueba AVT.xlsx")) 
str(raw_data)



test <- raw_data%>% 
mutate(#    mutate(EMD= as.Date(`FECHA DE Germinacion`, origin= "1899-12-30"), 
           REGION=factor(REGION, c("I", "II", "III", "IV", "V", "VI", "RACCS", "RACCN")))
#Raw data requiere fix date formats


test %>% select(REGION, DEPARTAMENTO, Rendimiento) %>% na.omit() %>%
    ggplot(aes(x=`REGION`,y= Rendimiento, fill)) +
    geom_jitter(aes(color=DEPARTAMENTO), size=2) +
    stat_summary(fun.data = mean_sdl) +
    theme_bw() +
    ggsave(paste0("Rendimiento.png"), width=9, height=4.5)


