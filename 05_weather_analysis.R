##################################################
## Project: What can music tell us about people?
## Script Purpose: weather analysis versus valence values
## Date: 05/16/2019
## Author: Félix Carlos Camacho Criado
##################################################

library(ggplot2)
library(lubridate)
library(gdata)
library(readr)
library(dplyr)
library(plyr)
library(countrycode)

#First step is download  weather data. 
#FTP Source
# ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/ - NOAA's National Climatic Data Center (NCDC) 
# 

#info with all countries represented
weather_countries <- read.delim('data/weather/ghcnd-countries.txt', sep = "", header = FALSE, col.names = c("COUNTRY_ID","COUNTRY1", "COUNTRY2", "COUNTRY3"))  
#info with all Weather Station represented
weathers_stations  <- read.delim('data/weather/ghcnd-stations.txt', sep = "", header = FALSE)  

#info with data collected by the weather stations year by year
weather_2017 <- read.table('data/weather/2017.csv.gz', sep = ",", header = FALSE, col.names = c("ID", "DATE", "ELEMENT", "DATA_VALUE", "M-FLAG", "Q-FLAG", "S-FLAG", "OBS-TIME"))    
weather_2018 <- read.table('data/weather/2018.csv.gz', sep = ",", header = FALSE, col.names = c("ID", "DATE", "ELEMENT", "DATA_VALUE", "M-FLAG", "Q-FLAG", "S-FLAG", "OBS-TIME"))   
weather_2019 <- read.table('data/weather/2019.csv.gz', sep = ",", header = FALSE, col.names = c("ID", "DATE", "ELEMENT", "DATA_VALUE", "M-FLAG", "Q-FLAG", "S-FLAG", "OBS-TIME"))  

#info about variables name can be found into "ghcn-daily-by_year-format (1).rtf" file in the /data folder.

# #AVG precipitation instead of SUM of all precipitations by country will be taken. SUM is not a good measure since there are more 
# stations in some countries that in others, for example in Australia there are 1000 weather stations with data and only one in Belgium 

#cleaning dataset tasks

#merge data from three years
weather <- rbind(weather_2017,weather_2018,weather_2019)

#Just interested in precipitations so, a filter will be applied over the weather dataframe ("PRCP")

weather <- weather %>% 
  filter(ELEMENT == "PRCP") %>% 
  mutate(COUNTRY_ID =  substr(ID,1,2)) %>%
  mutate(YEAR = substr(DATE,1,4)) %>% 
  select(ID, COUNTRY_ID, DATE, YEAR ,ELEMENT, DATA_VALUE)

weather$DATE <- as.Date(as.character(weather$DATE),format="%Y%m%d")

#write_csv(weather, "weather.csv")

weather_countries <- weather_countries %>% 
  mutate(COUNTRY_DESC = trim(paste(COUNTRY1,COUNTRY2,COUNTRY3))) %>% 
  select(COUNTRY_ID, COUNTRY_DESC)

#merging info about contries and precipitations
weather_mx <-  left_join(weather,weather_countries, by = "COUNTRY_ID")

#This dataframe generate exactly what is required. An dataframe which shows the country, the month (represented by
# the first day of the month, and also the precipitation average per month)
weather_mx<- weather_mx %>% 
  group_by(COUNTRY_DESC, MONTH = floor_date(DATE, "month")) %>% 
  summarise(AVG_PRCP = mean(DATA_VALUE)) %>% 
  arrange(MONTH)

#Having this, now it's possible combine weather info with songs info.

#Putting the focus in the possible relationship between valence and precipitations

feature <- "avg_valence"
valence_2017 <- feature_by_month("2017",feature) %>% 
  dplyr::rename(
    COUNTRY_DESC = Zone)

valence_2018 <- feature_by_month("2018",feature) %>%
  dplyr::rename(
    COUNTRY_DESC = Zone)

valence_2019 <- feature_by_month("2019",feature) %>%
  dplyr::rename(
    COUNTRY_DESC = Zone)

valence_all <- rbind(valence_2017,valence_2018,valence_2019)

precip_valence <- left_join(valence_all, weather_mx) %>% 
  na.omit()

precip_valence$continent <- countrycode(sourcevar = precip_valence[, 1],
                                                        origin = "country.name",
                                                        destination = "continent")

#Calculating the correlations

corr_ <- precip_valence %>% 
  group_by(COUNTRY_DESC) %>% 
  summarise(correl = cor(mean, AVG_PRCP)) 

precip_valence_as <- precip_valence %>% 
  filter(continent == "Asia")
precip_valence_as <- left_join(precip_valence_as,corr_)

precip_valence_am <- precip_valence%>% 
  filter(continent == "Americas")
precip_valence_am <- left_join(precip_valence_am,corr_)

precip_valence_eu <- precip_valence %>% 
  filter(continent == "Europe")
precip_valence_eu <- left_join(precip_valence_eu,corr_)

precip_valence_oc <- precip_valence %>% 
  filter(continent == "Oceania")
precip_valence_oc <- left_join(precip_valence_oc,corr_)


#By checking this graph it can be seen the correlation month by month between the valence and the precipitations happened
#in the last two years and a half. The reason because it's been comparing month by month and not anually it's simple as
#doesn't rain the same in every month, specially in the places where they have seasonal rains,
#so if it had been taken an anual measure will be totally different. 

asia_rl <- ggplot(precip_valence_as, aes(x = MONTH)) +
     facet_wrap(paste("R=",round(correl,2)) ~ COUNTRY_DESC) +
     geom_line(aes(y = mean, colour = "Valence")) +
     geom_line(aes(y = AVG_PRCP/300, colour = "Precipitations")) +
     # annotate("text", x= as.Date("2017-05-01") , y=0.68, label= cor, size = 1) +
     scale_y_continuous(sec.axis = sec_axis(~.*300, name = "avg_prcp")) +
     scale_colour_manual(values = c("blue", "red")) +
     labs(y = "Valence",
              x = "Month",
              colour = "Variables") +
     theme(legend.position = c(0.8, 0.9)) +
     theme_bw()

asia_rl

# stat_cor(aes(x = MONTH, y = AVG_PRCP),method = "pearson", label.x = as.Date("2017-01-01"), label.y = 0.68, size = 5)

america_rl <- ggplot(precip_valence_am, aes(x = MONTH)) +
  facet_wrap(paste("R=",round(correl,2)) ~ COUNTRY_DESC) +
  geom_line(aes(y = mean, colour = "Valence")) +
  geom_line(aes(y = AVG_PRCP/300, colour = "Precipitations")) +
  # annotate("text", x= as.Date("2017-05-01") , y=0.68, label= cor, size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*300, name = "avg_prcp")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Valence",
       x = "Month",
       colour = "Variables") +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw()

america_rl

europe_rl <- ggplot(precip_valence_eu, aes(x = MONTH)) +
  facet_wrap(paste("R=",round(correl,2)) ~ COUNTRY_DESC) +
  geom_line(aes(y = mean, colour = "Valence")) +
  geom_line(aes(y = AVG_PRCP/300, colour = "Precipitations")) +
  # annotate("text", x= as.Date("2017-05-01") , y=0.68, label= cor, size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*300, name = "avg_prcp")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Valence",
       x = "Month",
       colour = "Variables") +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw()

europe_rl

oceania_rl <- ggplot(precip_valence_oc, aes(x = MONTH)) +
  facet_wrap(paste("R=",round(correl,2)) ~ COUNTRY_DESC) +
  geom_line(aes(y = mean, colour = "Valence")) +
  geom_line(aes(y = AVG_PRCP/300, colour = "Precipitations")) +
  # annotate("text", x= as.Date("2017-05-01") , y=0.68, label= cor, size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*300, name = "avg_prcp")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Valence",
       x = "Month",
       colour = "Variables") +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw()

oceania_rl




  