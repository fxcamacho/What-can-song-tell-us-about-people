 
##################################################
## Project: What can music tell us about people?
## Script Purpose: Loading function that will be used in the project files
## Date:
## Author: Félix Carlos Camacho Criado
##################################################


#install.packages("countrycode")
#install.packages("widgetframe")
#install.packages("plyr")

library(countrycode)
library(widgetframe)
library(plyr)


# This function pulls data from a certain date
pull_top200 <- function(zone,date_ini) {
  
  # Create the URL target
  urls <- str_c(url_prefix,zone,url_middle,format.Date(date_ini),"--",format.Date(date_ini+7),url_suffix)

  # Pull data from Spotify
  tryCatch( {
    top200_df <- read.csv(urls, skip = 1) 
  } ,  error = function(e) {
    top200_df <- data.frame("Position","Track.Name","Artist","Streams","URL")
    message("No data for this country/dates")
    print(e)})
  
  # There are sometimes file that do not exist, this handles the exception
  ifelse(all(colnames(top200_df) %in% col_names),
         top200_df <- top200_df %>% mutate(Date = date_ini, Zone = zone), # Create a date attribute
         top200_df <- data.frame()) # Return an empty data frame if invalid
  
  top200_df
}


#this function found de nº of NA values in each column of a df
findNAs <- function (df){
  for (i in 1:length(df)){
    a<-df %>% 
      filter(is.na(df[i])) %>% 
      count()
    cat(str_c("The column " ,colnames(df[i]), " has ", a, " NA values \n" ))
  }
}

#this function show the average values of each feature per year
feature_by_year_top <- function(anio, feature) {
  
    top200_df_tracks_vf %>% 
    select(Zone, year, feature) %>% 
    filter(year == anio) %>% 
    group_by(Zone,year) %>% 
    na.omit() -> countries_df
 
   evalString <- function(s) {
   eval(parse(text = s), parent.frame())
   }
  countries_df <- ddply(countries_df,'Zone',summarise, mean = mean(evalString(feature))) %>% 
  arrange(desc(mean))

  countries_zone <- as.character(toupper(countries_df$Zone)) 
  countries_df$Zone <- countrycode(countries_zone, "iso2c", "country.name", nomatch = "GLOBAL")
  countries_df$Zone[countries_df$Zone == "United States"] <- "USA"
  countries_df$Zone[countries_df$Zone == "United Kingdom"] <- "UK"
  countries_df$Zone[countries_df$Zone == "Czechia"] <- "Czech Republic"
  
  countries_df
}

#this function plot a interactive world map where the values of the selected featured are shown 
feature_map_interactive <- function(anio, feature) {
  
  map_countries <- feature_by_year_top(anio, feature)
  
  WorldData <- map_data('world')
  WorldData %>% filter(region != "Antarctica") -> WorldData
  
  #Total <- WorldData[WorldData$region %in% countries_df_2017$Zone, ]
  WorldData$value <- map_countries$mean[match(WorldData$region, map_countries$Zone)]
  
    g <- ggplot(WorldData) + 
     geom_polygon_interactive(
       color='white',
       aes(long, lat, group=group, fill=value,
           tooltip=sprintf("%s<br/>%s",region, round(value,digits = 3)))) +
    scale_fill_continuous(
                          low =  "yellow", 
                          high = "red4", 
                          guide= "colourbar") +
    # low = "thistle2", 
    # high = "blue", 
    # guide="barcolour") +
    theme_bw(base_size = 20)  + 
    labs(fill = feature ,title = paste("World Map by", toupper(feature)), 
         subtitle = paste("Year of analysis:",anio), x="", y="") +
    scale_y_continuous(breaks=c()) + 
    scale_x_continuous(breaks=c()) + 
    theme(panel.border =  element_blank())
   
    widgetframe::frameWidget(ggiraph(code=print(g), width = 1 ,width_svg = 20, height_svg = 10))
  #No 2017 data for India/Israel/South Africa and Vietnam
}


#used for hapiness analysis
song_by_year <- function(anio, feature) {
  
  top200_df_tracks_vf %>% 
    select(artist_name, track_name,Zone, year, feature) %>% 
    filter(year == anio) %>% 
    group_by(Zone,year) %>% 
    na.omit() -> countries_df
  
  # evalString <- function(s) {
  #   eval(parse(text = s), parent.frame())
  # }
  # 
  # countries_df <- ddply(countries_df,'Zone',summarise, mean = mean(evalString(feature))) %>% 
  #   arrange(desc(mean))
  
  countries_zone <- as.character(toupper(countries_df$Zone)) 
  countries_df$Zone <- countrycode(countries_zone, "iso2c", "country.name", nomatch = "GLOBAL")
  countries_df$Zone[countries_df$Zone == "United States"] <- "USA"
  countries_df$Zone[countries_df$Zone == "United Kingdom"] <- "UK"
  countries_df$Zone[countries_df$Zone == "Czechia"] <- "Czech Republic"
  
  countries_df 
}


feature_by_month <- function(anio, feature) {
  
    top200_df_tracks_vf %>% 
    select(Zone, year, date,feature) %>% 
    filter(year == anio) %>% 
    mutate(date = replace(date, which(date == "2016-12-30"),"2017-01-01")) %>% 
    group_by(Zone, MONTH = floor_date(date, "month")) %>% 
    na.omit() -> countries_df
  
  evalString <- function(s) {
    eval(parse(text = s), parent.frame())
  }

  countries_df <- ddply(countries_df,.(Zone, MONTH),summarise, mean = mean(evalString(feature))) %>% 
    arrange(desc(mean))
  
  countries_zone <- as.character(toupper(countries_df$Zone)) 
  countries_df$Zone <- countrycode(countries_zone, "iso2c", "country.name", nomatch = "GLOBAL")
  countries_df$Zone[countries_df$Zone == "United States"] <- "USA"
  countries_df$Zone[countries_df$Zone == "United Kingdom"] <- "UK"
  countries_df$Zone[countries_df$Zone == "Czechia"] <- "Czech Republic"
  
  countries_df
}


most200popularsongs_by_year <- function(anio) {
  
df_global <- top200_df_tracks_vf %>% 
  filter(Zone != "global" & year == anio) %>% 
  group_by(track_name, artist_name) %>% 
  na.omit() %>% 
  summarise_at(vars(starts_with("streams")), list(sum_streams = sum), na.rm =TRUE) %>% 
  arrange(desc(sum_streams)) %>% 
  unique()

df_features <- top200_df_tracks_vf %>% 
  filter(Zone != "global" & year == anio) %>% 
  na.omit() %>% 
  select(track_name, artist_name,avg_danceability,avg_energy,avg_loudness,avg_speechiness,avg_acousticness,avg_instrumentalness
         ,avg_liveness,avg_valence,avg_tempo,avg_duration_ms) %>% 
  unique()

df_songs_all <- left_join(df_global, df_features)

df_songs_all["popularity"] <- index(df_songs_all)
df_songs_all["target"] <- df_songs_all["popularity"]<200
df_songs_all[,15] <- lapply(df_songs_all[,15], as.numeric)
df_songs_all
}

featureboxplots_by_year <- function(anio) {

df_songs_all <- most200popularsongs_by_year(anio)
  
df_songs_all$target <- as.factor(df_songs_all$target)

box_danceability <- qplot(target,avg_danceability,data=df_songs_all,geom="boxplot")
box_energy <-qplot(target,avg_energy,data=df_songs_all,geom="boxplot")
box_loudness <-qplot(target,avg_loudness,data=df_songs_all,geom="boxplot")
box_speechiness <-qplot(target,avg_speechiness,data=df_songs_all,geom="boxplot")
box_acousticness <-qplot(target,avg_acousticness,data=df_songs_all,geom="boxplot")
box_instrumentalness <-qplot(target,avg_instrumentalness,data=df_songs_all,geom="boxplot")
box_liveness <-qplot(target,avg_liveness,data=df_songs_all,geom="boxplot")
box_valence <-qplot(target,avg_valence,data=df_songs_all,geom="boxplot")
box_tempo <-qplot(target,avg_tempo,data=df_songs_all,geom="boxplot")
box_duration_ms <-qplot(target,avg_duration_ms,data=df_songs_all,geom="boxplot")

grid.arrange(box_danceability, box_energy,box_loudness, box_acousticness, box_speechiness, box_instrumentalness,
             box_valence, box_valence, box_duration_ms, box_tempo,  nrow = 2)
}