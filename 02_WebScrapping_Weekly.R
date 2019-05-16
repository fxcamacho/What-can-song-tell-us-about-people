
##################################################
## Project: What can music tell us about people?
## Script Purpose: WebScrapping about Spotify songs data
## Date: 05/16/2019
## Author: Félix Carlos Camacho Criado
##################################################

# In this project it has been decided to download the data in a weekly range because it will be easier to differenciate 
# the seasons every year and also for future analysis.

# install.packages("tidyverse")
# install.packages("lubridate")

library(tidyverse)
library(lubridate)
library(readr)
library(plyr)


# If you launch the following url in your browser, an new download of .csv file will start. 
#Example: https://spotifycharts.com/regional/tr/weekly/2019-04-05--2019-04-12

#some previous dataframes and variables need to be defined:

# First we have to compose each part of the url
url_prefix <- 'https://spotifycharts.com/regional/'
url_middle <- '/weekly/'
url_suffix <- '/download'

# Get start and end date for 2017
start_date_2017 <- as.Date("2016/12/30")
end_date_2017 <- as.Date("2017/12/29")

# Get start and end date for 2018
start_date_2018 <- as.Date("2017/12/29")
end_date_2018 <- as.Date("2018/12/28")

# Get start and end date for 2019
start_date_2019 <- as.Date("2018/12/28")
end_date_2019 <- as.Date("2019/04/05")

# "Zones" dataframe is created which includes info related to the countries of which you will obtain TOP 200 songs.
id_spotify <- c("global", "us", "gb", "ar", "at", "au", 
                "be", "bg", "bo", "br", "ca", "ch", "cl", "co",
                "cr", "cz", "de", "dk", "do", "ec", "ee", "es", 
                "fi", "fr", "gr", "gt", "hk", "hn", "hu", "id",
                "ie", "il","in", "is", "it", "jp", "uy", "lt", 
                "lu", "lv", "mt", "mx", "my", "ni", "nl", "no",
                "nz", "pa", "pe", "pl", "pt", "py", "ro", "se", "sg", "sk", 
                "sv", "th", "tr", "tw", "vn", "za")

descr <-    c("Global", "UnitedStates", "UnitedKingdom", "Argentina", 
              "Austria", "Australia", "Belgium", "Bulgaria", "Bolivia", "Brazil",
              "Canada", "Switzerland", "Chile", "Colombia",
              "Costa Rica", "CzechRepublic", "Deustchland", "Denmark",
              "DominicanRepublic", "Ecuador", "Estonia", "Spain", 
              "Finland", "France", "Greece", "Guatemala", "Hong Kong", "Honduras",
              "Hungary", "Indonesia","Ireland", "Israel","India", "Iceland", "Italy",
              "Japan", "Uruguay", "Lithuania","Luxembourg", "Latvia", "Malta", 
              "Mexico", "Malasya", "Nicaragua", "Netherlands", "Norway", "NewZealand",
              "Panama", "Peru", "Poland", "Portugal", "Paraguay", "Romania",
              "Sweden", "Singapur", "Slovakia","ElSalvador", "Thailand", "Turkey",
              "Taiwan", "Vietnam", "South Africa")

zones <- data.frame(id_spotify,descr)

# List of Column Names that should exist in the dataframe
col_names = c("Position","Track.Name","Artist","Streams","URL")

# Iterate every date from start to end

#As you can see this is a long process, once the data are downloaded the best way to work is saving the recently downloaded
#data in some .csv files. I highly recommend to upload the file already downloaded that you can find in the /data folder in the 
#project.

# And get 2017 top 200 songs by country. If there is a file that it does not exists, the process will skip to the 
#next week

    date <- start_date_2017 # set data counter
    top200_df <- data.frame("Position","Track.Name","Artist","Streams","URL")
    top200_df_2017<- data.frame() # initialize empty df
    i = 1
    for (i in 1:length(zones$id_spotify)) {
      while (date <= end_date_2017) {
        print(str_c("Processing ", date," ", zones$descr[i]))
        # Concat the current data frame with new data
        top200_df_2017 <- top200_df_2017 %>% rbind(pull_top200(zones$id_spotify[i],date))
      date <- date + 7
      }
    i <- i+1
    date <- start_date_2017
    }

# And get 2018 top 200 songs by country
    date <- start_date_2018 # set data counter
    top200_df_2018 <- data.frame() # initialize empty df
    i <- 1
    for (i in 1:length(zones$id_spotify)) {
      while (date <= end_date_2018) {
        print(str_c("Processing ", date," ", zones$descr[i]))
        # Concat the current data frame with new data
        top200_df_2018 <- top200_df_2018 %>% rbind(pull_top200(zones$id_spotify[i],date))
        date <- date + 7
      }
      i <- i+1
      date <- start_date_2018
    }
    
# And get 2019 top 200 songs by country. No data from January to March in India.
    date <- start_date_2019 # set data counter
    top200_df_2019 <- data.frame() # initialize empty df
    i <- 1
    for (i in 1:length(zones$id_spotify)) {
      while (date <= end_date_2019) {
        print(str_c("Processing ", date," ", zones$descr[i]))
        # Concat the current data frame with new data
        top200_df_2019 <- top200_df_2019 %>% rbind(pull_top200(zones$id_spotify[i],date))
        date <- date + 7
      }
      i <- i+1
      date <- start_date_2019
    }
    
#So, as you can see this is a long process, once the data are downloaded the best way to work is saving the recently downloaded
#data in some .csv files. 
    
# top200_df_2017 %>% 
#       write_csv("top200songs_2017.csv")
# top200_df_2018 %>% 
#       write_csv("top200songs_2018.csv")
# top200_df_2019 %>% 
#       write_csv("top200songs_2019.csv")


#this is how you can upload the recently downloaded datasets into three dataframes.

top200_df_2017 <- read_csv("data/top200songs_2017.csv")
top200_df_2018 <- read_csv("data/top200songs_2018.csv")
top200_df_2019 <- read_csv("data/top200songs_2019.csv")

#the columns are renamed in order to facilitate the comprehension and avoiding some characters like "."

top200_df_2017 <- top200_df_2017 %>%
  dplyr::rename(position = Position,
         track_name = Track.Name,
         artist_name = Artist,
         streams = Streams,
         url = URL,
         date = Date)

top200_df_2018 <- top200_df_2018 %>%
  dplyr::rename(position = Position,
         track_name = Track.Name,
         artist_name = Artist,
         streams = Streams,
         url = URL,
         date = Date)

top200_df_2019 <- top200_df_2019 %>%
  dplyr::rename(position = Position,
         track_name = Track.Name,
         artist_name = Artist,
         streams = Streams,
         url = URL,
         date = Date)

#three dataframes union 
top200_df_all <- rbind(top200_df_2017,top200_df_2018,top200_df_2019)

 # top200_df_all %>% 
 #   write_csv("top200.csv")

#If you consider that all days/week are informed every country should have the same number of observations (TOP 200 per week, and every
#complete year has 52 weeks), therefore we should found a dataframe composed by (53*200 + 53*200 + 14*200)* = 23800
# *(52 weeks from 2017, 52 from 2018 and 15 from 2019)

#let check the data!

top200_df_all %>% 
  group_by(Zone) %>% 
  summarise(numero = n()) %>% 
  filter(numero < 23800)

#Checking the data it can be seen that some countries like India, Japan or Thailand don't have all their weeks informed. This can be
#corroborated in Spotify Charts website. So, il, in, jp, ro, th, vn and za have less data from spotify API than the rest 

#Once you have the info about track_names, artists, number of streams and zone which can be found in Spotify Charts, it's time to
#pull metadata from the songs using Spotifyr library and their functions.

#Then, all artist in df will be extracted and using get_artist_audio_features() you could extract every track info including the 
#features of every track of each artist. 
  
artists <- top200_df_all$artist_name%>% unique()
artists <- as.character(artists)
tracks <- data.frame()

#IMPORTANT get_artist_audio_features() from Spotifyr library does not always work correctly and you will find some
#difficult pulling the metadata songs. This is a recognized problem in the library, so the best way to avoid the metadata loss of each track is
#to create a automated process that could pull the data and if the function doesn't work ok, the artist from whom you have not been able to extract
#the data, will be included in the remaining_list so the proccess will try it again in the next try of the loop.

#As it is a long process I highly recommend to upload the file called "tracks.csv" which is the final output of this automated process.
#You can find in the /data folder in the project.

max_tries = 5

# Pull song metadata
for (try in 1:max_tries) {
  remaining_artists <- c()

  for (i in 1:length(artists)) {
    print(str_c("Processing Artist: ", artists[i], " ", i, " of ", length(artists), " pass: ", try))
    succeed = FALSE

    tryCatch({
      # Select and join the artist's tracks with top 200
      tracks <- get_artist_audio_features(artists[i]) %>% # Get the artist's albums
        #mutate(artist_name = artists[i]) %>% # Add the artist's name
        filter(track_name %in% top200_df_all$track_name) %>% # Filter only tracks that are in top200
        rbind(tracks)
      succeed = TRUE
      print("Done!")
    }, error = function(e) { print(e) })

    # If the pull did not succeed, add the artist to the remaining artist list
    if (!succeed) {
      remaining_artists <- c(remaining_artists, artists[i])
    }
  }
  artists <- remaining_artists
}

# Save data
# write_csv(tracks,"tracks.csv")

#tidy tracks in order to obtain the audio features and eliminate duplicated and NA songs. For those songs which
#appears more than 1 time will be used the average of their audio features.

tracks_cleaning_step <- tracks %>% 
  select(track_name, artist_name, danceability, energy, 
         loudness, speechiness,acousticness,instrumentalness, liveness, valence, tempo, 
         duration_ms)

tracks_all <- tracks_cleaning_step %>% 
  group_by(track_name, artist_name) %>% 
  summarize(avg_danceability = mean(danceability, na.rm = TRUE) ,
             avg_energy = mean(energy, na.rm = TRUE), avg_loudness = mean(loudness, na.rm = TRUE), 
             avg_speechiness = mean(speechiness, na.rm = TRUE),avg_acousticness = mean(acousticness, na.rm = TRUE),
             avg_instrumentalness = mean(instrumentalness, na.rm = TRUE),
             avg_liveness = mean(liveness, na.rm = TRUE),avg_valence = mean(valence, na.rm = TRUE),
             avg_tempo = mean(tempo, na.rm = TRUE),avg_duration_ms = mean(duration_ms, na.rm = TRUE))

#column types conversion before join both dataframes
str(top200_df_all)
str(tracks_all$artist_name)

top200_df_all$track_name <- as.character(top200_df_all$track_name)
top200_df_all$artist_name <- as.character(top200_df_all$artist_name)
top200_df_all$url <- as.character(top200_df_all$url)

#
# tracks <- tracks %>% 
#   rename(external_urls.spotify = url)

top200_df_tracks <- data.frame()
top200_df_tracks<- top200_df_all %>% left_join(tracks_all)


#We can find the artist whose songs will be not considered in future analysis.
cat(str_c("Total number of songs: ", nrow(top200_df_tracks),
          "\nNumber of songs with metadata: ", nrow(top200_df_tracks %>% na.omit()),
          "\nPercentage of songs with metadata: ", nrow(top200_df_tracks %>% na.omit()) / nrow(top200_df_tracks)))

#Hay un 60% de datos informados porque en ocasiones la funcion get_artist_audio_features no tiene datos de las
#que salen el top200 por ejemplo aerosmith y track_name = ] "I Don't Want to Miss a Thing - From the Touchstone film, \"Armageddon\""

#cleaning data
top200_df_tracks_vf <- top200_df_tracks %>% 
  mutate(year = year(top200_df_tracks$date)) %>% 
  select(position,Zone,date,year,track_name, artist_name, streams, avg_danceability, avg_energy, 
         avg_loudness, avg_speechiness,avg_acousticness,avg_instrumentalness, avg_liveness, avg_valence, avg_tempo, 
         avg_duration_ms)

#Including the last days of 2016 in the first week of 2017
top200_df_tracks_vf$year[top200_df_tracks_vf$year == 2016] <- 2017

#no data for 2016 in year column
top200_df_tracks_vf %>% 
  filter(year == 2016)

# Save data
# write_csv(top200_df_tracks_vf,"top200__vf.csv")

#Follow the previous steps is a good idea if you need to understand how the data were downloaded from Spotify Web API but you if you do 
# not want to wait until the processes finish, you can also upload de final output "top200_vf.csv". You can easily find it in the /data folder.

top200_df_tracks_vf <- read_csv("data/top200_vf.csv")





