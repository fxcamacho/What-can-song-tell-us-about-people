#WebScrapping
# Target URL prefix and suffix
url_prefix <- 'https://spotifycharts.com/regional/'
url_middle <- '/weekly/'
url_suffix <- '/download'
# https://spotifycharts.com/regional/tr/weekly/2019-04-05--2019-04-12

# List of Column Names that should exist in the dataframe
col_names = c("Position","Track.Name","Artist","Streams","URL")


# This pulls data from a certain date
pull_top200 <- function(zone,date_ini) {
  
  # Create the URL target
  urls <- str_c(url_prefix,zone,url_middle,format.Date(date_ini),"--",format.Date(date_ini+7),url_suffix)
  
  # Pull data from Spotify
  tryCatch( {
    top200_df <- read.csv(urls, skip = 1) 
  } ,  error = function(e) {
    top200_df <- data.frame("Position","Track.Name","Artist","Streams","URL","Date","Zone")
    message("No data for this country/dates")
    print(e)})
  # There are sometimes file that do not exist, this handles the exception
  ifelse(all(colnames(top200_df) %in% col_names),
         top200_df <- top200_df %>% mutate(Date = date_ini, Zone = zone), # Create a date attribute
         top200_df <- data.frame()) # Return an empty data frame if invalid
  
  top200_df
}

# Get start and end date for 2017
start_date_2017 <- as.Date("2016/12/30")
end_date_2017 <- as.Date("2017/12/29")

# Get start and end date for 2018
start_date_2018 <- as.Date("2017/12/29")
end_date_2018 <- as.Date("2018/12/28")

# Get start and end date for 2019
start_date_2019 <- as.Date("2018/12/28")
end_date_2019 <- as.Date("2019/04/05")

# Iterate every date from start to end
# And get 2017 top 200 songs by country
    date <- start_date_2017 # set data counter
    top200_df_2017_tw <- data.frame() # initialize empty df
    i <- i
    for (i in length(zones$id_spotify)) {
      while (date <= end_date_2017) {
        print(str_c("Processing ", date," ", zones$descr[i]))
        # Concat the current data frame with new data
        top200_df_2017_tw <- top200_df_2017_vt %>% rbind(pull_top200(zones$id_spotify[i],date))
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

#unión de los 3 dataframes
top200_df_all <- rbind(top200_df_2017,top200_df_2018,top200_df_2019)
#top200_df_2017 = rbind(top200_df_2017_ie,top200_df_2017_it,top200_df_2017_tw)

top200_df_2017 %>% 
  write_csv("top200songs_2017.csv")

top200_df_2018 %>% 
  write_csv("top200songs_2018.csv")

top200_df_2019 %>% 
  write_csv("top200songs_2019.csv")

top200_df_2017 <- read_csv("top200songs_2017.csv")
top200_df_2018 <- read_csv("top200songs_2018.csv")
top200_df_2019 <- read_csv("top200songs_2019.csv")

top200_df_2017 <- top200_df_2017 %>%
  rename(position = Position,
         track_name = Track.Name,
         artist_name = Artist,
         streams = Streams,
         url = URL,
         date = Date)

top200_df_2018 <- top200_df_2018 %>%
  rename(position = Position,
         track_name = Track.Name,
         artist_name = Artist,
         streams = Streams,
         url = URL,
         date = Date)

top200_df_2019 <- top200_df_2019 %>%
  rename(position = Position,
         track_name = Track.Name,
         artist_name = Artist,
         streams = Streams,
         url = URL,
         date = Date)

top200_df_all = rbind(top200_df_2017,top200_df_2018,top200_df_2019)

top200_df_all %>% 
  write_csv("top200songs.csv")

#checking if each country has the same number of observations. It should be 24200 (53*53*15 weeks)

#il, in, jp, ro, th, vn and za have less data from spotify API than the rest 
top200_df_all %>% 
  group_by(Zone) %>% 
  summarise(numero = n()) %>% 
  filter(numero < 23200)

#Retrieving features of every track
# Pull song metadata
  
artists <- top200_df_all$artist_name%>% unique()
artists <- as.character(artists)
tracks <- data.frame()

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
      print("Succeed")
    }, error = function(e) { print(e) })

    # If the pull did not succeed, add the artist to the remaining artist list
    if (!succeed) {
      remaining_artists <- c(remaining_artists, artists[i])
    }
  }

  artists <- remaining_artists
}

# Save data
write_csv(tracks,"tracks.csv")

#tidy tracks in order to obtain the audio features and eliminate duplicated songs. For those songs which
#appears more than 1 time will be used the average of their audio features.

tracks_clean <- tracks %>% 
  select(track_name, artist_name, artist_id, danceability, energy, 
         loudness, speechiness,acousticness,instrumentalness, liveness, valence, tempo, 
         duration_ms)

tracks_group <- tracks_clean %>% 
  group_by(track_name, artist_name) %>% 
  summarize(avg_danceability = mean(danceability, na.rm = TRUE) ,
             avg_energy = mean(energy, na.rm = TRUE), avg_loudness = mean(loudness, na.rm = TRUE), 
             avg_speechiness = mean(speechiness, na.rm = TRUE),avg_acousticness = mean(acousticness, na.rm = TRUE),
             avg_instrumentalness = mean(instrumentalness, na.rm = TRUE),
             avg_liveness = mean(liveness, na.rm = TRUE),avg_valence = mean(valence, na.rm = TRUE),
             avg_tempo = mean(tempo, na.rm = TRUE),avg_duration_ms = mean(duration_ms, na.rm = TRUE))

# Join the tracks uri with top200
str(top200_df_all)
str(tracks$artist_name)
top200_df_all$track_name <- as.character(top200_df_all$track_name)
top200_df_all$artist_name <- as.character(top200_df_all$artist_name)
top200_df_all$url <- as.character(top200_df_all$url)
#
# tracks <- tracks %>% 
#   rename(external_urls.spotify = url)

top200_df_tracks <- data.frame()
top200_df_tracks<- top200_df_all %>% left_join(tracks_group)

#Instead of this we notice some error in the info request to Spotify API. Function get_artist_audio_features
#is not working ok for all artist.

#We can find the artist whose songs will be not considered in future analysis.
findNAs <- function (df){
  for (i in 1:length(df)){
    a<-df %>% 
    filter(is.na(df[i])) %>% 
    count()
   cat(str_c("The column " ,colnames(df[i]), " has ", a, " NA values \n" ))
  }
}

cat(str_c("Total number of songs: ", nrow(top200_df_tracks),
          "\nNumber of songs with metadata: ", nrow(top200_df_tracks %>% na.omit()),
          "\nPercentage of songs with metadata: ", nrow(top200_df_tracks %>% na.omit()) / nrow(top200_df_tracks)))

#Hay un 60% de datos informados porque en ocasiones la funcion get_artist_audio_features no tiene datos de las
#que salen el top200 por ejemplo aerosmith y track_name = ] "I Don't Want to Miss a Thing - From the Touchstone film, \"Armageddon\""

#Check if there's a country with a special lack of data in order to exclude them of our analysis
#/*
tot <- top200_df_tracks %>% 
  count(Zone)

totNA <- top200_df_tracks %>% 
  filter(is.na(top200_df_tracks$avg_energy)) %>% 
  count(Zone)

globtot <- tot %>% 
  left_join(totNA, by = "Zone") %>% 
  mutate(perc = n.y/n.x) %>% 
  filter(perc > 0.8)

totyear <- top200_df_tracks %>% 
  mutate(year = year(top200_df_tracks$date)) %>% 
  count(year)
totyear

totyearNA <- top200_df_tracks %>% 
  mutate(year = year(top200_df_tracks$date)) %>% 
  filter(is.na(top200_df_tracks$avg_energy)) %>% 
  count(year)
totyearNA

totyeartot <- totyear %>% 
  left_join(totyearNA, by = "year") %>% 
  mutate(perc = n.y/n.x) 
 
totyeartot

#ojito 2019!!! 50% sin informar, mejora un 1% mas.... no merece la pena.

#*/
# Save data
top200_df_tracks <- read_csv("top200_info.csv")

write_csv(top200_df_tracks,"top200_info.csv")

#cleaning data
top200_df_tracks_c <- top200_df_tracks %>% 
  mutate(year = year(top200_df_tracks_c$date)) %>% 
  select(position,Zone,date,year,track_name, artist_name, streams, avg_danceability, avg_energy, 
         avg_loudness, avg_speechiness,avg_acousticness,avg_instrumentalness, avg_liveness, avg_valence, avg_tempo, 
         avg_duration_ms)

# Save data

write_csv(top200_df_tracks_c,"top200_vf.csv")
top200_df_tracks_c <- read_csv("top200_vf.csv")




