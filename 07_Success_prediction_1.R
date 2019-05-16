
##################################################
## Project: What can music tell us about people?
## Script Purpose: Define a target which classify an dataset song in very popular or not
## Date: 05/16/2019
## Author: Félix Carlos Camacho Criado
##################################################

# install.packages("zoo")
# install.packages("psych")

library(zoo)
library(psych)

#Over the whole song dataset it will be considered if a song could be considered popular or not based in the
#the value of their features. 

#Preparing the dataset and obtaining the more listened songs 

df_notglobal <- top200_df_tracks_vf %>% 
  filter(Zone != "global") %>% 
  group_by(track_name, artist_name) %>% 
  na.omit() %>% 
  summarise_at(vars(starts_with("streams")), list(sum_streams = sum), na.rm =TRUE) %>% 
  arrange(desc(sum_streams)) %>% 
  unique()

df_features <- top200_df_tracks_vf %>% 
  filter(Zone != "global") %>% 
  na.omit() %>% 
  select(track_name, artist_name,avg_danceability,avg_energy,avg_loudness,avg_speechiness,avg_acousticness,avg_instrumentalness
         ,avg_liveness,avg_valence,avg_tempo,avg_duration_ms) %>% 
  unique()

df_songs_all <- left_join(df_notglobal, df_features)


#First of all it is necessary to define a target, so based on the dataset it has been decided to considered a
#a Worldwide popular song the 25% of the whole dataset.

describe(df_songs_all$sum_streams)

#(14449*25)/100 ~ 3600 more listened songs will be considered the target = 1

df_songs_all["popularity"] <- index(df_songs_all)
df_songs_all["target"] <- df_songs_all["popularity"]<3600
df_songs_all[,15] <- lapply(df_songs_all[,15], as.numeric)

#The analysis will be done in Python using a Jupyter notebook. Please check "Success_prediction.ipynb" file
# write.csv(df_songs_all, "popularity.csv")
