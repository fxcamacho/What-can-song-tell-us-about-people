##################################################
## Project: What can music tell us about people?
## Script Purpose: Feature analysis
## Date: 05/16/2019
## Author: Félix Carlos Camacho Criado
##################################################

# install.packages("zoo")
# install.packages("psych")
# install.packages("corrplot")
# install.packages("forcats")
# install.packages("gridExtra")

library(zoo)
library(dplyr)
library(psych)
library(ggplot2)
library(corrplot)
library(forcats)
library(gridExtra)

#Which songs generate more revenues?
#Spotify The streaming music giant now reportedly pays $0.00437 per play.
#sources: https://www.digitalmusicnews.com/2018/12/25/streaming-music-services-pay-2019/

revenue_per_stream = 0.00437

#Songs that have generated more revenues
tracks_by_revenue <- top200_df_tracks_vf %>% 
  filter(Zone == "global") %>% 
  group_by(track_name, artist_name) %>% 
  mutate(revenue = streams * revenue_per_stream) %>% 
  summarise_each (funs(sum), sum_rev = revenue) %>% 
  arrange(desc(sum_rev))


#This is a common analysis for all countries so it will be filter by zone = global
track_by_feature <- top200_df_tracks_vf %>% 
  filter(Zone == "global") %>% 
  group_by(track_name, artist_name) %>% 
  summarise_at(vars("avg_danceability","avg_energy","avg_loudness","avg_speechiness","avg_acousticness",
                    "avg_instrumentalness","avg_liveness","avg_valence","avg_tempo","avg_duration_ms"), funs(mean))

#matching both df for feature analysis
tracks_joins <- data.frame()
tracks_joins<- left_join(tracks_by_revenue, track_by_feature)

#Let's check the correlation between variables. At this point, it will only be selected the features with values
#between 0 an 1.

tracks_joins %>% 
  select(track_name,artist_name, sum_rev, avg_danceability, avg_energy, avg_speechiness,avg_acousticness,
         avg_liveness, avg_valence)

#delete the first three columns because just need the features columns for checking the correlation between them
spotify_features <- tracks_joins[,-(1:3)] %>% 
  na.omit()

mtCor <- cor(spotify_features)
corrplot(mtCor, method = "ellipse", type = "upper", tl.srt = 60)


# It seems like energy and loudness are highly positively correlated.
# Also, valence is positively correlated with danceability and energy. 
# Considering happy songs usually make people feeling good and they are suitable for dancing , the correlation make a lot sense. 
# Interestingly, acoustiness is highly negative correlated with energy and loudness as a acoustic song usually is softly perfom

# The relation between danceability/energy/valence can be observed in the following density graph

correlated_density <- ggplot(tracks_joins) +
  geom_density(aes(avg_energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(avg_valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(avg_danceability, fill ="danceability", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Valence and Danceability") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Valence and Danceability") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

correlated_density

# #You can see that the Ed Sheeran song "Shape of you" have generated much money than the rest of the songs with
# a huge difference of $. So, due to visualization issues this songs will be considered as a outlier and it will
# not be considered for this specific panel

#Let check how these more streamed songs are distributed based in their features.

pan_energy <- ggplot(data = tracks_joins[-1,], mapping = aes(y = sum_rev)) +
  geom_bin2d(aes(x = avg_energy), na.rm = TRUE) +
  scale_fill_continuous(low = "khaki1", high = "red") +
  theme_bw()

pan_valence <- ggplot(data = tracks_joins[-1,], mapping = aes(y = sum_rev)) +
  geom_bin2d(aes(x = avg_valence), na.rm = TRUE) +
  scale_fill_continuous(low = "khaki1", high = "red") +
  theme_bw()

pan_dance <- ggplot(data = tracks_joins[-1,], mapping = aes(y = sum_rev)) +
  geom_bin2d(aes(x = avg_danceability), na.rm = TRUE) +
  scale_fill_continuous(low = "lightblue", high = "midnightblue") +
  theme_bw()

pan_speech <- ggplot(data = tracks_joins[-1,], mapping = aes(y = sum_rev)) +
  geom_bin2d(aes(x = avg_speechiness), na.rm = TRUE) +
  scale_fill_continuous(low = "lightblue", high = "midnightblue") +
  theme_bw()

pan_acous <- ggplot(data = tracks_joins[-1,], mapping = aes(y = sum_rev)) +
  geom_bin2d(aes(x = avg_acousticness), na.rm = TRUE) +
  scale_fill_continuous(low = "mistyrose", high = "magenta4") +
  theme_bw()

pan_live <- ggplot(data = tracks_joins[-1,], mapping = aes(y = sum_rev)) +
  geom_bin2d(aes(x = avg_liveness), na.rm = TRUE) +
  scale_fill_continuous(low = "mistyrose", high = "magenta4") +
  theme_bw()

grid.arrange(pan_energy, pan_valence,pan_dance, pan_speech, pan_acous, pan_live,  nrow = 3)

#Let see the average values for each feature

spotify_features <- spotify_features[,-(3)] 
spotify_features_means <- spotify_features  %>% summarise_all(funs(mean))

x1 <- colnames(spotify_features_means)
Avg_features <- x1[1:7]
v1 <- spotify_features_means$avg_danceability
v2 <- spotify_features_means$avg_energy
v3 <- spotify_features_means$avg_loudness
v4 <- spotify_features_means$avg_speechiness
v6 <- spotify_features_means$avg_instrumentalness
v7 <- spotify_features_means$avg_liveness
v8 <- spotify_features_means$avg_valence

Values= c(v1,v2,v4,v5,v6,v7,v8)

spotify_features_means_bar <- data.frame(Avg_features,Values)

ggplot(data=spotify_features_means_bar, aes(x=Avg_features, y=Values)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label = paste(round(Values,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ylim(0,1) +
  theme_minimal()


#ARTIST ANALYSIS

#Tidying dataframe
df_global <- top200_df_tracks_vf %>% 
  filter(Zone == "global") %>% 
  group_by(track_name, artist_name) %>% 
  na.omit() %>% 
  summarise_at(vars(starts_with("streams")), list(sum_streams = sum), na.rm =TRUE) %>% 
  arrange(desc(sum_streams)) %>% 
  unique()

df_features_global <- top200_df_tracks_vf %>% 
  filter(Zone == "global") %>% 
  na.omit() %>% 
  select(track_name, artist_name,avg_danceability,avg_energy,avg_loudness,avg_speechiness,avg_acousticness,avg_instrumentalness
         ,avg_liveness,avg_valence,avg_tempo,avg_duration_ms) %>% 
  unique()

df_songs_pop <- left_join(df_global, df_features_global)

df_songs_pop$artist_name <- as.factor(df_songs_pop$artist_name)

#Here You can see the artist with more number of hits in the 500 more listened songg list
# in the world through the last 2 years and a half.
more_hits_artist <- df_songs_pop[1:500,] %>% 
  group_by(artist_name) %>% 
  summarise(numero = n()) %>% 
  arrange(desc(numero))

more_hits_artist[1:10,] %>% 
  mutate(artist_name = fct_reorder(artist_name, numero)) %>%
  ggplot( aes(x=artist_name, y= numero)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  ggtitle("TOP 10 Artists with more hits streamed in 2017/2018/2019") +
  coord_flip() +
  theme_bw()

#But if you check who's the artist which earn more money in the last 2 year and a half due to the number of
#streaming... Drake is not the winner, anyway he earned a lot as he's the second one in the list

more_revenues_artist <- df_songs_pop %>% 
  group_by(artist_name) %>% 
  summarise(revenues = sum(sum_streams)) %>% 
  arrange(desc(revenues))

more_revenues_artist[1:10,] %>% 
  mutate(artist_name = fct_reorder(artist_name, revenues)) %>%
  ggplot( aes(x=artist_name, y= revenues)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  ggtitle("TOP 10 Artists who earned more money based on Spotify streamings in 2017/2018/2019") +
  coord_flip() +
  theme_bw()