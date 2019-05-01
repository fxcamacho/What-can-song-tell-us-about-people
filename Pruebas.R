
joy <- get_artist_audio_features('The Beatles')
#install.packages("tidyverse")
#install.packages("knitr")
library("tidyverse")
library("knitr")

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()


a %>% 
  select playlist_name = "NAVIDAD 2016"

get_my_top_artists(time_range = 'long_term', limit = 5) %>% 
  select(artist_name, artist_genres) %>% 
  rowwise %>% 
  mutate(artist_genres = paste(artist_genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()