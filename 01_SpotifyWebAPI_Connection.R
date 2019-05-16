##################################################
## Project: What can music tell us about people?
## Script Purpose: Define a conection with Spotify Web API
## Date: 05/16/2019
## Author: Félix Carlos Camacho Criado
##################################################

#Package Installation
install.packages('spotifyr')

#Package Load
library(spotifyr)

#Authentication process, you should substitute 32 Xs values for your SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET codes.

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')

#get_spotify_access_token() creates a Spotify access token in order to access Spotify Web API.
access_token <- get_spotify_access_token(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                                   client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"))

