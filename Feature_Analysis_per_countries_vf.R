 
##################################################
## Project: What can music tell us about people?
## Script Purpose: 
## Date:
## Author: Félix Carlos Camacho Criado
##################################################

#install.packages("gridExtra")
#install.packages("countrycode")
#install.packages("plyr")
#install.packages("maps")
#install.packages("ggiraph")
#install.packages("ggplot2")

library(gridExtra)
library(countrycode)
library(plyr)
library(dplyr)
library(maps)
library(ggiraph)
library(ggplot2)


#FEATURE ANALYSIS PER COUNTRY

#Danceability analysis

feature <- "avg_danceability"
map_dance_2017 <- feature_by_year_top("2017",feature)

map_dance_2017 <- map_dance_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Danceability - 2017") +
  xlab("Zones") +
  theme_bw()

map_dance_2018 <- feature_by_year_top("2018",feature)
map_dance_2018 <- map_dance_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Danceability - 2018") +
  xlab("Zones") +
  theme_bw()


map_dance_2019 <- feature_by_year_top("2019", feature)
map_dance_2019 <- map_dance_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Danceability - 2019") +
  xlab("Zones") +
  theme_bw()


danceability_panel <- grid.arrange(map_dance_2017, map_dance_2018,map_dance_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)


#Energy Analysis

feature <- "avg_energy"

map_ener_2017 <- feature_by_year_top("2017",feature)
map_ener_2017 <- map_ener_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Energy - 2017") +
  xlab("Zones") +
  theme_bw()

map_ener_2018 <- feature_by_year_top("2018",feature)
map_ener_2018 <- map_ener_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Energy - 2018") +
  xlab("Zones") +
  theme_bw()


map_ener_2019 <- feature_by_year_top("2019", feature)
map_ener_2019 <- map_ener_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Energy - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_ener_2017, map_ener_2018,map_ener_2019)


energy2017 <- feature_map_interactive("2017", feature)
energy2018 <-feature_map_interactive("2018", feature)
energy2019 <-feature_map_interactive("2019", feature)

save(energy2017, file="images/energy2017.RData")
save(energy2018, file="images/energy2018.RData")
save(energy2019, file="images/energy2019.RData")


#Speechiness Analysis

feature <- "avg_speechiness"

map_speech_2017 <- feature_by_year_top("2017",feature)

map_speech_2017 <- map_speech_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Speechiness - 2017") +
  xlab("Zones") +
  theme_bw()

map_speech_2018 <- feature_by_year_top("2018",feature)
map_speech_2018 <- map_speech_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Speechiness - 2018") +
  xlab("Zones") +
  theme_bw()


map_speech_2019 <- feature_by_year_top("2019", feature)
map_speech_2019 <- map_speech_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Speechiness - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_speech_2017, map_speech_2018,map_speech_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)

#Liveness Analysis

feature <- "avg_liveness"

map_live_2017 <- feature_by_year_top("2017",feature)

map_live_2017 <- map_live_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("liveness - 2017") +
  xlab("Zones") +
  theme_bw()

map_live_2018 <- feature_by_year_top("2018",feature)
map_live_2018 <- map_live_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("liveness - 2018") +
  xlab("Zones") +
  theme_bw()


map_live_2019 <- feature_by_year_top("2019", feature)
map_live_2019 <- map_live_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("liveness - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_live_2017, map_live_2018,map_live_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)

# Duration Analysis

feature <- "avg_duration_ms"

map_dur_2017 <- feature_by_year_top("2017",feature)
map_dur_2017 <- map_dur_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2))), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Duration ms - 2017") +
  xlab("Zones") +
  theme_bw()

map_dur_2018 <- feature_by_year_top("2018",feature)
map_dur_2018 <- map_dur_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2))), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Duration ms - 2018") +
  xlab("Zones") +
  theme_bw()


map_dur_2019 <- feature_by_year_top("2019", feature)
map_dur_2019 <- map_dur_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2))), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Duration ms - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_dur_2017, map_dur_2018,map_dur_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)


# Acousticness Analysis

feature <- "avg_acousticness"

map_acous_2017 <- feature_by_year_top("2017",feature)
map_acous_2017 <- map_acous_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("acousticness - 2017") +
  xlab("Zones") +
  theme_bw()

map_acous_2018 <- feature_by_year_top("2018",feature)
map_acous_2018 <- map_acous_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("acousticness - 2018") +
  xlab("Zones") +
  theme_bw()


map_acous_2019 <- feature_by_year_top("2019", feature)
map_acous_2019 <- map_acous_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("acousticness - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_acous_2017, map_acous_2018,map_acous_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)


#Valence Analysis
feature <- "avg_valence"

map_val_2017 <- feature_by_year_top("2017",feature)
map_val_2017 <- map_val_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Valence - 2017") +
  xlab("Zones") +
  theme_bw()

map_val_2018 <- feature_by_year_top("2018",feature)
map_val_2018 <- map_val_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Valence - 2018") +
  xlab("Zones") +
  theme_bw()


map_val_2019 <- feature_by_year_top("2019", feature)
map_val_2019 <- map_val_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Valence - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_val_2017, map_val_2018,map_val_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)

#Loudness Analysis
feature <- "avg_loudness"

map_loud_2017 <- feature_by_year_top("2017",feature)
map_loud_2017 <- map_loud_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Loudness - 2017") +
  xlab("Zones") +
  theme_bw()

map_loud_2018 <- feature_by_year_top("2018",feature)
map_loud_2018 <- map_loud_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Loudness - 2018") +
  xlab("Zones") +
  theme_bw()


map_loud_2019 <- feature_by_year_top("2019", feature)
map_loud_2019 <- map_loud_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Loudness - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_loud_2017, map_loud_2018,map_loud_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)

#Mapa Instrumentalness

feature <- "avg_instrumentalness"

map_inst_2017 <- feature_by_year_top("2017",feature)
map_inst_2017 <- map_inst_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Instrumentalness - 2017") +
  xlab("Zones") +
  theme_bw()

map_inst_2018 <- feature_by_year_top("2018",feature)
map_inst_2018 <- map_inst_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Instrumentalness - 2018") +
  xlab("Zones") +
  theme_bw()


map_inst_2019 <- feature_by_year_top("2019", feature)
map_inst_2019 <- map_inst_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = paste(round(mean,2),"%")), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Instrumentalness - 2019") +
  xlab("Zones") +
  theme_bw()

instrumentalness_panel <- grid.arrange(map_inst_2017, map_inst_2018,map_inst_2019)

#no SAfrica data in 2017

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)

#Tempo Analysis

feature <- "avg_tempo"

map_tempo_2017 <- feature_by_year_top("2017",feature)
map_tempo_2017 <- map_tempo_2017[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = round(mean,2)), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Tempo - 2017") +
  xlab("Zones") +
  theme_bw()

map_tempo_2018 <- feature_by_year_top("2018",feature)
map_tempo_2018 <- map_tempo_2018[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "indianred2") +
  geom_text(aes(label = round(mean,2)), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Tempo - 2018") +
  xlab("Zones") +
  theme_bw()


map_tempo_2019 <- feature_by_year_top("2019", feature)
map_tempo_2019 <- map_tempo_2019[1:5,] %>% 
  ggplot(aes(reorder(Zone,-mean),mean)) +
  geom_bar(stat = "identity", fill = "khaki2") +
  geom_text(aes(label = round(mean,2)), size = 5, hjust = 0.5, vjust = 3, position ="stack") +
  ggtitle("Tempo - 2019") +
  xlab("Zones") +
  theme_bw()

grid.arrange(map_tempo_2017, map_tempo_2018,map_tempo_2019)

#FEATURE WORLD MAP
feature_map_interactive("2017", feature)
feature_map_interactive("2018", feature)
feature_map_interactive("2019", feature)
