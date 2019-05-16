##################################################
## Project: What can music tell us about people?
## Script Purpose: Study the relation between hapiness and songs listened in a country
## Date: 05/16/2019
## Author: Félix Carlos Camacho Criado
##################################################

library(ggplot2)
library(readr)
library(countrycode)

#Loading data 

#https://worldhappiness.report/

all_tracks <- top200_df_tracks_vf
hapiness_2017 <- read_csv("data/WorldHappiness2017_Data.csv")
hapiness_2018 <- read_csv("data/WorldHappiness2018_Data.csv")
hapiness_2019 <- read_csv("data/WorldHappiness2019_Data.csv")

#exploratory analysis

glimpse(hapiness_2017)
glimpse(hapiness_2018)
glimpse(hapiness_2019)

#due the dataset were obtained from different sources, they need to be cleaned and unified by the same format

hapiness_2017 <- subset( hapiness_2017, select = -c(Whisker.high, Whisker.low))

hapiness_2017 <- hapiness_2017 %>% 
  dplyr::rename(
    Country = Country,
    n_rank = Happiness.Rank,
    Score = Happiness.Score,
    Economy = Economy..GDP.per.Capita.,
    Family = Family,
    Health_Life_Expectancy = Health..Life.Expectancy.,
    Freedom = Freedom,
    Generosity = Generosity,
    Corruption = Trust..Government.Corruption.,
    Residual = Dystopia.Residual)
  

#cleaning for 2018 dataset
hapiness_2018 <- hapiness_2018 %>% 
  dplyr::rename(
    Country = Country,
    n_rank = Rank,
    Score = Score,
    Economy = GDP_Per_Capita,
    Family = Social_Support,
    Health_Life_Expectancy = Healthy_Life_Expectancy,
    Freedom = Freedom_To_Make_Life_Choices,
    Generosity = Generosity,
    Corruption = Perceptions_Of_Corruption,
    Residual = Residual)


#convert 2 variables in double
hapiness_2018$Corruption <- as.double(hapiness_2018$Corruption)
hapiness_2018$Residual <- as.double(hapiness_2018$Residual)


#the idea of the following analysis is to compare the countries with highest values in valence feature versus
#countries with the highest values in this report

#2017
hapiness_2017
feature <- "avg_valence"
map_valence_2017 <- dance_by_year_top("2017",feature) %>% 
    dplyr::rename(
    Country = Zone)

happiness_vs_valence_2017 <- left_join(hapiness_2017,map_valence_2017) %>% 
  na.omit()

happiness_vs_valence_2017 <- happiness_vs_valence_2017 %>% 
  select(Country, Score, mean)

happiness_vs_valence_2017_df <- data.frame(happiness_vs_valence_2017$Country)
happiness_vs_valence_2017$continent <- countrycode(sourcevar = happiness_vs_valence_2017_df[, 1],
                            origin = "country.name",
                            destination = "continent")

ggplot(happiness_vs_valence_2017, aes(x=Score,y=mean, label = Country, colour = continent)) +
      geom_point() +
      geom_text(aes(label=Country),hjust=0, vjust=0) +
      scale_x_continuous(name = "Hapiness Score") +
      scale_y_continuous(name = "Valence") +
      ggtitle("2017 - Valence VS Hapiness") +
      theme_bw() 

hapiness_2018
feature <- "avg_valence"
map_valence_2018 <- dance_by_year_top("2018",feature) %>% 
  dplyr::rename(
    Country = Zone)

happiness_vs_valence_2018 <- left_join(hapiness_2018,map_valence_2018) %>% 
  na.omit()

happiness_vs_valence_2018 <- happiness_vs_valence_2018 %>% 
  select(Country, Score, mean)

happiness_vs_valence_2018_df <- data.frame(happiness_vs_valence_2018$Country)
happiness_vs_valence_2018$continent <- countrycode(sourcevar = happiness_vs_valence_2018_df[, 1],
                                                   origin = "country.name",
                                                   destination = "continent")

ggplot(happiness_vs_valence_2018, aes(x=Score,y=mean, label = Country, colour = continent)) +
  geom_point() +
  geom_text(aes(label=Country),hjust=0, vjust=0) +
  scale_x_continuous(name = "Hapiness Score") +
  scale_y_continuous(name = "Valence") +
  ggtitle("2018 - Valence VS Hapiness") +
  theme_bw() 


# lm(formula = mean ~ Score, data = happiness_vs_valence_2017)

#Valence density vs Ranking score by continent

feature <- "avg_valence"

song_valence_2018 <- song_by_year("2018",feature) %>% 
  dplyr::rename(
    Country = Zone)

happiness_vs_song_valence_2018 <- left_join(hapiness_2018,song_valence_2018) %>% 
  na.omit()

happiness_vs_song_valence_2018 <- happiness_vs_song_valence_2018 %>% 
  select(artist_name, track_name, Country, Score, avg_valence)

happiness_vs_song_valence_2018_df <- data.frame(happiness_vs_song_valence_2018$Country)
happiness_vs_song_valence_2018$continent <- countrycode(sourcevar = happiness_vs_song_valence_2018_df[, 1],
                                                   origin = "country.name",
                                                   destination = "continent")

happiness_vs_song_valence_2018 <- happiness_vs_song_valence_2018 %>% 
  select(Country, Score, artist_name, track_name, avg_valence, continent)

#data per continent
happiness_vs_song_valence_2018_eu <- happiness_vs_song_valence_2018 %>% 
  filter(continent == "Europe") %>% 
  arrange(desc(Score))

happiness_vs_song_valence_2018_am <- happiness_vs_song_valence_2018 %>% 
  filter(continent == "Americas")

happiness_vs_song_valence_2018_as <- happiness_vs_song_valence_2018 %>% 
  filter(continent == "Asia")

happiness_vs_song_valence_2018_oc <- happiness_vs_song_valence_2018 %>% 
  filter(continent == "Oceania")

happiness_vs_song_valence_2018_af <- happiness_vs_song_valence_2018 %>% 
  filter(continent == "Africa")

c_density_eu <- ggplot(happiness_vs_song_valence_2018_eu) +
  facet_wrap(paste("Score=",round(Score,2))~Country) +
  geom_density(aes(avg_valence, fill ="valence")) + 
  scale_x_continuous(name = "Valence") +
  scale_y_continuous(name = "Density") +
  ggtitle("2018 - Density plot of Valence VS Hapiness Score ") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

c_density_eu

c_density_am <- ggplot(happiness_vs_song_valence_2018_am) +
  facet_wrap(paste("Score=",round(Score,2))~Country) +
  geom_density(aes(avg_valence, fill ="valence")) + 
  scale_x_continuous(name = "Valence") +
  scale_y_continuous(name = "Density") +
  ggtitle("2018 - Density plot of Valence VS Hapiness Score ") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

c_density_am

c_density_as <- ggplot(happiness_vs_song_valence_2018_as) +
  facet_wrap(paste("Score=",round(Score,2))~Country) +
  geom_density(aes(avg_valence, fill ="valence")) + 
  scale_x_continuous(name = "Valence") +
  scale_y_continuous(name = "Density") +
  ggtitle("2018 - Density plot of Valence VS Hapiness Score ") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

c_density_as

c_density_oc <- ggplot(happiness_vs_song_valence_2018_oc) +
  facet_wrap(paste("Score=",round(Score,2))~Country) +
  geom_density(aes(avg_valence, fill ="valence")) + 
  scale_x_continuous(name = "Valence") +
  scale_y_continuous(name = "Density") +
  ggtitle("2018 - Density plot of Valence VS Hapiness Score ") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

c_density_oc


c_density_af <- ggplot(happiness_vs_song_valence_2018_af) +
  facet_wrap(paste("Score=",round(Score,2))~Country) +
  geom_density(aes(avg_valence, fill ="valence")) + 
  scale_x_continuous(name = "Valence") +
  scale_y_continuous(name = "Density") +
  ggtitle("2018 - Density plot of Valence VS Hapiness Score ") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

c_density_af

# regression model
summary(lm(log(mean) ~ log(Score) + continent, data = happiness_vs_valence_2018))

# scatterplot with linear regression

ggplot(happiness_vs_valence_2018, aes(x=Score,y=mean, label = Country, colour = continent)) +
  geom_point() +
  geom_text(aes(label=Country),hjust=0, vjust=0) +
  stat_smooth(method = "lm") + 
  scale_x_continuous(name = "Hapiness Score") +
  scale_y_continuous(name = "Valence") +
  ggtitle("Valence VS Hapiness") +
  theme_bw() 


#CLUSTERING
#Let's try and check how k-means group the countries in clusters
ggplot(happiness_vs_valence_2018, aes(x=Score,y=mean, label = Country, colour = continent)) +
  geom_point() +
  geom_text(aes(label=Country),hjust=0, vjust=0) +
  scale_x_continuous(name = "Hapiness Score") +
  scale_y_continuous(name = "Valence") +
  ggtitle("Valence VS Hapiness") +
  theme_bw() 


features_h <- c('Score', 'mean')
n_clusters <- 3
country_clusters <- kmeans(happiness_vs_valence_2018[, features_h], n_clusters, nstart = 30)

happiness_vs_valence_2018$cluster <- factor(country_clusters$cluster)

centroids <- data.frame(cluster = factor(seq(1:n_clusters)),
                        Score = country_clusters$centers[,'Score'],
                        mean = country_clusters$centers[,'mean'])

# cross tab of countries by cluster
print(table(happiness_vs_valence_2018$cluster, happiness_vs_valence_2018$continent))


g <- ggplot() + 
  geom_point(data = happiness_vs_valence_2018, 
             aes(x = Score, 
                 y = mean,
                 color = cluster),
             size = 3) +
  geom_text(data = happiness_vs_valence_2018,
            aes(x = Score,
                y = mean,
                label = Country, hjust=0, vjust=0,
                color = cluster))+
            # nudge_y = .2,
            # check_overlap = TRUE)
   geom_point(data = centroids,
              mapping = aes(x = Score,
                            y = mean,
                            color = cluster),
              size = 20,
              pch = 13) 

print(g)

