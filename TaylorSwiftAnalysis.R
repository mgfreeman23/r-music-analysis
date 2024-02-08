library(tidyverse)

# reading in and viewing data
taylor_album_songs <- read.csv("C:/Users/malin/Downloads/taylor_album_songs.csv")
View(taylor_album_songs)

# getting more condensed view of data 
glimpse(taylor_album_songs)

# checking some of the averages of metrics of interest 
mean(taylor_album_songs$danceability, na.rm = TRUE)
mean(taylor_album_songs$valence, na.rm = TRUE)
mean(taylor_album_songs$loudness, na.rm = TRUE)
mean(taylor_album_songs$energy, na.rm = TRUE)

# data summary for each category
summary(taylor_album_songs)

# removing null values from categories of interest
taylor_album_songs$danceability <-ifelse(is.na(taylor_album_songs$danceability),
                                         median(taylor_album_songs$danceability, na.rm = TRUE),
                                         taylor_album_songs$danceability)
taylor_album_songs$loudness <-ifelse(is.na(taylor_album_songs$loudness),
                                     median(taylor_album_songs$loudness, na.rm = TRUE),
                                     taylor_album_songs$loudness)
taylor_album_songs$energy <-ifelse(is.na(taylor_album_songs$energy),
                                   median(taylor_album_songs$energy, na.rm = TRUE),
                                   taylor_album_songs$energy)
taylor_album_songs$duration_ms <-ifelse(is.na(taylor_album_songs$duration_ms),
                                        median(taylor_album_songs$duration_ms, na.rm = TRUE),
                                        taylor_album_songs$duration_ms)
taylor_album_songs$valence <-ifelse(is.na(taylor_album_songs$valence),
                                    median(taylor_album_songs$valence, na.rm = TRUE),
                                    taylor_album_songs$valence)
taylor_album_songs$tempo <-ifelse(is.na(taylor_album_songs$tempo),
                                  median(taylor_album_songs$tempo, na.rm = TRUE),
                                  taylor_album_songs$tempo)
summary(taylor_album_songs)

# extracting specific categories
taylor_album_songs <- select(taylor_album_songs, valence, explicit, danceability, acousticness, instrumentalness, energy,loudness, album_name, tempo)
summary(taylor_album_songs)


# grouping by album name and printing some metrics
taylor_album_songs %>%
  group_by(album_name) %>%
  summarise(mean(energy),
            mean(valence),
            mean(acousticness),
            mean(danceability))

# Experimenting with some data visualizations
ggplot(taylor_album_songs, aes(x = energy,
                               color = album_name)) +
  geom_histogram() 

ggplot(taylor_album_songs, aes(x = valence,
                               y = energy,
                               color = album_name)) +
  geom_point()

# Use statistical analysis to determine if all albums "sound the same"
summary(taylor_album_songs)

# creating a new dataframe with albums and song danceability
weight <- c(taylor_album_songs$danceability)
group <- c(taylor_album_songs$album_name)
taylor_danceability <- data.frame(weight, group)
print(taylor_danceability)

# plotting scatterplot of danceability for each album
p <- ggboxplot(taylor_danceability, x = "group", y = "weight")
p + rotate_x_text()

#using statistical function to determine if statistically significant diff between group medians
kruskal.test(weight ~ group, data = taylor_danceability)

# Repeat process for other categories!

#For valence
weight <- c(taylor_album_songs$valence)
group <- c(taylor_album_songs$album_name)
taylor_valence <- data.frame(weight, group)
print(taylor_valence)

p <- ggboxplot(taylor_valence, x = "group", y = "weight")
p + rotate_x_text()

kruskal.test(weight ~ group, data = taylor_valence)

#For tempo
weight <- c(taylor_album_songs$tempo)
group <- c(taylor_album_songs$album_name)
taylor_tempo <- data.frame(weight, group)
print(taylor_tempo)

p <- ggboxplot(taylor_tempo, x = "group", y = "weight")
p + rotate_x_text()

kruskal.test(weight ~ group, data = taylor_tempo)

#For energy
weight <- c(taylor_album_songs$energy)
group <- c(taylor_album_songs$album_name)
taylor_energy <- data.frame(weight, group)
print(taylor_energy)

p <- ggboxplot(taylor_energy, x = "group", y = "weight")
p + rotate_x_text()

kruskal.test(weight ~ group, data = taylor_energy)
