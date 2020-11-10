#This is a script used to pull data from the spotify API for use in the following projects
# Clustering the Past Decade in Music: https://github.com/angel-sarmiento/Spotify-Project
# The Past Decade of Music: https://github.com/angel-sarmiento/the_past_decade_in_music
# PostgreSQL Database

#Authentication Script
#NOT PRESENT IN PROJECT FOLDER FOR SECURITY REASONS
#source("../API_KEY/spotify_authentication.R")

# loading libraries

# Tidy data wrangling
library(tidyverse)
library(purrr)

#machine learning and plotting
library(caret)
library(dendextend)

#time series
library(lubridate)
library(tsibble)

#tag dataset
library(lastfmR)

#Pretty stuff
library(circlize)
library(wesanderson)
pal <- wes_palette("Moonrise3", 31, type = "continuous")
# library(highcharter)
# library(scales)
# Setting the seed for reproducability 
set.seed(5543)



playlist_id <- "0NdonfZ87AajuaMPaOwXp9"

# "51IpWbcq8OfFTZa1Ac5A2Z"
playlist <- get_playlist(playlist_id)

play_tracks_df <- get_playlist_tracks(playlist_id = playlist_id)

#getting the list of the best album names for pulling info
album_names <- as.vector(play_tracks_df$track.album.name)
album_ids <- as.vector(play_tracks_df$track.album.id)


# Wrangling
#making a dataframe with the data from all of the albums 

#this works much faster!
full_album_df <- map_df(album_ids, get_albums)

#getting the album tracks of each album 
full_tracks_df <- map_df(album_ids, get_album_tracks)


#getting the track features and appending them to the existing dataframe.
track_feats <- map_df(full_tracks_df$id, get_track_audio_features)


#dataframe with all of the tracks
full_df <- cbind(full_tracks_df, track_feats)

full_df <- subset(full_df, select=which(!duplicated(names(full_df)))) 


# ONLY RUN ONCE

#unnesting the artist's names
artists <- unnest(full_df, cols = c("artists"), names_repair = "universal")
artists <- artists %>% select(c(3, 7:33))
full_df <- artists
colnames(full_df)[1] <- "artist.name"
colnames(full_df)[9] <- "track.name"

#Removing unanted columns
rem_col <- c(3, 5, 6, 8, 10, 12, 13, 14, 26, 27)

full_df[rem_col] <- NULL
full_df <- full_df[!duplicated(full_df["track.name"]),]

#unnesting the album artists names 
albums.artists <- unnest(full_album_df, cols = c("artists"), names_repair = "universal")
colnames(albums.artists)[4] <- "artist.name"
colnames(albums.artists)[15] <- "album.name"

#More unnesting to make joining easier
albums.artists <- unnest(albums.artists, cols = c("tracks.items"), names_repair = "unique")
# changing the name of the column with track names
colnames(albums.artists)[33] <- "track.name"



albums <- albums.artists %>% select(c("album.name", "popularity", "release_date", "genres", "artist.name", "track.name"))



# Inner join and removal of duplicates for final dataset
full_dataset <- inner_join(full_df, albums, by = c("artist.name", "track.name"))




# tags <- map_df(as.vector(full_dataset$artist.name), get_tags); beepr::beep("coin")
# 
# write_csv(tags, "tags.csv")
tags <- read_csv("tags.csv")



#Getting appropriate tags
tags <- tags %>% 
  filter(tag_freq == 100)

#renaming the artist name column for a join
colnames(tags)[1] <- "artist.name"

#joining the tags dataframe with the full data
full_dataset <- left_join(full_dataset, tags, by = "artist.name")
full_dataset <- full_dataset[!duplicated(full_dataset["track.name"]),]

#Lets unnest the countries and rename the resulting column
final_dataset <- full_dataset %>% 
  mutate(available_markets = map(available_markets, as_tibble)) %>% 
  unnest(cols = c(available_markets)) %>% 
  rename(country = value) %>% 
  select(-c(genres))

#writing this data to a csv for later use
write_csv(final_dataset, "spotify_full_100_10s.csv")
