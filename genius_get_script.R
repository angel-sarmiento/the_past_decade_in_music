#This is a script to get data from the genius api, then join that to the songs that are in the billboard top 100

#libraries
library(tidyverse)
library(geniusr)
library(tidytext)

# Loading in the spotify album data 
spotify_albums <- read_csv("data/spotify_full_100_10s.csv") %>% 
  rename(id = id...12)


#This dataframe is going to be used to pull get the relevant lyrics for the songs
basic_music_df <- spotify_albums %>% 
  group_by(album.name) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  select(c("artist.name", "album.name", "track.name", "track_number"))

#Authenticating with the Genius API using a key stored locally.
source("../API_KEY/genius_authentication.R")


#Now to try to get the data 
inputs <- basic_music_df %>% 
  ungroup() %>% 
  select(c("artist.name", "track.name")) 

### For now, Just going to pull the lyrics for the first Album in the dataset, Frank Ocean's Blonde

Blonded <- inputs %>% 
  filter(artist.name == "Frank Ocean")

# Getting rid of some annoying characters
inputs <- basic_music_df %>% 
  ungroup() %>% 
  select(c("artist.name", "track.name")) %>% 
  mutate_all(funs(str_replace_all(track.name, " \\+ ", " ")))


#Just going to pull all of the songs manually. Genius API isn't very robust in this regard unfortunately.
# This is the worst thing I have ever done, but its the only thing that works.
song1 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[1, 2])
song2 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[2, 2]) 
song4 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[4, 2])
song5 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[5, 2])
song6 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[6, 2])
song7 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[7, 2])
song8 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[8, 2])
song9 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[9, 2])
song10 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[10, 2])
song11 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[11, 2])
song12 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[12, 2])
song13 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[13, 2])
song14 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[14, 2])
song15 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[15, 2])
song16 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[16, 2])
song17 <- get_lyrics_search(artist_name = "Frank Ocean", song_title=Blonded[17, 2])


songs_merged <- do.call("rbind", list(song1, song2, song4, song5, song6, 
                                      song7, song8, song9, song10, song11, song12, 
                                      song13, song14, song15, song16))

write.csv(songs_merged, "data/lyrics_FO_genius.csv")

### TODO: 


    
    
    
    
    
    