#database Project to be included with Spotify Project

# Database PROJECT

```{r}
#unnesting the artist's names
#artists <- unnest(album_df, cols = c("artists"), names_repair = "universal")
#artists <- artists %>% select(c(3, 7:33))
art_names <- artists %>% select(c(name...4, id...3)) 

colnames(art_names)[1:2] <- c("name", "id")

#Getting a dataframe with all of the artist info 

artist_df <- map_df(art_names$id, get_artists)
```


```{r}
#Getting the columns that make sense for the database 
album_df <- full_album_df %>% 
  select(c(album_type, artists, external_urls.spotify, 
           href, id, name, release_date, release_date_precision, type, uri, external_urls.spotify)) %>% 
  unnest(cols = c("artists"), names_repair = "minimal")

colnames(album_df)[4] <- "artist"

tracks_df <- full_tracks_df %>% 
  select(c(artists, disc_number, duration_ms, explicit, 
           external_urls.spotify, href, id, name, preview_url, track_number, type, uri)) %>% 
  unnest(cols = c("artists"), names_repair = "minimal")

markets_df <- full_album_df %>% 
  select(c(available_markets, id)) %>% 
  unnest(cols = c("available_markets"), names_repair = "minimal")

artist_df <- artist_df %>% 
  select(c(external_urls.spotify, href, id, name, type, uri))
```




```{r SPOTIFY DATABASE}
# Writing all of these files to csvs 
write_csv(album_df, "albums.csv")
write_csv(tracks_df, "tracks.csv")
write_csv(track_feats, "track_features.csv")
write_csv(artist_df, "artist_df.csv")
# write_csv(markets_df, "markets_df.csv")
```
