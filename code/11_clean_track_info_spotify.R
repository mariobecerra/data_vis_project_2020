library(tidyverse)
library(here)

song_counts = readRDS(here("out/songs_plays.rds"))
artist_info_spotify = read_csv(here("out/artist_info_spotify_clean.csv")) %>% 
  filter(!is.na(art_id_spotify))
song_info_spotify = read_csv(here("out/song_info_spotify.csv"))
song_spotify_IDs = readRDS(here("out/song_IDs_spotify.rds"))

spotify_info_join = song_spotify_IDs %>% 
  inner_join(song_info_spotify, by = c("id" = "track_id")) %>% 
  distinct()



artist_song_plays = song_counts %>% 
  inner_join(spotify_info_join, 
             by = c("uuid_artist" = "uuid_artist",
                    "Artist" = "Artist",
                    "Song" = "Song")) 


artist_song_plays %>% 
  select(
    uuid_artist, 
    Artist, 
    Song, 
    song_number_of_plays_lfm = number_of_plays, 
    song_number_of_users_listened_lfm = number_of_users_listened,
    artist_id_spotify = artists_IDs,
    artist_name_spotify = artists,
    track_name_spotify = display_name,
    track_id_spotify = id,
    track_duration_ms_spotify = track_duration_ms,
    track_explicit_spotify = track_explicit,
    track_popularity_spotify = track_popularity,
    album_id_spotify = album_id,
    album_name_spotify = album_name,
    album_release_date_spotify = album_release_date,
    album_release_date_precision_spotify = album_release_date_precision
  ) %>% 
  write_csv(., here("out/song_info_spotify_clean.csv"))




