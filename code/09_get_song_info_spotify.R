library(httr)
library(Rspotify)
library(tidyverse)
library(here)

# Keys for the Spotify API
keys_vec = read_csv(here("data/keys_spotify_api.csv"), col_names = F)
keys <- spotifyOAuth(keys_vec[1], keys_vec[2], keys_vec[3])


song_ids = readRDS(here("out/song_IDs_spotify.rds"))



get_tracks_info = function(tracks, keys){
  # tracks: character vector with Spotify IDs of songs
  # keys: spotifyOAuth object
  
  column_names = c("track_duration_ms",
                   "track_explicit",
                   "track_href",
                   "track_id",
                   "track_is_local",
                   "track_name",
                   "track_popularity",
                   "track_track_number",
                   "track_type",
                   "track_uri",
                   "album_href",
                   "album_id",
                   "album_name",
                   "album_release_date",
                   "album_release_date_precision",
                   "album_total_tracks",
                   "album_type",
                   "album_uri")
  
  
  json_info <- httr::content(httr::GET(
    paste0("https://api.spotify.com/v1/tracks?ids=", paste0(tracks, collapse = ",")),
    httr::config(token = keys)))
  
  
  
  tracks_info = lapply(seq_along(json_info$tracks), function(i){
    x = json_info$tracks[[i]]
    
    # Try to get info for that track
    info_track_i = try(tibble(
      track_duration_ms = x$duration_ms,
      track_explicit = x$explicit,
      track_href = x$href,
      track_id = x$id,
      track_is_local = x$is_local,
      track_name = x$name,
      track_popularity = x$popularity,
      track_track_number = x$track_number,
      track_type = x$type,
      track_uri = x$uri,
      album_href = x$album$href,
      album_id = x$album$id,
      album_name = x$album$name,
      album_release_date = x$album$release_date,
      album_release_date_precision = x$album$release_date_precision,
      album_total_tracks = x$album$total_tracks,
      album_type = x$album$type,
      album_uri = x$album$uri
    ) %>% 
      mutate_all(~as.character(.)), silent = T)
    
    if(inherits(info_track_i, "data.frame")){
      return(info_track_i)
    } else{
      aaa = rep(NA_character_, length(column_names))
      names(aaa) = column_names
      out = bind_rows(aaa)
      out$track_id = tracks[i]
      return(out)
      
    }
    
  }) %>% 
    bind_rows()
  
  return(tracks_info)
  
}






n_songs = nrow(song_ids)
n_tracks_per_iter = 50
n_iter = ceiling(n_songs/n_tracks_per_iter)

out_folder = "out/song_info_spotify/"
dir.create(here(out_folder))

for(i in 1:n_iter){
  cat("Iteration", i, "of", n_iter, "\n")
  row_init = (i-1)*n_tracks_per_iter + 1
  if(i != n_iter){
    row_end = (i)*n_tracks_per_iter   
  } else{
    row_end = n_songs
  }
  
  
  tracks = song_ids$id[row_init:row_end]
  
  track_info_i = get_tracks_info(tracks, keys)
  
  filename_i = here(
    out_folder,
    paste0(str_pad(string = i, width = nchar(n_iter) + 1, side = "left", pad = "0"), 
           ".rds")
  )
  
  cat("Saving RDS file...")
  saveRDS(track_info_i, filename_i)
  cat("Saved.\n")
  
}






