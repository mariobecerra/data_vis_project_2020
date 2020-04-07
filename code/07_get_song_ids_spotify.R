library(httr)
library(Rspotify)
library(tidyverse)
library(here)

# Keys for the Spotify API
keys_vec = read_csv(here("data/keys_spotify_api.csv"), col_names = F)
keys <- spotifyOAuth(keys_vec[1], keys_vec[2], keys_vec[3])


artist_info_spotify = read_csv(here("out/artist_info_spotify_clean.csv")) %>% 
  filter(!is.na(art_id_spotify))

songs_plays = readRDS(here("out/songs_plays.rds"))

artist_song_plays_top = songs_plays %>% 
  inner_join(artist_info_spotify, 
             by = c("uuid_artist" = "artid",
                    "Artist" = "artname"))


out_folder = "out/song_spotify_ids/"
dir.create(here(out_folder))

pattern_to_replace = "\\!|\\*|\\'|\\(|\\)|\\;|\\:|\\@|\\&|\\=|\\+|\\$|\\,|\\/|\\?|\\#|\\[|\\]"



# for(i in 1:nrow(artist_song_plays_top)){
# Doing it for all 800K songs took too long. Stopped at half of them.
for(i in 1:400645){
  
  song_name_i = artist_song_plays_top$Song[i]
  cat("Doing", i, "(", song_name_i, ")\n")
  
  file_name_i = here(
    out_folder, 
    paste0(
      str_pad(string = i, width = 6, side = "l", pad = "0"),
      ".rds"
      )
  )
  
  # If file does not exist
  if(!file.exists(file_name_i)){
    track = gsub(pattern = pattern_to_replace, replacement = "", song_name_i)
    song_info = try(searchTrack(track, keys), silent = T)
    
    # If there was no error
    if(inherits(song_info, "data.frame")){
      
      cat("\tNo error when searching track\n")
      
      filtered_i = song_info %>% 
        # Filter songs that have a matching artist and whose display_name contains the original song name
        filter(artists_IDs == artist_song_plays_top$art_id_spotify[i]) %>% 
        filter(grepl(x = display_name, pattern = song_name_i, fixed = T)) %>% 
        # Paste original info to cross later
        mutate(
          searched_track = track,
          uuid_artist = artist_song_plays_top$uuid_artist[i],
          Artist = artist_song_plays_top$Artist[i],
          Song = song_name_i)
      
      filter_song_name = filtered_i %>% 
        filter(display_name == song_name_i) %>% 
        top_n(popularity, n = 1)
      
      # If there wasn't an exact match, keep the most popular one
      if(nrow(filter_song_name) == 0){
        out = filtered_i %>% 
          top_n(popularity, n = 1)
      } else{
        out = filter_song_name
      }
      
      if(nrow(out) > 0){
        cat("\tSaving RDS...")
        saveRDS(out, file_name_i, compress = F)
        cat("Saved.\n")
      } else{
        cat("\tSaving RDS with empty rows...")
        saveRDS(out, file_name_i, compress = F)
        cat("Saved.\n")
      }
      
      
    } else{
      # If there was an error
      if(class(song_info) == "try-error"){
        cat("\t", as.character(song_info), "\n")
      } else{
        cat("Something else happened.\n")
      }
      
    }
  } else{
    # If file exists
    cat("\tFile exists. Moving on to next track.\n")
  }
  
}














