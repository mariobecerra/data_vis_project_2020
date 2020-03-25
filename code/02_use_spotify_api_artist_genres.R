library(tidyverse)
library(Rspotify)
library(here)

# Keys for the Spotify API
keys_vec = read_csv(here("data/keys_spotify_api.csv"), col_names = F)
keys <- spotifyOAuth(keys_vec[1], keys_vec[2], keys_vec[3])

# Read data
users = read_tsv(here("data/userid-profile.tsv"))
artists_plays = readRDS(here("out/artists_plays.rds"))





get_artist_info <- function(artist_name, token) {
  artist_info <- try(searchArtist(artist_name, token), silent = T)
  
  if(class(artist_info) == "try-error"){
    out_vec = c(artist_name, rep(NA_character_, 5))
  } else{
    artist = try(artist_info$artist[[1]])
    genres <- try(artist_info$genres[[1]])
    id = try(artist_info$id[[1]])
    popularity = try(artist_info$popularity[[1]])
    followers = try(artist_info$followers[[1]])
    
    out_vec = c(artist_name,
                as.character(try(artist_info$artist[[1]])),
                as.character(try(artist_info$id[[1]])),
                as.character(try(artist_info$popularity[[1]])),
                as.character(try(artist_info$followers[[1]])),
                as.character(try(artist_info$genres[[1]]))
    )
  }
  
  return(out_vec)
}








# Iterates over the artists in the artists_plays dataframe and looks for information such as genres, number of followers, etc.
# It saves the information in groups of 100 artists and writes the information in csv files in a folder.
n_per_group = 100
n_iter = ceiling(nrow(artists_plays)/n_per_group)
n_last_group = nrow(artists_plays) - n_per_group*(n_iter-1)

artist_info_folder = here("out/artist_info_spotify/")
dir.create(artist_info_folder)

for(k in 1:n_iter){
  
  k_init = (k-1)*n_per_group + 1
  
  if(k == n_iter){
    k_end = k_init + n_last_group - 1
  } else{
    k_end = k*n_per_group  
  }
  
  
  k_init_pad = str_pad(k_init, width = nchar(as.character(nrow(artists_plays))), side = 'left', pad = '0')
  k_end_pad = str_pad(k_end, width = nchar(as.character(nrow(artists_plays))), side = 'left', pad = '0')
  file_name_k = paste0(artist_info_folder, "/artists_", k_init_pad, "_", k_end_pad, ".csv")
  
  cat("Doing from", k_init, "to", k_end, "out of", nrow(artists_plays), "(", as.character(Sys.time()), ")\n")
  
  if(!file.exists(file_name_k)){
    
    k_th_tibble = tibble(
      artist_name = rep(NA_character_, n_per_group),
      artist = rep(NA_character_, n_per_group),
      id = rep(NA_character_, n_per_group),
      popularity = rep(NA_character_, n_per_group),
      followers = rep(NA_character_, n_per_group),
      genres = rep(NA_character_, n_per_group)
    )
    
    counter = 1
    for(i in k_init:k_end){
      # print(counter)
      artist_name = strsplit(artists_plays$Artist[i], '_')[[1]][2]
      artist_info = try(get_artist_info(artist_name, keys), silent = T)
      
      # In case there's an unxpected error with the function
      if(class(artist_info) == "try-error"){
        cat("Caught error in artist ", artist_name, ".\n", 
                "Error:\n", as.character(artist_info))
        artist_info = c(artist_name, rep(NA_character_, 5))
      }
      # cat(paste0('"', paste(artist_info, collapse = '\",\"'), '"'))
      
      k_th_tibble[counter, ] = artist_info
      
      counter = counter + 1
    }
    
    write.table(x = k_th_tibble, file = file_name_k, quote = T, row.names = F, col.names = F, sep = ",")
    
  } else{
    cat("File ", file_name_k, " exists.\nSkipping to next segment of artists.")
  }
  
}






















