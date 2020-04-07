library(scales)
library(parallel)
library(tidyverse)
library(here)

song_files = list.files(here("out/song_spotify_ids/"), full.names = T)
n_files = length(song_files)
n_cores = detectCores() - 1

index_files = split(1:n_files, 1:n_cores)
log_folder = "out/log/"
dir.create(here(log_folder))


# Iterate in a parallel way on all files
song_info = mclapply(1:length(index_files), function(core_id){
  
  # Each core has its own log file
  log_file = here(log_folder, paste0("log_read_rds_", core_id, ".log"))
  file.create(log_file)
  
  # The indices of filenames corresponding to this core
  core_indices = index_files[[core_id]]
  
  # Iterate over all files in "core_indices"
  
  core_df = map_df(seq_along(core_indices), function(i){
  
    # Print progress on log file  
    if(i %% 100 == 1) {
      cat("Doing file", i, "out of", length(core_indices), "\n",
          file = log_file,
          append = T)
    }
    
    # Index of file to read
    n_file_i = core_indices[i]
    
    # Name of file to read
    file_name_i = song_files[i]
    
    # Read file and transform all columns to character because some are factors
    song_info = readRDS(file_name_i) %>% 
      as_tibble() %>% 
      mutate_all(~as.character(.))
    
    return(song_info)
  })
  return(core_df)
  
}) %>% 
  bind_rows()





# # In a non-parallel way
# song_info = map_df(seq_along(song_files), function(i){
#   
#   file_name_i = song_files[i]
#   cat("File ", comma(i), " of ", comma(n_files), " (", basename(file_name_i), ")\n",
#       sep = "")
#   
#   song_info = readRDS(file_name_i) %>% 
#     as_tibble() %>% 
#     mutate_all(~as.character(.))
#   
#   return(song_info)
# })


saveRDS(song_info, here("out/song_IDs_spotify.rds"))









# searchTrack("Roadhouse Blues", keys)
# 
# 
# 
# 
# 
# 
# # https://developer.spotify.com/documentation/web-api/reference/search/search/
# # https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-tracks/
# # https://developer.spotify.com/documentation/web-api/
# req = httr::GET(paste0("https://api.spotify.com/v1/search?q=", gsub(" ", "+", track), "&type=track"), httr::config(token = token))
# json1 <- httr::content(req)
# x <- json1$tracks$items
# display_name = unlist(lapply(x, function(x) x$name))
# id = unlist(lapply(x, function(x) x$id))
# popularity <- unlist(lapply(x, function(x) x$popularity))
# artist.pre <- lapply(x, function(x) x$artists)
# artists_IDs <- plyr::ldply(artist.pre, data.frame)$id
# artists <- plyr::ldply(artist.pre, data.frame)$name
# type <- unlist(lapply(x, function(x) x$type))
# dados <- data.frame(display_name, id, popularity, artists,
#                     artists_IDs, type, stringsAsFactors = F)
# dados[order(-popularity), ]
# 
# 
# 
# 
# tracks = c("6rqhFgbbKwnb9MLmUQDhG6", "1Q5kgpp4pmyGqPwNBzkSrw", "5iqhyXGD2Ag4ZeLNyrffPv")
# 
# req2 = httr::GET(
#   paste0("https://api.spotify.com/v1/tracks?ids=", paste0(tracks, collapse = ",")), 
#   httr::config(token = token))
# json2 <- httr::content(req2)
# aaa = lapply(json2$tracks, function(x){
#   display_name = x$name
#   id = x$id
#   popularity <- x$popularity
#   artist.pre <-  x$artists
#   artists_IDs <- plyr::ldply(artist.pre, data.frame)$id
#   artists <- plyr::ldply(artist.pre, data.frame)$name
#   type <- x$type
#   dados <- data.frame(display_name, id, popularity, artists,
#                       artists_IDs, type, stringsAsFactors = F)
# })







# getFeatures
# function (track_id, token) 
# {
#     req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/", 
#         track_id), httr::config(token = token))
#     json1 <- httr::content(req)
#     dados = data.frame(id = json1$id, danceability = json1$danceability, 
#         energy = json1$energy, key = json1$key, loudness = json1$loudness, 
#         mode = json1$mode, speechiness = json1$speechiness, acousticness = json1$acousticness, 
#         instrumentalness = json1$instrumentalness, liveness = json1$liveness, 
#         valence = json1$valence, tempo = json1$tempo, duration_ms = json1$duration_ms, 
#         time_signature = json1$time_signature, uri = json1$uri, 
#         analysis_url = json1$analysis_url, stringsAsFactors = F)
#     return(dados)
# }



# searchTrack
# function (track, token) 
# {
#     req <- httr::GET(paste0("https://api.spotify.com/v1/search?q=", 
#         gsub(" ", "+", track), "&type=track"), httr::config(token = token))
#     json1 <- httr::content(req)
#     x <- json1$tracks$items
#     display_name = unlist(lapply(x, function(x) x$name))
#     id = unlist(lapply(x, function(x) x$id))
#     popularity <- unlist(lapply(x, function(x) x$popularity))
#     artist.pre <- lapply(x, function(x) x$artists)
#     artists_IDs <- plyr::ldply(artist.pre, data.frame)$id
#     artists <- plyr::ldply(artist.pre, data.frame)$name
#     type <- unlist(lapply(x, function(x) x$type))
#     dados <- data.frame(display_name, id, popularity, artists, 
#         artists_IDs, type, stringsAsFactors = F)
#     return(dados[order(-popularity), ])
# }



