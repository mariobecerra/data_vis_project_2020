library(tidyverse)
library(Rspotify)
library(here)

keys_vec = read_csv(here("data/keys_spotify_api.csv"), col_names = F)
keys <- spotifyOAuth(keys_vec[1], keys_vec[2], keys_vec[3])
users = read_tsv(here("data/userid-profile.tsv"))






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










library(tidyverse)
library(Rspotify)

users = read_tsv("../../data/lastfm-dataset-1K/userid-profile.tsv")
users %>% group_by(country) %>% tally() %>% arrange(desc(n))

library(ggdark)

users %>% 
  group_by(country) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(country = ifelse(is.na(country), "Unknown", country)) %>% 
  arrange(n) %>% 
  mutate(country = fct_reorder(.f = country, .x = n, .desc = T)) %>% 
  ggplot() +
  geom_bar(aes(country, n), stat = "identity") +
  dark_theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

artists_plays = readRDS("../../out/artists_plays.rds")


artists_plays %>% 
  ungroup() %>% 
  head(40) %>% 
  mutate(Artist = fct_reorder(.f = Artist, .x = number_of_plays, .desc = T)) %>% 
  ggplot() +
  geom_bar(aes(Artist, number_of_plays), stat = "identity") +
  dark_theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 




searchArtist("Becky G", keys)

searchArtist("Metallica", keys)

getTrack("6rqhFgbbKwnb9MLmUQDhG6", keys)
getFeatures("6rqhFgbbKwnb9MLmUQDhG6", keys) 
searchTrack("Roadhouse Blues", keys)
getFeatures("1Q5kgpp4pmyGqPwNBzkSrw", keys)



# https://developer.spotify.com/documentation/web-api/reference/search/search/
# https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-tracks/
# https://developer.spotify.com/documentation/web-api/
req = httr::GET(paste0("https://api.spotify.com/v1/search?q=", gsub(" ", "+", track), "&type=track"), httr::config(token = token))
json1 <- httr::content(req)
x <- json1$tracks$items
display_name = unlist(lapply(x, function(x) x$name))
id = unlist(lapply(x, function(x) x$id))
popularity <- unlist(lapply(x, function(x) x$popularity))
artist.pre <- lapply(x, function(x) x$artists)
artists_IDs <- plyr::ldply(artist.pre, data.frame)$id
artists <- plyr::ldply(artist.pre, data.frame)$name
type <- unlist(lapply(x, function(x) x$type))
dados <- data.frame(display_name, id, popularity, artists,
                    artists_IDs, type, stringsAsFactors = F)
dados[order(-popularity), ]




tracks = c("6rqhFgbbKwnb9MLmUQDhG6", "1Q5kgpp4pmyGqPwNBzkSrw", "5iqhyXGD2Ag4ZeLNyrffPv")

req2 = httr::GET(
  paste0("https://api.spotify.com/v1/tracks?ids=", paste0(tracks, collapse = ",")), 
  httr::config(token = token))
json2 <- httr::content(req2)
aaa = lapply(json2$tracks, function(x){
  display_name = x$name
  id = x$id
  popularity <- x$popularity
  artist.pre <-  x$artists
  artists_IDs <- plyr::ldply(artist.pre, data.frame)$id
  artists <- plyr::ldply(artist.pre, data.frame)$name
  type <- x$type
  dados <- data.frame(display_name, id, popularity, artists,
                      artists_IDs, type, stringsAsFactors = F)
})














