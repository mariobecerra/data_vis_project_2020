library(data.table)
library(tidyverse)
library(Rspotify)
library(here)



artists_plays = readRDS(here("out/artists_plays.rds")) %>% 
  ungroup()


artist_info_folder = here("out/artist_info_spotify/")
file_names = list.files(artist_info_folder, full.names = T)


artist_info_spotify = map_df(file_names, function(f){
  cat("Reading file", f, "\n")
  # df = read.csv(f, header = F, colClasses = rep("character", 6))
  df = data.table::fread(f, sep = ",", header = F, na.strings = "NA", stringsAsFactors = F, colClasses = rep("character", 6))
  if(nrow(df) != 100) cat("\tFile with", nrow(df), "lines instead of 100.\n")
  return(df)
}) %>% 
  as_tibble() %>% 
  set_names(c("Artist_LastFM", "Artist_spotify", "id_spotify", "popularity_spotify", "followers_spotify", "genres_spotify")) %>% 
  mutate(followers_spotify = as.numeric(followers_spotify),
         popularity_spotify = as.numeric(popularity_spotify))


# I forgot to add the artist key in the files in out/artist_info_spotify/ , but it's no biggie because they're in the same order
# I checked that all artists are in the correct row in the following dataframe
artist_info = bind_cols(
  artists_plays,
  artist_info_spotify[1:nrow(artists_plays), ]) 


check_artists = artist_info %>% 
  filter(!is.na(Artist_spotify)) %>% 
  select(id_spotify, uuid_artist, new_id, Artist_LastFM, Artist_spotify, genres_spotify, number_of_plays ) %>% 
  # convert to lowercase and remove tildes
  mutate(artist_lfm_lower = iconv(tolower(Artist_LastFM), from="UTF-8", to="ASCII//TRANSLIT"),
         artist_spotify_lower = iconv(tolower(Artist_spotify), from="UTF-8", to="ASCII//TRANSLIT")) %>% 
  # remove punctuation characters
  mutate(artist_lfm_lower = str_replace_all(artist_lfm_lower, "[[:punct:]]", ""),
         artist_spotify_lower = str_replace_all(artist_spotify_lower, "[[:punct:]]", ""))



# These are the artists that have differences between the name gotten from Spotify and the one in LastFM
aaaa = check_artists %>% 
  filter(artist_lfm_lower != artist_spotify_lower) %>% 
  mutate(edit_dist = stringdist::stringdist(artist_lfm_lower, artist_spotify_lower))

# > aaaa$number_of_plays %>% sum()
# [1] 1516298
# # They respresent a total of 1,516,298 plays. Not that big a deal if we remove them.


# Artists with apparently correct Spotify info
artists_checked = check_artists %>% 
  filter(artist_lfm_lower == artist_spotify_lower) %>% 
  select(id_spotify, uuid_artist, new_id) %>% 
  left_join(artist_info) %>% 
  select(artid = uuid_artist,
         artname = Artist_LastFM,
         number_of_plays_lfm = number_of_plays,
         number_of_users_listened_lfm = number_of_users_listened,
         art_id_spotify = id_spotify,
         art_name_spotify = Artist_spotify,
         popularity_spotify,
         followers_spotify,
         genres_spotify
         )




# Create a final dataframe with NAs in the Spotify info where there was no info
# Note that some popular artists don't have Spotify information. That's because of the API process.
# For example, for some reason, if I use the API to look for "Björk", it does not find it. But if I look for "Bjork" it does.
# I think I should have removed tildes first. But it's too late now.
# Another example is "Beck". If I use searchArtist("Beck", keys) the first option is "Becky G", and the third option is "Beck".
# The problem here is that my code only checked for the firt option, and if it didn not match, it assigns an NA.
# I should have searched for the best matching string. But again, too late now, I won't do it again.
artists_final = artist_info %>% 
  select(artid = uuid_artist,
         artname = Artist_LastFM,
         number_of_plays_lfm = number_of_plays,
         number_of_users_listened_lfm = number_of_users_listened) %>% 
  left_join(artists_checked) %>% 
  distinct()

write_csv(artists_final, here("out/artist_info_spotify_clean.csv"))




