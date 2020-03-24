library(tidyverse)

songs <- read.table(here("data/userid-timestamp-artid-artname-traid-traname.tsv"), 
                        header = F, 
                        stringsAsFactors = F, 
                        quote = "", 
                        na.strings = "", 
                        sep = "\t", 
                        comment.char = "",
                        col.names = c("user", "timestamp", "uuid_artist", "Artist", "uuid_song", "Song"))

trackMap <- read_tsv(here("out/trackMap.tsv"),
                     col_names = c("new_id", "uuid_artist"))

artists_plays <- songs %>% 
  group_by(uuid_artist, Artist) %>% 
  summarise(number_of_plays = n(),
            number_of_users_listened = length(unique(user))) %>% 
  left_join(trackMap) %>% 
  mutate(Artist = paste0(new_id, "_", Artist)) %>% 
  arrange(desc(number_of_plays))
  
saveRDS(artists_plays, here("out/artists_plays.rds"))
