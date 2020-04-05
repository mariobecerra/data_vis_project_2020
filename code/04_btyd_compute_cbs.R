library(BTYDplus)
library(tidyverse)
library(lubridate)
library(here)


# Takes a couple of minutes to read
songs <- read.table(here("data/userid-timestamp-artid-artname-traid-traname.tsv"), 
                    header = F, 
                    stringsAsFactors = F, 
                    quote = "", 
                    na.strings = "", 
                    sep = "\t", 
                    comment.char = "",
                    col.names = c("user", "timestamp", "uuid_artist", "Artist", "uuid_song", "Song")) %>% 
  select(cust = user, date = timestamp) %>% 
  as_tibble() %>% 
  mutate(date = ymd_hms(date))


# Takes about 20 seconds each
cbs_lastfm_day = BTYDplus::elog2cbs(songs, units = "day")

cbs_lastfm_week = BTYDplus::elog2cbs(songs, units = "week")

saveRDS(cbs_lastfm_day, here("out/cbs_lastfm_day.rds"))
saveRDS(cbs_lastfm_week, here("out/cbs_lastfm_week.rds"))

saveRDS(songs, here("out/transactions_btyd.rds"))

