library(tidyverse)
library(here)


filenames = list.files(here("out/song_info_spotify/"), full.names = T)


info = map_df(filenames, function(x){
  out = readRDS(x)
  return(out)
})

write_csv(info, here("out/song_info_spotify.csv"))
