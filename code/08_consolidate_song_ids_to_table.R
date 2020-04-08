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
# Takes like 2 minutes in 11 cores and 280 thousand files
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
    file_name_i = song_files[n_file_i]
    
    # Read file and transform all columns to character because some are factors
    song_info = readRDS(file_name_i) %>% 
      as_tibble() %>% 
      mutate_all(~as.character(.))
    
    return(song_info)
  })
  return(core_df)
  
}, mc.cores = n_cores) %>% 
  bind_rows()



saveRDS(song_info, here("out/song_IDs_spotify.rds"))





