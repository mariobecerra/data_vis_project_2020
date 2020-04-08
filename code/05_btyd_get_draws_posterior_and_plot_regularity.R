library(stringr)
library(BTYDplus)
library(tidyverse)
library(lubridate)
library(here)


theme_set(theme_bw())

# devtools::load_all("~/Desktop/btyd_baz-master/")
# pggg_mcmc_DrawParameters
# elog2cbs()


cbs_lastfm_day = readRDS(here("out/cbs_lastfm_day.rds"))
# cbs_lastfm_week = readRDS(here("out/cbs_lastfm_week.rds"))
transactions = readRDS(here("out/transactions_btyd.rds"))



draws_day = pggg.mcmc.DrawParameters(
  cal.cbs = cbs_lastfm_day,
  mcmc = 100,
  burnin = 10,
  thin = 1,
  chains = 2,
  mc.cores = 2
)

saveRDS(draws_day, here("out/btyd_draws_day.rds"))



tidy_draws_day = lapply(1:length(draws_day$level_1), function(i){
  cat("Customer", i, "\n")
  n_chains = length(draws_day$level_1[[i]])
  
  params_cust = lapply(1:n_chains, function(chain){
    out_chain = draws_day$level_1[[i]][[chain]] %>% 
      as.data.frame() %>% 
      mutate(chain = chain) %>% 
      as_tibble()
    return(out_chain)
  }) %>% 
    bind_rows() %>% 
    mutate(cust = i)
  
  return(params_cust)
}) %>% 
  bind_rows()

saveRDS(tidy_draws_day, here("out/btyd_tidy_draws_day.rds"))


summary_k_day = tidy_draws_day %>% 
  group_by(cust) %>% 
  summarize(
    median_k = median(k),
    k_p10 = as.numeric(quantile(k, 0.1)),
    k_p90 = as.numeric(quantile(k, 0.9))
  ) %>% 
  mutate(usid = cbs_lastfm_day$cust)




most_regular_day = summary_k_day %>% 
  top_n(30, median_k) %>% 
  arrange(median_k) %>% 
  mutate(ix = 1:nrow(.)) 


least_regular_day = summary_k_day %>% 
  top_n(-30, median_k) %>% 
  arrange(desc(median_k)) %>% 
  mutate(ix = 1:nrow(.)) 


data_plot_regular_day = most_regular_day %>% 
  left_join(transactions, by = c("usid" = "cust")) %>% 
  mutate(date2 = floor_date(date, unit = "day"),
    y = 1) %>% 
  select(-date) %>% 
  distinct()


data_plot_irregular_day = least_regular_day %>% 
  left_join(transactions, by = c("usid" = "cust")) %>% 
  mutate(date2 = floor_date(date, unit = "day"),
         y = 1) %>% 
  select(-date) %>% 
  distinct()




plot_most_regular_day = data_plot_regular_day %>% 
  ggplot() +
  geom_hline(aes(yintercept = ix), size = 0.1, color = "grey") +
  geom_point(aes(date2, ix), size = 0.2) +
  ggtitle("Most regular users (daily)") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("Date") +
  ylab("User") +
  geom_text(
    data = most_regular_day,
    aes(max(data_plot_regular_day$date2), 
        ix, 
        label = paste0("k=", round(median_k, 1))),
    hjust = -0.1,
    size = 2.5)


plot_least_regular_day = data_plot_irregular_day %>% 
  ggplot() +
  geom_hline(aes(yintercept = ix), size = 0.1, color = "grey") +
  geom_point(aes(date2, ix), size = 0.3) +
  ggtitle("Least regular users (daily)") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("Date") +
  ylab("User") +
  geom_text(
    data = least_regular_day,
    aes(max(data_plot_regular_day$date2), 
        ix, 
        label = paste0("k=", round(median_k, 1))),
    hjust = -0.1,
    size = 2.5)


out_folder = "out/plots/"
dir.create(here(out_folder))
height_cm = 9
width_cm = 25

ggsave(plot = plot_least_regular_day, 
       filename = here(out_folder, "plot_least_regular_day.png"), 
       device = "png",
       width = width_cm, 
       height = height_cm, 
       units = "cm")


ggsave(plot = plot_most_regular_day, 
       filename = here(out_folder, "plot_most_regular_day.png"), 
       device = "png",
       width = width_cm, 
       height = height_cm, 
       units = "cm")








# # Per week
# 
# # Takes like two minutes
# Sys.time()
# draws_week = pggg.mcmc.DrawParameters(
#   cal.cbs = cbs_lastfm_week,
#   mcmc = 1000,
#   burnin = 500,
#   thin = 1,
#   chains = 2,
#   mc.cores = 3
# )
# Sys.time()
# 
# 
# 
# tidy_draws_week = lapply(1:length(draws_week$level_1), function(i){
#   cat("Customer", i, "\n")
#   n_chains = length(draws_week$level_1[[i]])
#   
#   params_cust = lapply(1:n_chains, function(chain){
#     out_chain = draws_week$level_1[[i]][[chain]] %>% 
#       as.data.frame() %>% 
#       mutate(chain = chain) %>% 
#       as_tibble()
#     return(out_chain)
#   }) %>% 
#     bind_rows() %>% 
#     mutate(cust = i)
#   
#   return(params_cust)
# }) %>% 
#   bind_rows()
# 
# 
# 
# summary_k_week = tidy_draws_week %>% 
#   group_by(cust) %>% 
#   summarize(
#     median_k = median(k),
#     k_p10 = as.numeric(quantile(k, 0.1)),
#     k_p90 = as.numeric(quantile(k, 0.9))
#   ) %>% 
#   mutate(usid = cbs_lastfm_week$cust)
# 
# 
# custs_sample = sample(1:nrow(summary_k_week), 10)
# 
# tidy_draws_week %>% 
#   filter(cust %in% custs_sample) %>% 
#   group_by(cust, chain) %>% 
#   mutate(ix = 1:n()) %>% 
#   ungroup() %>% 
#   ggplot() +
#   geom_line(aes(ix, k, group = chain)) +
#   facet_wrap(~cust, scales = "free_y")
# 
# 
# summary_k_week$median_k %>% qplot()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# summary_k_week %>% filter(median_k > 1)
# 
# 
# 
# transactions %>% 
#   filter(cust == "user_000354") %>% 
#   mutate(date2 = as.numeric(
#     paste0(
#       year(date), 
#       str_pad(string = week(date), 
#               width = 2, 
#               pad = "0", 
#               side = "left"))),
#     y = 1) %>% 
#   select(-date) %>% 
#   distinct() %>% 
#   ggplot() +
#   geom_point(aes(date2, y))
# 
# 
# 
# 
# most_regular_week = summary_k_week %>% 
#   top_n(20, median_k) 
# 
# 
# data_plot_regular_week = most_regular_week %>% 
#   arrange(desc(median_k)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   left_join(transactions, by = c("usid" = "cust")) %>% 
#   mutate(date2 = as.numeric(
#     paste0(
#       year(date), 
#       str_pad(string = week(date), 
#               width = 2, 
#               pad = "0", 
#               side = "left"))),
#     y = 1) %>% 
#   select(-date) %>% 
#   distinct()
# 
# 
# data_plot_regular_week %>% 
#   ggplot() +
#   geom_hline(aes(yintercept = ix), size = 0.1, color = "grey") +
#   geom_point(aes(date2, ix), size = 0.1) +
#   ggtitle("Most regular users (week)") +
#   theme(panel.grid = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) +
#   # geom_text(
#   #   data = muestra_depositos %>% 
#   #     select(ix, k_median, clasificacion_k) %>% 
#   #     distinct(),
#   #   aes(min(muestra_depositos$fecha) - months(3), ix, label = round(k_median, 1)),
#   #   hjust = 0.5,
#   #   size = 2.5) +
#   NULL









