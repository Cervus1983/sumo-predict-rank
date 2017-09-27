# functions & meta data ----
source("functions.R")


# all banzuke since 1991 ----
banzuke <- do.call(
  rbind,
  lapply(
    grep(
      "^\\d{6}\\.csv$",
      list.files("banzuke"),
      value = TRUE
    ),
    function(filename) read_csv(paste("banzuke", filename, sep = "/")) %>% 
      mutate(basho = substr(filename, 1, 6))
  )
) %>% 
  # haridashi & tsukedashi (can be ignored for our purposes)
  mutate(
    rank = gsub("HD$", "", rank),
    rank = gsub("TD$", "", rank)
  )


# order ranks ----
ranks_ordered <- banzuke %>% 
  select(rank) %>% 
  unique() %>% 
  mutate(rank_int = rank2int(rank)) %>% 
  arrange(rank_int) %>% 
  pull(rank)


# main data set ----
data <- banzuke %>% 
  # add column: next_basho
  inner_join(
    .,
    tibble(
      basho = c(NA, unique(banzuke$basho)),
      next_basho = c(unique(banzuke$basho), NA)
    )
  ) %>% 
  # add column: new_rank
  left_join(
    .,
    banzuke %>% select(id, new_rank = rank, next_basho = basho)
  ) %>% 
  # order ranks
  mutate(
    rank = ordered(rank, levels = ranks_ordered),
    new_rank = ordered(new_rank, levels = ranks_ordered)
  ) %>% 
  # remove retirements & lower divisions
  filter(
    complete.cases(.),
    rank < "J1e"
  ) %>% 
  # relevant columns
  select(basho, rikishi, rank, w, l, new_rank)
