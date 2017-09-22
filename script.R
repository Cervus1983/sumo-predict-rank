library(keras)
library(tidyverse)


# data ----
df <- readRDS("data.rds")
df_train <- df %>% filter(basho != 201701)
df_test <- df %>% filter(basho == 201701)


# functions & meta data ----
source("functions.R")

ranks_ordered <- df %>% 
  select(rank) %>% 
  unique() %>% 
  mutate(rank_int = rank2int(rank)) %>% 
  arrange(rank_int) %>% 
  pull(rank)


# models ----
source("model1_conf.R")
source("model2_conf.R")

if (TRUE) {
  model1 <- load_model_hdf5("model1.hdf5")
} else {
  model1 <- train(df_train, model1_conf)
  save_model_hdf5(model1, "model1.hdf5")
}

if (TRUE) {
  model2 <- load_model_hdf5("model2.hdf5")
} else {
  model2 <- train(df_train, model2_conf)
  save_model_hdf5(model2, "model2.hdf5")
}


# actual vs prediction ----
df_test %>% 
  # model1 to predict new_level
  cbind(new_level = model1 %>% 
    predict(model1_conf$as_input(df_test)) %>% 
    apply(1, which.max) %>% 
    level2char()
  ) %>% 
  # model2 to predict relative order
  inner_join(
    cbind(
      df_test %>% head_to_head(),
      y = predict(model2, df_test %>% model2_conf$as_input())[,2]
    ) %>% 
      group_by(rikishi = rikishi.x) %>% 
      summarise(y = sum(y))
  ) %>% 
  # infer new_rank_pred from new_level & relative order
  group_by(new_level) %>% 
  mutate(
    y = row_number(-y),
    new_position = paste0((y - 1) %/%2 + 1, c("w", "e")[y %%2 + 1]),
    new_rank_pred = ordered(paste0(new_level, new_position), levels = ranks_ordered)
  ) %>% 
  ungroup() %>% 
  select(-new_level, -new_position, -y) %>% 
  View()
