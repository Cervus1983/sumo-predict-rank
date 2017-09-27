library(keras)
library(tidyverse)


# data & functions ----
source("data.R")

data_train <- data %>% filter(basho != 201707)
data_test <- data %>% filter(basho == 201707)


# models ----
source("model1_conf.R")
source("model2_conf.R")

if (TRUE) {
  model1 <- load_model_hdf5("model1.hdf5")
} else {
  model1 <- train(data_train, model1_conf)
  save_model_hdf5(model1, "model1.hdf5")
}

if (TRUE) {
  model2 <- load_model_hdf5("model2.hdf5")
} else {
  model2 <- train(data_train, model2_conf)
  save_model_hdf5(model2, "model2.hdf5")
}


# actual vs prediction ----
df <- data_test %>% 
  # model1 to predict new_level
  cbind(new_level = model1 %>% 
    predict(model1_conf$as_input(data_test)) %>% 
    apply(1, which.max) %>% 
    level2char()
  ) %>% 
  # model2 to predict relative order
  inner_join(
    cbind(
      data_test %>% head_to_head(),
      y = predict(model2, data_test %>% model2_conf$as_input())[,2]
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
  select(-new_level, -new_position, -y)


# visualise ----
library(plotly)

df %>% 
  mutate(new_rank_pred = ordered(
    ifelse(new_rank_pred < "J1e", as.character(new_rank_pred), as.character(new_rank)),
    levels = ranks_ordered
  )) %>% 
  plot_ly(
    color = ~rank2level(rank),
    colors = "Set1",
    x = ~new_rank,
    y = ~rank
  ) %>% 
  add_markers() %>% 
  add_segments(
    xend = ~new_rank_pred,
    yend = ~rank
  ) %>% 
  layout(
    showlegend = FALSE,
    xaxis = list(
      showticklabels = FALSE,
      title = "New rank"
    ),
    yaxis = list(
      autorange = "reversed",
      title = "Old rank"
    )
  )
