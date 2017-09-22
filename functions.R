head_to_head <- function(data) inner_join(data, data, by = "basho") %>% 
  filter(rikishi.x != rikishi.y)

level2char <- function(level) ordered(
  c("Y", "O", "S", "K", "M", "J")[level],
  levels = c("Y", "O", "S", "K", "M", "J")
)

rank2level <- function(rank) ordered(
  substr(as.character(rank), 1, 1),
  levels = c("Y", "O", "S", "K", "M", "J")
)

rank2int <- Vectorize(
  function(rank) ifelse(
    is.na(rank),
    NA,
    as.integer(sprintf(
      "%d%02d%d",
      match(sub("\\d+.*$", "", rank), c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")),
      as.integer(gsub("\\D", "", rank)),
      switch(
        sub("^.+\\d+", "", rank),
        e = 1,
        w = 2,
        3
      )
    ))
  )
)

train <- function(data, model_conf) {
  model <- model_conf$build(
    model_conf$as_input(data),
    model_conf$as_output(data)
  )
  
  model %>% compile(
  	optimizer = "adam",
  	loss = "categorical_crossentropy",
  	metrics = "accuracy"
  )

  model %>% fit(
  	model_conf$as_input(data),
  	model_conf$as_output(data),
  	epochs = 10,
  	callbacks = callback_tensorboard(),
  	validation_split = .2
  )
  
  model
}
