model1_conf <- list(
  as_input = function(data) data %>% 
    transmute(
      level = rank2level(rank),
      position = (as.integer(substr(as.character(rank), 2, nchar(as.character(rank)) - 1)) - 1) * 2 + match(substr(as.character(rank), nchar(as.character(rank)), nchar(as.character(rank))), c("e", "w")),
      w, l
    ) %>% 
    mutate_all(as.integer) %>% 
    as.matrix(),
  
  as_output = function(data) data %>% 
    transmute(y = as.integer(rank2level(new_rank)) - 1) %>% 
    pull(y) %>% 
    to_categorical(num_classes = 6),
  
  build = function(input, output) keras_model_sequential() %>% 
  	layer_dense(
  		units = dim(input)[2] * 100,
  		activation = "relu",
  		input_shape = dim(input)[2]
  	) %>% 
    layer_dropout(rate = .25) %>% 
  	layer_dense(
  		units = dim(input)[2] * 50,
  		activation = "relu"
  	) %>% 
    layer_dropout(rate = .5) %>% 
    layer_dense(
  		units = dim(output)[2],
  		activation = "softmax"
  	)
)
