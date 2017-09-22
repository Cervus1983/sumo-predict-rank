model2_conf <- list(
  as_input = function(data) data %>% 
    head_to_head() %>% 
    transmute(
      rank.x, w.x, l.x,
      rank.y, w.y, l.y
    ) %>% 
    mutate_all(as.integer) %>% 
    as.matrix(),
  
  as_output = function(data) data %>% 
    head_to_head() %>% 
    transmute(y = as.integer(new_rank.x < new_rank.y)) %>% 
    pull(y) %>% 
    to_categorical(num_classes = 2),
  
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
