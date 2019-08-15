###Neural Net based on amino acid class
library(keras)

#which library to use
lib <- random_library

#split into test and train set
split <- build_train_split(lib)

train_sample <- split[[1]]
test_sample <- split[[2]]

#Split train_sample into features and labels
train_x <- build_class_array(train_sample)

train_y <- (train_sample$binary_stop %>% as.numeric() - 1) %>% 
                to_categorical()

# Defining Model ----------------------------------------------------------
model <- keras_model_sequential()
model %>% 
  layer_flatten(input_shape = c(20,6)) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = 50, activation = 'relu') %>% 
  layer_dense(units = 2, activation = 'softmax')



model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Training ------------------------------------------------------------------

model %>% fit(
  train_x, train_y,
  batch_size = 32,
  epochs = 30, 
  validation_split = 0.2
)

summary(model)












