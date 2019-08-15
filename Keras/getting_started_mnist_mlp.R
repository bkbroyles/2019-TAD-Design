##Introduction to keras and R studio

#Keras is the interface with tensorflow to build Deep learning models

#install keras package
library(keras)

#load mnist image data set and train a net to classify numbers
mnist <- dataset_mnist()
xtrain <- mnist$train$x
ytrain <- mnist$train$y

xtest <- mnist$test$x
ytest <- mnist$test$y

#images are 3d array (images, height, width)
#flatten width and height into one vector
xtrain <- array_reshape(xtrain, c(nrow(xtrain), 784))
xtest <- array_reshape(xtest, c(nrow(xtest), 784))

xtrain <- xtrain/255
xtest <- xtest/255

#one hot encode labels
ytrain <- to_categorical(ytrain, 10)
ytest <- to_categorical(ytest, 10)

#simple sequential model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

#compile model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

#train
history <- model %>% fit(
  xtrain, ytrain,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

#evaluate performance
model %>% evaluate(xtest, ytest)

#generate predictions
model %>% predict_classes(xtest)











