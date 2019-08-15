##More complicated prediction with fashion data
fashion_mnist <- dataset_fashion_mnist()

c(xtrain, ytrain) %<-% fashion_mnist$train
c(xtest, ytest) %<-% fashion_mnist$test

#remake class names since they aren't stored in the dataset
class_names <- c('T-shirt', 'Trouser', 'Pullover',
                 'Dress', 'Coat', 'Sandal',
                 'Shirt', 'Sneaker', 'Bag', 'Ankle boot')

#explore data a little
dim(xtrain)
dim(ytrain)

ytrain[1:20]

#Data Preprocessing
library(ggplot2)

image1 <- as.data.frame(xtrain[1,,])
colnames(image1) <- seq_len(ncol(image1))
image1$y <- seq_len(nrow(image1))
image1 <- gather(image1, 'x', 'value', -y)
image1$x <- as.integer(image1$x)

ggplot(image1, aes(x, y, fill = value))+
  geom_tile()+
  scale_fill_gradient(low = 'white', high = 'black', na.value = NA)+
  scale_y_reverse()+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  theme(aspect.ratio = 1)+
  xlab('')+
  ylab('')

#normalize greyscale values
xtrain <- xtrain/255
xtest <- xtest/255

##Display 25 entries with name
par(mfcol = c(5,5))
par(mar = c(0, 0, 1.5, 0), xaxs = 'i', yaxs = 'i')
for(i in 1:25){
  img <- xtrain[i,,]
  img <- t(apply(img, 2, rev))
  image(1:28, 1:28, img, col = grey((0:255)/255), xaxt = 'n',
        yaxt = 'n', main = paste(class_names[ytrain[i] + 1]))
}

#build model
model <- keras_model_sequential() 
model %>% 
  layer_flatten(input_shape = c(28,28)) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = 10, activation = 'softmax')

#compile model
model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

#train model
model %>% fit(xtrain, ytrain, epochs = 5)
