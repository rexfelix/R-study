library(tensorflow)
library(keras)

gpu <- tf$config$experimental$get_visible_devices('GPU')[[1]]
tf$config$experimental$set_memory_growth(device = gpu, enable = TRUE)

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train, c(dim(x_train),1))
x_test <- array_reshape(x_test, c(dim(x_test),1))
y_train <- to_categorical(y_train,10)
y_test <- to_categorical(y_test,10)

model <- keras_model_sequential()

model %>% layer_conv_2d(padding = 'same', 
                        filters = 64,
                        kernel_size = c(4,4),
                        strides = c(2,2),
                        activation = 'relu', 
                        input_shape = dim(x_train)[2:4]) %>% 
  layer_max_pooling_2d(pool_size = c(2,2),
                       strides = c(2,2),
                       padding = 'same') %>% 
  layer_conv_2d(padding = 'same', 
                filters = 128,
                kernel_size = c(4,4),
                strides = c(2,2),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2),
                       strides = c(2,2),
                       padding = 'same') %>% 
  layer_conv_2d(padding = 'same', 
                filters = 32,
                kernel_size = c(4,4),
                strides = c(2,2),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2),
                       strides = c(2,2),
                       padding = 'same') %>% 
  layer_flatten() %>% 
  layer_dense(10,activation = 'softmax')

model %>% compile(loss='categorical_crossentropy', optimizer=optimizer_rmsprop(), metrics=c('accuracy'))

summary(model)

model %>% 
  fit(x_train,y_train, epochs=5, batch_size=32, validation_split=0.2)

model %>% evaluate(x_test,y_test)
pred <- model %>% predict_classes(x_test)
pred <- to_categorical(pred)
(score <- mean(pred==y_test))
# 0.9954 at epochs=6
# 0.9965 at epochs=5