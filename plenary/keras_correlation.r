# Di's attempt with keras, and correlation
library(keras)
library(tidyverse)
library(ash)
library(nullabor)
params <- read_csv("data/parameters.csv")
params0 <- read_csv("data/parameters_0.csv")
paramsb <- read_csv("data/parameters_b.csv")

simdata <- function(par, npixels) {
  x <- rnorm(n=par$n)
  y <- par$beta*x + rnorm(n=par$n, sd=par$sigma)
  #fit <- lm(y~x, data=data.frame(x, y))
  #newdata <- data.frame(x=seq(-4,4,0.1))
  #newdata$y <- predict(fit, newdata)
  i <- bin2(cbind(x, y), nbin=c(npixels, npixels))
  #i <- bin2(rbind(cbind(x, y), cbind(newdata$x, newdata$y)), nbin=c(npixels, npixels))
  i$nc <- ((i$nc - min(i$nc))/(max(i$nc) - min(i$nc)))*255
  return(i)
}

i1 <- simdata(paramsb[1,], npixels)
nimages <- 20000
nimages <- 40000
nimages <- 60000
nimages <- 80000
nimages <- 100000
nimages <- 120000
nimages <- 140000
nimages <- 160000

npixels <- 30
npixels <- 150

nulls <- array(dim=c(nimages, npixels, npixels))
for (i in 1:nimages) {
  i1 <- simdata(params0[i,], npixels)
  nulls[i,,] <- i1$nc
}

linear <- array(dim=c(nimages, npixels, npixels))
for (i in 1:nimages) {
  i1 <- simdata(paramsb[i,], npixels)
  linear[i,,] <- i1$nc
}

nulls <- array_reshape(nulls, c(nrow(nulls), npixels^2))
linear <- array_reshape(linear, c(nrow(linear), npixels^2))
train <- rbind(nulls, linear)
train_y <- to_categorical(c(rep(0, nimages), rep(1, nimages)), 2)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(npixels^2)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  train, train_y,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)
plot(history)

ntestimages <- 10000

nulls <- array(dim=c(ntestimages, npixels, npixels))
for (i in 1:ntestimages) {
  i1 <- simdata(params0[nimages+i,], npixels)
  nulls[i,,] <- i1$nc
}

linear <- array(dim=c(ntestimages, npixels, npixels))
for (i in 1:ntestimages) {
  i1 <- simdata(paramsb[nimages+i,], npixels)
  linear[i,,] <- i1$nc
}

nulls <- array_reshape(nulls, c(nrow(nulls), npixels^2))
linear <- array_reshape(linear, c(nrow(linear), npixels^2))
test <- rbind(nulls, linear)
test_y <- to_categorical(c(rep(0, ntestimages), rep(1, ntestimages)), 2)
model %>% evaluate(test, test_y)
model %>% predict_classes(test) -> test_yp
tb <- table(test_y[,2], test_yp)
(tb[1,1] + tb[2,2])/sum(tb)
tb[1,2]/sum(tb[1,])
tb[2,1]/sum(tb[2,])

training_size_sim <- tibble(
  nimages=c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000),
  acc=c(0.9194, 0.9283, 0.93225, 0.92605, 0.92755, 0.92985, 0.92865, 0.9254),
  class0=c(0.031, 0.017, 0.0179, 0.0073, 0.0087, 0.0098, 0.005, 0.0059),
  class1=c(0.1302, 0.1264, 0.1176, 0.1406, 0.1362, 0.1305, 0.1377, 0.1433))

training_size_sim_50x50 <- tibble(
  nimages=c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000),
  acc=c(0.8949, 0.9054, 0.9225, 0.9228, ),
  class0=c(0.0581, 0.0492, 0.023, 0.0164, ),
  class1=c(0.1521, 0.14, 0.132, 0.138, ))

# Read in Turk data
turk_trials <- read_csv("data/raw_data_turk2.csv")
# De-identify
#turk_trials_ip <- turk_trials %>% select(ip_address) %>%
#  mutate(ip_address_id = as.numeric(factor(ip_address)))
#turk_trials <- turk_trials %>% mutate(ip_address = as.numeric(factor(ip_address)))
#write_csv(turk_trials_ip, "data/turk2_ip_address.csv")
#write_csv(turk_trials, "data/raw_data_turk2.csv")

unique_pics <- turk_trials %>%
  select(pic_name, plot_location, sample_size, beta, sigma) %>%
  distinct() %>%
  separate(pic_name, c("name", "extra"), sep="\\.", remove=FALSE) %>%
  select(-extra) %>%
  mutate(name = str_sub(name, 6, str_length(name)))

#fl <- paste0("data/turk_data2/dat_", unique_pics$name[1], ".txt")
#d <- read_delim(fl, delim=" ")
#d %>% gather(.n, Y, -X) %>%
#  mutate(.n = as.numeric(.n)) %>%
#  ggplot(aes(X, Y)) +
#  geom_point(alpha=0.5) +
#  geom_smooth(method="lm", se=FALSE) +
#  facet_wrap(~.n, ncol=5)

# Check the turk results
# Convert to keras data form
#i <- bin2(cbind(d$X, as.vector(unlist(d[,turk_trials$plot_location[1]+1]))),
#          nbin=c(30, 30))
#i$nc <- ((i$nc - min(i$nc))/(max(i$nc) - min(i$nc)))*255
#turk_i <- array(dim=c(1, 30, 30))
#turk_i[1,,] <- i$nc
#turk_irow <- array_reshape(turk_i, c(nrow(turk_i), npixels^2))
#model %>% predict_classes(turk_irow)

# Predict them all
turk_i <- array(dim=c(nrow(unique_pics), npixels, npixels))
for (i in 1:nrow(unique_pics)) {
  fl <- paste0("data/turk_data2/dat_", unique_pics$name[i], ".txt")
  d <- read_delim(fl, delim=" ")
  img <- bin2(cbind(d$X, as.vector(unlist(d[,unique_pics$plot_location[i]+1]))),
            nbin=c(npixels, npixels))
  img$nc <- ((img$nc - min(img$nc))/(max(img$nc) - min(img$nc)))*255
  turk_i[i,,] <- img$nc
  cat(i, "\n")
}
turk_i <- array_reshape(turk_i, c(nrow(turk_i), npixels^2))

computer_predictions <- unique_pics
computer_predictions <- computer_predictions %>%
  mutate(c_p = model %>% predict_classes(turk_i))

# Human predictions
human_predictions <- turk_trials %>%
  group_by(pic_name) %>%
  summarise(x = length(response[response]),
            K = n(),
            prop = length(response[response])/n())
human_predictions$pval <- NA
for (i in 1:nrow(human_predictions)) {
  human_predictions$pval[i] <- pvisual(human_predictions$x[i], human_predictions$K[i])[2]
  cat(i, human_predictions$pval[i], "\n")
}
human_predictions <- human_predictions %>%
  mutate(h_p = ifelse(pval > 0.05, 0, 1))

predictions <- left_join(computer_predictions, human_predictions, by="pic_name")
predictions %>% count(c_p, h_p)
predictions <- predictions %>%
  mutate(effect = sqrt(sample_size)*abs(beta)/sigma)
#write_csv(predictions, path="data/predictions.csv")
# predictions <- read_csv("data/predictions.csv")

predictions_effect <- predictions %>%
  group_by(effect) %>%
  summarise(computer = sum(c_p)/n(),
            human = sum(h_p)/n()) %>%
  gather(predictor, p, -effect)

ggplot() +
  geom_line(data=ump, aes(x=effect, y=pow), colour="white", size=2) +
  geom_point(data=predictions_effect, aes(x=effect, y=p, colour=predictor), alpha=0.4) +
  geom_smooth(data=predictions_effect, aes(x=effect, y=p, colour=predictor), se=FALSE, span=0.9) +
  scale_color_brewer("", palette="Dark2")

predictions$alpha <- log(.05/0.95)
fit_c <- glm(c_p~offset(alpha)+effect-1, family="binomial", data=predictions)
newdata <- predictions %>% select(effect) %>% distinct() %>%
  mutate(alpha = log(.05/0.95))
pred_c <- predict(fit_c, newdata, type="response")
fit_h <- glm(h_p~offset(alpha)+effect-1, family="binomial", data=predictions)
pred_h <- predict(fit_h, newdata, type="response")
glm_predictions <- tibble(effect=newdata$effect, computer=pred_c, human=pred_h) %>%
  gather(predictor, p, -effect)

ggplot() +
  geom_line(data=ump, aes(x=effect, y=pow), colour="white", size=2) +
  geom_line(data=glm_predictions, aes(x=effect, y=p, colour=predictor), size=1) +
  scale_color_brewer("", palette="Dark2") +
  ylim(c(0,1)) +
  xlab("Effect") +
  ylab("Power")

predictions %>% filter(beta==0) %>%
  select(sample_size, beta, sigma, c_p, h_p)
table(predictions$h_p, predictions$c_p)
predictions %>% filter(h_p == 0) %>%
  select(name, plot_location, sample_size, beta, sigma, c_p, h_p) %>% arrange(beta) %>%
  print(n=50)
predictions %>% filter(c_p != h_p) %>%
  select(name, plot_location, sample_size, beta, sigma, c_p, h_p)
d <- read_delim("data/turk_data2/dat_turk2_100_350_12_1.txt", delim=" ")
ggplot(d, aes(x=X, y=`1`)) + geom_point(alpha=0.5)
