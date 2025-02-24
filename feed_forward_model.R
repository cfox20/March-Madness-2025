
# Load Packages -----------------------------------------------------------
library("torch")
library("torchvision")
library("luz")
library("tidyverse")
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5))


# Set Device --------------------------------------------------------------

# Set the device to GPU if available
device <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")
# device <- torch_device("cpu")


# Setup Dataloader --------------------------------------------------------

data <- read_rds("data//ff_mod_data.rds")


train <- data$train_data |> drop_na()
valid <- data$valid_data |> drop_na()
# test <- data$test_data |> drop_na()

# Center and scale data using the values from the training set
train_means <- train |>
  select(6:ncol(train)) |>
  colMeans() |>
  as.numeric()

train_means[1:65] <- 0
train_means[148:217] <- 0

train_sd <- train |>
  select(6:ncol(train)) |>
  summarise(across(everything(), sd)) |>
  as.numeric()


train_sd[1:65] <- 1
train_sd[148:217] <- 1

# write_rds(list(train_means = train_means, train_sd = train_sd), "data//train_scaling.rds")

center_scale <- function(tb, means, sd) {
  t((t(tb) - means) / sd)
}



ff_march_madness_ds <- dataset(
  name = "Game Prediction",
  initialize = function(tb) {
    self$x <- as.matrix(tb[, 6:ncol(tb)]) |>
                center_scale(train_means, train_sd) %>%
                # cbind(as.matrix(tb[,6]), .) |>
                torch_tensor(dtype = torch_float32(), device = device)
    self$y <- torch_tensor(as.numeric(tb$w_team == "a"),
                           dtype = torch_float(),
                           device = device)$unsqueeze(2)
  },
  .getitem = function(i) {
    list(x = self$x[i,], y = self$y[i])
  },
  .length = function() {
    dim(self$x)[1]
  }
)

# Create datasets for train, validation, and test
train_ds <- ff_march_madness_ds(train)
valid_ds <- ff_march_madness_ds(valid)
# test_ds <- ff_march_madness_ds(test)

train_dl <- dataloader(train_ds, batch_size = 200, shuffle = TRUE)
valid_dl <- dataloader(valid_ds, batch_size = 200)
# test_dl <- dataloader(test_ds, batch_size = length(test_ds))



# Setup Model -------------------------------------------------------------


# Create the feed forward nn model
n_preds <- ncol(train_ds$x)

ff_net <- nn_module(
  initialize = function() {
   self$linear1 <- nn_linear(n_preds, 256)
   self$linear2 <- nn_linear(256, 128)
   # self$linear3 <- nn_linear(150, 100)
   self$output <- nn_linear(128, 1)
   # self$linear5 <- nn_linear(50, 1)

   self$norm1 <- nn_batch_norm1d(256)
   self$norm2 <- nn_batch_norm1d(128)

   self$dropout <- nn_dropout(p = .1)

   self$relu <- nn_relu()
  },
  forward = function(x) {
    x |>
      self$linear1() |>
      self$norm1() |>
      self$relu() |>
      self$dropout() |>
      self$linear2() |>
      self$norm2() |>
      self$relu() |>
      self$dropout() |>
      self$output()
  }
)



# Luz

mod <- ff_net |>
  setup(
    loss = nn_bce_with_logits_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_binary_accuracy()
    )
  )


# rate_and_losses <- mod |> lr_finder(train_dl)
# plot(rate_and_losses)

fitted <- mod |>
  fit(
    train_dl,
    epochs = 50,
    valid_data = valid_dl,
    verbose = 1,
    callbacks = list(
      luz_callback_early_stopping(patience = 10)
    )
  )

# luz_save(fitted, "models//mens.rds")
# fitted <- luz_load("models//basic_class.rds")




# Womens' Model -----------------------------------------------------------



# Setup Dataloader --------------------------------------------------------

data <- read_rds("data//w_ff_mod_data.rds")


train <- data$train_data |> drop_na()
valid <- data$valid_data |> drop_na()
# test <- data$test_data |> drop_na()

# Center and scale data using the values from the training set
train_means <- train |>
  select(6:ncol(train)) |>
  colMeans() |>
  as.numeric()


train_means[1:64] <- 0
train_means[132:195] <- 0

train_sd <- train |>
  select(6:ncol(train)) |>
  summarise(across(everything(), sd)) |>
  as.numeric()


train_sd[1:64] <- 1
train_sd[132:195] <- 1

write_rds(list(train_means = train_means, train_sd = train_sd), "data//w_train_scaling.rds")

center_scale <- function(tb, means, sd) {
  t((t(tb) - means) / sd)
}



w_ff_march_madness_ds <- dataset(
  name = "Game Prediction",
  initialize = function(tb) {
    self$x <- as.matrix(tb[, 6:ncol(tb)]) |>
      center_scale(train_means, train_sd) %>%
      # cbind(as.matrix(tb[,6]), .) |>
      torch_tensor(dtype = torch_float32(), device = device)
    self$y <- torch_tensor(as.numeric(tb$w_team == "a"),
                           dtype = torch_float(),
                           device = device)$unsqueeze(2)
  },
  .getitem = function(i) {
    list(x = self$x[i,], y = self$y[i])
  },
  .length = function() {
    dim(self$x)[1]
  }
)

# Create datasets for train, validation, and test
train_ds <- ff_march_madness_ds(train)
valid_ds <- ff_march_madness_ds(valid)
# test_ds <- ff_march_madness_ds(test)

train_dl <- dataloader(train_ds, batch_size = 200, shuffle = TRUE)
valid_dl <- dataloader(valid_ds, batch_size = 200)
# test_dl <- dataloader(test_ds, batch_size = length(test_ds))



# Setup Model -------------------------------------------------------------


# Create the feed forward nn model
n_preds <- ncol(train_ds$x)

ff_net <- nn_module(
  initialize = function() {
    self$linear1 <- nn_linear(n_preds, 300)
    self$linear2 <- nn_linear(300, 150)
    self$linear3 <- nn_linear(150, 100)
    self$linear4 <- nn_linear(100, 50)
    self$linear5 <- nn_linear(50, 1)
    # self$linear5 <- nn_linear(50, 1)

    self$norm1 <- nn_batch_norm1d(300)
    self$norm2 <- nn_batch_norm1d(150)
    self$norm3 <- nn_batch_norm1d(100)
    self$norm4 <- nn_batch_norm1d(50)
    # self$norm4 <- nn_batch_norm1d(50)

    self$dropout <- nn_dropout()

    self$relu <- nn_relu()
  },
  forward = function(x) {
    x |>
      self$linear1() |>
      self$norm1() |>
      self$relu() |>
      self$dropout() |>
      self$linear2() |>
      self$norm2() |>
      self$relu() |>
      self$dropout() |>
      self$linear3() |>
      self$norm3() |>
      self$relu() |>
      self$dropout() |>
      self$linear4() |>
      self$norm4() |>
      self$relu() |>
      self$dropout() |>
      self$linear5() |>
      # self$norm4() |>
      # self$relu() |>
      # self$dropout() |>
      # self$linear5() |>
      nnf_sigmoid()
  }
)



# Luz

mod <- ff_net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_binary_accuracy()
    )
  )


# rate_and_losses <- mod |> lr_finder(train_dl)
# plot(rate_and_losses)

fitted <- mod |>
  fit(
    train_dl,
    epochs = 50,
    valid_data = valid_dl,
    verbose = 1,
    callbacks = list(
      luz_callback_early_stopping(patience = 3)
    )
  )


luz_save(fitted, "models//womens.rds")
# fitted <- luz_load("models//basic_class.rds")










