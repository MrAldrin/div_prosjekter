library(dplyr)
library(readr)
library(zoo)
library(locpol)

# Read in data ----
sec <- read_delim("abelee_data/prices_secondary.csv", delim = " ")
pri <- read_delim("abelee_data/prices_primary.csv", delim = " ")

# functions ----
add_mid_price <- function(df) {
  df %>% mutate(mid_price = .5 * (bid_price + offer_price))
}

add_log_return <- function(df) {
  # df %>% mutate(log_return = c(NA, diff(log(df$mid_price)) - mean(diff(log(df$mid_price)))))
  df <- df %>% mutate(log_return = NA)
  for (i in 2:nrow(df)) {
    df$log_return[i] <- log(df$mid_price[i]) - log(df$mid_price[i-1])
  }
  # df <- df %>% mutate(log_return = c(NA, diff(log(df$mid_price)) - mean(diff(log(df$mid_price)))))
  return(df)
}

plot_bid_offer <- function(df) {
  plot(df$datetime, df$bid_price, type = "l", col = "blue", xlab = "Time", ylab = "Price", main = deparse(substitute(df)))
  lines(df$datetime, df$offer_price, type = "l", col = "red")
  legend("topright", legend = c("Bid Price", "Offer Price"), col = c("blue", "red"), lty = 1)
}

time_diff <- function(df) {
  df <- df %>% mutate(time_diff = c(NA, diff(df$timestamp)))
}

stable_time <- function(df) {
  # Transforms from nanosecounds to milliseconds
  df$timestamp <- (df$timestamp-df$timestamp[1])/1e6
  return(df)
}

# Initial inspection ----
plot_bid_offer(sec)
plot_bid_offer(pri)

# Run ----
sec <- add_mid_price(sec)
pri <- add_mid_price(pri)

sec <- stable_time(sec)
pri <- stable_time(pri)

sec <- time_diff(sec)
pri <- time_diff(pri)

t2 <- add_log_return(sec)
t <- add_log_return(sec[seq(1, 200,1),])

t <- t %>% mutate(log_return = NA)
# Testing ----
plot_price_avg_time <- function(df, window_size = 50) {
  rolling_avg_timestamps <- df$timestamp[window_size:length(df$timestamp)]
  rolling_avg <- rollapply(df$time_diff, width = window_size, FUN = mean, align = "right")
  par(mar = c(5, 4, 4, 5))
  plot(df$timestamp, df$mid_price, type = "l")
  par(new = TRUE)
  plot(rolling_avg_timestamps, rolling_avg, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red")
  axis(side=4, col = "red", col.axis="red")
  mtext("Time difference (rolling average)", side=4, line=3, col = "red")
}
plot_price_avg_time(pri)
plot_price_avg_time(sec)
test_data <- pri[1:100,]
test_data$timestamp <- (test_data$timestamp-test_data$timestamp[1])/1e6
t <- locpol(mid_price ~ timestamp, data = as.data.frame(pri[1:100,]), deg = 1, bw = NULL)
theta <- .9999
lm_1 <- lm(mid_price ~ timestamp, data = test_data, weights = theta^(test_data$timestamp[nrow(test_data)]-test_data$timestamp))
test$timestamp[1:100]

df_lagged <- pri %>%
  mutate(lag1 = lag(mid_price, 1),
         lag2 = lag(mid_price, 2),
         lag3 = lag(mid_price, 3),
         lag4 = lag(mid_price, 4),
         lag5 = lag(mid_price, 5),
         lag6 = lag(mid_price, 6),
         lag7 = lag(mid_price, 7),
         lag8 = lag(mid_price, 8),
         lag9 = lag(mid_price, 9),
         lag10 = lag(mid_price, 10),
         
  ) %>%
  slice(11:n())
min(sec$time_diff[-1])

plot(pri$timestamp, pri$time_diff)
plot(sec$time_diff)
t <- sec[seq(1, 200,1),]
plot(sec$time_diff[1:20000],sec$mid_price[1:20000])
plot(sec$time_diff[seq(1,200)])
histsec <- hist(sec$time_diff[1:100000])
histpri <- hist(pri$time_diff[1:100000])
plot(histsec, col = "blue")
lines(histpri, col = rgb(1, 0, 0, 0.5))
test_pri <- pri[seq(1, nrow(pri),10),]
device.off()
graphics.off()
plot(test_sec$datetime, test_sec$log_return, type = "l", col = "black", xlab = "Time", ylab = "Price")
par(mfrow = c(1,1))


plot(test_sec$datetime, test_sec$mid_price, type = "l")
lines(test_pri$datetime, test_pri$mid_price, type = "l", col = "red")
df <- df %>% mutate(log_return = c(NA, diff(log(df$mid_price)) - mean(diff(log(df$mid_price)))))

t <- diff(log(test_sec$mid_price))
test_sec <- test_sec %>% mutate(log_return = diff(log(test_sec)))
t <- add_log_return(t)
t <- test_sec %>% mutate(log_returns = c(NA, diff(log(test_sec$mid_price))))
t <- test_sec %>% mutate(log_returns = c(NA, diff(log(test_sec$mid_price)) - mean(diff(log(test_sec$mid_price)))))
mean(t$log_returns[-1])
plot(t$datetime[-1],t$log_returns[-1], type= "l")
diff(log(test_sec$mid_price))

for (i in 2:nrow(df)) {
  df$log_return[i] <- log(df$mid_price[i]) - log(df$mid_price[i-1])
}

for (i in 2:5) {
  print(log(t$mid_price[i]) - log(t$mid_price[i-1]))
}

s1 <- sec[1:6000,]
p1 <- pri[1:1000,]
plot(s1$timestamp, s1$mid_price, type = "l")
lines(p1$timestamp, p1$mid_price, col = "red")

# lm(mid_price ~ timestamp + )
# 
# y | 
# pm1
# pm2
# pm3


# Local regression ----

# Cut data
cut_data <- function(df, start, n_width) {
  end <- start + n_width -1
  df_cut <- df[start:end, c("timestamp", "mid_price")]
  return(df_cut)
}
future_data <- function(df){
  future_points <- df[(end+1):(end+n_future), c("timestamp", "mid_price", "time_diff")]
  future_points$timestamp <- future_points$timestamp - df_train$timestamp[length(df_train$timestamp)]
  return(list(df_train, future_points))
}

# cut_data <- function(df, start, n_width, n_future) {
## Legacy
#   end <- start + n_width -1
#   df_train <- df[start:end,]
#   future_points <- df[(end+1):(end+n_future), c("timestamp", "mid_price", "time_diff")]
#   future_points$timestamp <- future_points$timestamp - df_train$timestamp[length(df_train$timestamp)]
#   return(list(df_train, future_points))
# }

fit_lm <- function(df, weights) {
  lm_fit <- lm(mid_price ~ timestamp, data = df, weights = weights)
  return(lm_fit)
}

# Local regression
pipeline <- function(df, start, n_width, n_future, theta, factor){
  # Updated version
  # Cut data
  sec_data <- cut_data(sec, start = start, n_width = n_width)
  sec_future <- cut_data(sec, start = (start+n_width), n_width = n_future)
  start_time <- sec_data$timestamp[1] # First time stamp of series
  current_time <- sec_data$timestamp[n_width] # Current time stamp
  sec_future$timestamp <- sec_future$timestamp - current_time # Time relative to current time
  
  pri_rows <- which(pri$timestamp > start_time & pri$timestamp < current_time) # rows in pri within same time frame
  pri_data <- pri[pri_rows, c("timestamp", "mid_price")]
  data_train <- rbind(sec_data, pri_data)
  # Weights
  sec_weights <- theta^(sec_data$timestamp[n_width]-sec_data$timestamp)
  pri_weights <- factor*theta^(pri_data$timestamp[length(pri_rows)]-pri_data$timestamp)
  weights <- c(sec_weights, pri_weights)
  # Fit model
  lm_fit <- fit_lm(data_train, weights)
  # Predict
  pred <- predict(lm_fit, newdata = sec_future)
  # Get error
  sec_future$error <- sec_future$mid_price - pred
  return(list(data_train,sec_future))
}

theta_factor_tune <- function(theta, factor) {
  start <- 1
  n_width <- 100
  n_future <- 50
  theta <- theta
  factor <- factor
  n <- nrow(pri)
  runs <- n%/%(n_width+n_future)
  time_all <- numeric(runs*n_future)
  error_all <- numeric(runs*n_future)
  j <- 1
  for (i in (seq(1, n - n_width, n_width))) {
    data_used_and_future <- pipeline(pri, start = i, n_width = n_width, n_future = n_future, theta = theta, factor = factor)
    time_all[j:(j+n_future-1)] <- data_used_and_future[[2]]$timestamp
    error_all[j:(j+n_future-1)] <- data_used_and_future[[2]]$error
    j <- j + n_future
  }
  
  rows_short <- which(time_all > 10 & time_all < 100)
  rows_long <- which(time_all > 10000)
  error_short <- error_all[rows_short]
  error_long <- error_all[rows_long]
  length(rows_short)
  length(rows_long)
  RMSE_short <- sqrt(mean(error_short^2))
  RMSE_long <- sqrt(mean(error_long^2))
  return(list(RMSE_short, RMSE_long))
}

RMSE <- theta_factor_tune(theta = .999, factor = 1)

theta <- c(.9999 , .99999, .999999)
factor <- c(1, 5, 15, 30)
result_matrix_short <- matrix(NA, nrow = length(theta), ncol = length(factor))
rownames(result_matrix_short) <- theta
colnames(result_matrix_short) <- factor
result_matrix_long <- matrix(NA, nrow = length(theta), ncol = length(factor))
rownames(result_matrix_long) <- theta
colnames(result_matrix_long) <- factor
for (i in 1:length(theta)) {
  for (j in 1:length(factor)) {
    RMSE <- theta_factor_tune(theta[i], factor[j])
    RMSE_short <- RMSE[[1]]
    RMSE_long <- RMSE[[2]]
    result_matrix_short[i,j] <- RMSE_short
    result_matrix_long[i,j] <- RMSE_long
  }
}

# Parameters
start <- 1
n_width <- 100
n_future <- 10
# Cut data
data <- cut_data(sec, start, n_width, n_future)
data_train <- data[[1]]
future_data <- data[[2]]

# Weights
theta <- .9999
weights <- theta^(data_train$timestamp[n_width]-data_train$timestamp)

# Fit
lm_fit <- fit_lm(data_train, weights)

# Predict
pred <- predict(lm_fit, newdata = future_data)

# Get error
future_data$error <- future_data$mid_price - pred


plot(data_train$timestamp, data_train$mid_price, type = "l", col = "red", xlim = c(0,3e5))
lines(future_data$timestamp, pred, col = "blue")
lines(future_data$timestamp, future_data$mid_price, col = "green")

# Legacy ----
pipeline <- function(df, start, n_width, n_future, theta, factor){
  # Cut data
  data <- cut_data(df, start, n_width, n_future)
  data_train <- data[[1]]
  future_data <- data[[2]]
  # Weights
  weights = theta^(data_train$timestamp[n_width]-data_train$timestamp)
  # Fit model
  lm_fit <- fit_lm(data_train, weights)
  # Predict
  pred <- predict(lm_fit, newdata = future_data)
  # Get error
  future_data$error <- future_data$mid_price - pred
  return(list(data_train,future_data))
}