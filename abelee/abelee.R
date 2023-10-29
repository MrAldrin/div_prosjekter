library(dplyr)
library(readr)

# Read in data ----
sec <- read_delim("abelee_data/prices_secondary.csv", delim = " ")
pri <- read_delim("abelee_data/prices_secondary.csv", delim = " ")

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

# Initial inspection ----
plot_bid_offer(sec)
plot_bid_offer(pri)

# Run ----
sec <- add_mid_price(sec)
sec <- time_diff(sec)
pri <- add_mid_price(pri)
pri <- time_diff(pri)
t2 <- add_log_return(sec)
t <- add_log_return(sec[seq(1, 200,1),])

t <- t %>% mutate(log_return = NA)
# Testing ----
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
