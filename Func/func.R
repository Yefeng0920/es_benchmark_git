# calculate mode
mode_func <- function(data, indices) {
  d <- data[indices]
  return(LaplacesDemon::Modes(d)$modes[1])
}

mode_func2 <- function(data, indices) {
  d <- data[indices]
  return(LaplacesDemon::Modes(d)$modes[2]) # for the second mode
}

# calculate skewness
skewness_func <- function(data, indices) {
  d <- data[indices]
  return(moments::skewness(d))
}

# calculate kurtosis
kurtosis_func <- function(data, indices) {
  d <- data[indices]
  return(moments::kurtosis(d))
}

# Function calculating mean and variance based on folded distribution
## folded mean
folded_es <- function(mean, variance){   
  mu <- mean
  sigma <- sqrt(variance)
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_mu
}

## folded variance
folded_var <- function(mean, variance){
  mu <- mean
  sigma <- sqrt(variance)
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
  # variance
  fold_v <- fold_se^2
  fold_v
}


# alternatively
foldnorm <-  function(mu, var){
  sd <- sqrt(var)
  postfnorm <- stats::dnorm(mu, 0, sd)*2*sd^2 + mu*(2*stats::pnorm(mu, 0, sd) -1)
  est <- postfnorm
  var.fnorm <- mu^2 + sd^2 - (sd*sqrt(2/pi)*exp((-1*mu^2)/(2*sd^2)) + mu*(1-2*stats::pnorm(-1*mu/sd, 0, 1)))^2
  est <- data.frame(Mean=est, Variance = var.fnorm)
}