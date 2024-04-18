suppressPackageStartupMessages(require(tidyverse))

spain_wine_ratings <- read_csv("C:/Users/ranen/Downloads/wines_SPA.csv")

## We have enough full observations to not worry about omitting missing values
spain_wine_ratings <- na.omit(spain_wine_ratings)

response <- c("rating")

rating <- select(spain_wine_ratings, all_of(response))

## standardize columns
acidity_mean <- mean(spain_wine_ratings$acidity)
acidity_sd <- sd(spain_wine_ratings$acidity)
body_mean <- mean(spain_wine_ratings$body)
body_sd <- sd(spain_wine_ratings$body)
price_mean <- mean(spain_wine_ratings$price)
price_sd <- sd(spain_wine_ratings$price)
rating_mean <- mean(rating$rating)

rating <- rating %>%
  mutate(rating = rating-rating_mean)

spain_wine_ratings <- spain_wine_ratings %>%
  select(c(acidity, body, price)) %>%
  mutate(acidity = (acidity - acidity_mean)/acidity_sd) %>%
  mutate(body = (body - body_mean)/body_sd) %>%
  mutate(price = (price - price_mean)/price_sd) 


body_vec <- spain_wine_ratings$body
acidity_vec <- spain_wine_ratings$acidity
price_vec <- spain_wine_ratings$price
rating_vec <- rating$rating

## Prior Predictive check

set.seed(1)
intercepts <- rnorm(100,0,0.3)
bodys <- rnorm(100,0,1)
aciditys <- rnorm(100,0,1)
prices <- rnorm(100,0,1)

## x-values
body_vals <- sort(unique(body_vec))
acidity_vals <- sort(unique(acidity_vec))
price_vals <- sort(unique(price_vec))

plot(1, type = "n", xlab = "body", 
     ylab = "rating", xlim = c(min(body_vals), max(body_vals)),  
     ylim = c(-1, 1)) 
for(i in (1:100)){
  lines(x = body_vals, y = bodys[i]*body_vals+intercepts[i], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
}


plot(1, type = "n", xlab = "acidity", 
     ylab = "rating", xlim = c(min(acidity_vals), max(acidity_vals)),  
     ylim = c(-1, 1)) 
for(i in (1:100)){
  lines(x = acidity_vals, y = aciditys[i]*acidity_vals+intercepts[i], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
}

plot(1, type = "n", xlab = "price", 
     ylab = "rating", xlim = c(min(price_vals), max(price_vals)),  
     ylim = c(-1, 1)) 
for(i in (1:100)){
  lines(x = price_vals, y = prices[i]*price_vals+intercepts[i], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
}


## update priors
bodys_updated <- rnorm(100, mean = 0, 0.1)
aciditys_updated <- rnorm(100, mean = 0, 0.1)
prices_updated <- rnorm(100, mean = 0, 0.1)

plot(1, type = "n", xlab = "body", 
     ylab = "rating", xlim = c(min(body_vals), max(body_vals)),  
     ylim = c(-1, 1)) 
for(i in (1:100)){
  lines(x = body_vals, y = bodys_updated[i]*body_vals+intercepts[i], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
}


plot(1, type = "n", xlab = "acidity", 
     ylab = "rating", xlim = c(min(acidity_vals), max(acidity_vals)),  
     ylim = c(-1, 1)) 
for(i in (1:100)){
  lines(x = acidity_vals, y = aciditys_updated[i]*acidity_vals+intercepts[i], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
}

plot(1, type = "n", xlab = "price", 
     ylab = "rating", xlim = c(min(price_vals), max(price_vals)),  
     ylim = c(-1, 1)) 
for(i in (1:100)){
  lines(x = price_vals, y = prices_updated[i]*price_vals+intercepts[i], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
}
 


suppressPackageStartupMessages(require(rstan))

fit = stan(
  "wine.stan", 
  seed = 1,
  data = list(
    y = rating_vec,
    body = body_vec,
    acidity = acidity_vec,
    price = price_vec,
    N = length(rating_vec)
  ), 
  chains = 2,
  iter = 10000      
)

suppressPackageStartupMessages(require(bayesplot))

# Trace plots
mcmc_trace(fit, pars = c("b1")) + theme_minimal()
mcmc_trace(fit, pars = c("b2")) + theme_minimal()
mcmc_trace(fit, pars = c("b3")) + theme_minimal()

# rank plots
mcmc_rank_hist(fit, pars = c("b1")) + theme_minimal()
mcmc_rank_hist(fit, pars = c("b2")) + theme_minimal()
mcmc_rank_hist(fit, pars = c("b3")) + theme_minimal()

## Posterior Predictive

##randomly choose an observation
n = length(spain_wine_ratings$body)

## slightly smaller than 10% of observations used in validation set
validation_indices <- round(runif(0.1*n,-0.5, n+0.5))
validation_indices <- unique(validation_indices)

spain_wine_train <- filter(spain_wine_ratings, !row_number() %in% validation_indices)
spain_wine_valid <- spain_wine_ratings[validation_indices,]

rating_train <- filter(rating, !row_number() %in% validation_indices)
rating_valid <- rating[validation_indices,]

rating_train_vec <- rating_train$rating
body_train_vec <- spain_wine_train$body
body_valid_vec <- spain_wine_valid$body
acidity_train_vec <- spain_wine_train$acidity
acidity_valid_vec <- spain_wine_valid$acidity
price_train_vec <- spain_wine_train$price
price_valid_vec <- spain_wine_valid$price

calibration_fit <- stan(
  "wine.stan", 
  seed = 1,
  data = list(
    y = rating_train_vec,
    body = body_train_vec,
    acidity = acidity_train_vec,
    price = price_train_vec,
    N = length(rating_train_vec),
    N_valid = length(body_valid_vec),
    body_pred = body_valid_vec,
    acidity_pred = acidity_valid_vec,
    price_pred = price_valid_vec
  ), 
  chains = 2,
  iter = 10000      
)

calibration_quantiles <- summary(calibration_fit, probs = c(0.025, 0.975))$summary
calibration_quantiles <- calibration_quantiles[,c("2.5%", "97.5%")]
calibration_quantiles <- 
  calibration_quantiles[!rownames(calibration_quantiles) %in% 
                          c("b1", "b2", "b3", "intercept", "sigma", "lp__"),]

## Now calculate proportion of ratings in credible interval

inside <- 0
n_valid <- nrow(rating_valid)
for  (i in 1:n_valid) {
  if(rating_valid[i,] > calibration_quantiles[i,1] && 
     rating_valid[i,] < calibration_quantiles[i,2]) {
    inside <- inside + 1 
  }
}

calibration_tib <- 
  tibble("rating" = rating_valid$rating, 
         "lower bound" = calibration_quantiles[,1], 
         "upper bound" = calibration_quantiles[,2])

ggplot() +
  geom_point(aes(x = seq(1,25), y=rating_valid$rating[1:25])) +
  geom_point(aes(x = seq(1,25), y = calibration_tib$`lower bound`[1:25]), colour = "red") +
  geom_point(aes(x = seq(1,25), y = calibration_tib$`upper bound`[1:25]), colour = "blue") +
  labs(y = "rating",x = "") +
  theme_minimal()

## Fit to all data

full_fit <- stan(
  "wine.stan", 
  seed = 1,
  data = list(
    y = rating_vec,
    body = body_vec,
    acidity = acidity_vec,
    price = price_vec,
    N = length(rating_vec),
    N_valid = length(body_vec),
    body_pred = body_vec,
    acidity_pred = acidity_vec,
    price_pred = price_vec
  ), 
  chains = 2,
  iter = 10000      
)

full_quantiles <- summary(full_fit)$summary
preds <- full_quantiles[,"mean"]
preds <- preds[6:6334]

## Bayes prediction mean MSE
stopifnot(length(preds)==length(rating_vec))

bayes_mse = sum((rating_vec - preds)**2)/length(preds)
  
  