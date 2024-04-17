library(tidyverse)
library(glmnet)
library(stats)

spain_wine_ratings <- read_csv("C:/Users/ranen/Downloads/wines_SPA.csv")

head(spain_wine_ratings)

## Lasso

## We have enough full observations to not worry about omitting missing values
spain_wine_ratings <- na.omit(spain_wine_ratings)

response <- c("rating")

rating <- select(spain_wine_ratings, all_of(response))

## Transform Tibble to be compatible with glmnet 
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




X <- as.matrix(spain_wine_ratings)
y <- as.matrix(rating)

fit <- glmnet(X, y)

plot(fit)

cvfit <- cv.glmnet(X,y)

plot(cvfit)

coef(cvfit, s = cvfit$lambda.1se)
coef(cvfit, s = cvfit$lambda.min)

## Pre-processing for STAN

body_vec <- spain_wine_ratings$body
acidity_vec <- spain_wine_ratings$acidity
price_vec <- spain_wine_ratings$price
rating_vec <- spain_wine_ratings$rating

## Forward variable selection

require(leaps)

forward_var <- regsubsets(x=X,y=y, method = "forward")
summary(forward_var)


