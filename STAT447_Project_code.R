library(tidyverse)
library(glmnet)

spain_wine_ratings <- read_csv("C:/Users/ranen/Downloads/wines_SPA.csv")

head(spain_wine_ratings)

## Lasso


regression_tib <- select(spain_wine_ratings, 
                              c("year", "region", "price", "type", "body", "acidity", "rating"))

## We have enough full observations to not worry about omitting missing values
regression_tib <- na.omit(regression_tib)

predictors <- c("year", "region", "price", "type", "body", "acidity")

response <- "rating"

regression_vars_tib <- select(regression_tib, all_of(predictors))

rating <- select(regression_tib, all_of(response))

fit <- glmnet(regression_vars_tib, rating)

