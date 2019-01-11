library(tidyverse)
library(MASS)
library(ISLR)

airbnb <- read_csv("s3_files/boston/tomslee_airbnb_boston_0054_2014-09-24.csv")
names(airbnb)
plot(price ~ reviews, airbnb)
fit1 = lm(price ~ reviews, data = airbnb)
fit1
summary(fit1)
abline(fit1, col="red")
names(fit1)
confint(fit1)
predict(fit1, data.frame(reviews=c(50, 100, 150)), interval="confidence")

## Multiple linear regression
fit2 = lm(price ~ reviews + bedrooms, data = airbnb)
summary(fit2)

allfit <- airbnb %>%
  dplyr::select(-room_id, -host_id, -borough, -neighborhood, -latitude, -longitude, -last_modified)
fit3 = lm(price ~ ., allfit)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

fit4 = update(fit3, ~.-minstay)
summary(fit4)

## Nonlinear terms and interactions
fit5 = lm(price ~ accommodates*bedrooms, airbnb)
summary(fit5)

fit6 = lm(price ~ reviews + I(reviews^2), airbnb)
summary(fit6)
par(mfrow=c(1,1))
plot(price ~ reviews, airbnb)
points(airbnb$reviews, fitted(fit6), col="red", pch=20)

fit7 = lm(price ~ poly(reviews, 4), data=airbnb)
points(airbnb$reviews, fitted(fit7), col="blue", pch=20)


## Qualitative predictors
airbnb$type_factor <- factor(airbnb$room_type)
summary(airbnb)
contrasts(airbnb$type_factor)

## Writing R functions
regplot = function(x, y){
  fit = lm(y ~ x)
  plot(x, y)
  abline(fit, col="red")
}
regplot(airbnb$reviews, airbnb$price)

regplot = function(x, y, ...){
  fit = lm(y ~ x)
  plot(x, y, ...)
  abline(fit, col="red")
}
regplot(airbnb$reviews, airbnb$price, xlab="Reviews", ylab="Price", col="blue", pch=20)
