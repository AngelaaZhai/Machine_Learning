library(tidyverse)
library(ISLR)

airbnb <- read_csv("s3_files/boston/tomslee_airbnb_boston_0054_2014-09-24.csv")
airbnb$type_factor <- factor(airbnb$room_type)
airbnb <- airbnb %>%
  mutate(positive = factor(ifelse(overall_satisfaction>4.5, "Perfect", "Good"))) %>%
  filter(!is.na(positive))
summary(airbnb)
airbnb_pair <- airbnb %>%
  dplyr::select(-room_id, -host_id, -borough, -neighborhood, -latitude, -longitude, -last_modified, -room_type, -positive)
pairs(airbnb_pair, col=airbnb$type_factor)

## Logistic regression
glm.fit = glm(positive ~ reviews + accommodates + bedrooms + price + minstay + type_factor,
              data=airbnb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type="response")
glm.probs[1:5]
glm.pred = ifelse(glm.probs>0.5, "Perfect", "Good")
table(glm.pred, airbnb$positive)
mean(glm.pred==airbnb$positive)

## Make training and test set
train = airbnb$room_id<2000000
glm.fit = glm(positive ~ reviews + accommodates + bedrooms + price + minstay + type_factor,
              data=airbnb, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=airbnb[!train,], type="response")
glm.pred = ifelse(glm.probs>0.5, "Perfect", "Good")
positive.2000 = airbnb$positive[!train]
table(glm.pred, positive.2000)
mean(glm.pred==positive.2000)

## Fit smaller model
glm.fit = glm(positive ~ reviews + accommodates,
              data=airbnb, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=airbnb[!train,], type="response")
glm.pred = ifelse(glm.probs>0.5, "Perfect", "Good")
table(glm.pred, positive.2000)
mean(glm.pred==positive.2000)
