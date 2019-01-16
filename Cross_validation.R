library(ISLR)
library(boot)
?cv.glm
plot(reviews ~ price, data=airbnb)

## LOOCV
glm.fit = glm(reviews ~ price, data=airbnb)
cv.glm(airbnb, glm.fit)$delta

## Lets write a simple function to use formula
loocv = function(fit){
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

cv.error = rep(0, 5)
degree = 1:5
for(d in degree){
  glm.fit = glm(reviews ~ poly(price, d), data=airbnb)
  cv.error[d] = loocv(glm.fit)
}
plot(degree, cv.error, type="b")

## 10-fold CV
cv.error10 = rep(0, 5)
for(d in degree){
  glm.fit = glm(reviews ~ poly(price, d), data=airbnb)
  cv.error10[d] = cv.glm(airbnb, glm.fit, K=10)$delta[1]
}
lines(degree, cv.error10, type="b", col="red")
