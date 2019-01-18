## Model selection by Cross-Validation
## We will do 10-fold cross-validation. It's really easy!
set.seed(1)
folds = sample(rep(1:10, length=nrow(airbnb_fit)))
folds
table(folds)
cv.errors = matrix(NA, 10, 8)
for (k in 1:10){
  best.fit = regsubsets(price ~ ., data=airbnb_fit[folds!=k,], method="forward")
  for (i in 1:8){
    pred = predict(best.fit, airbnb_fit[folds==k,], id=i)
    cv.errors[k, i] = mean((airbnb_fit$price[folds==k]-pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type="b")
