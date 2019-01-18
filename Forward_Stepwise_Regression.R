## Forward Stepwise Selection
## Here we use the `regsubset` function but specify the `method=forward` option
regfit.fwd = regsubsets(price ~ ., data=airbnb_fit, method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")


## Model Selection Using a Validation Set
## let's make a training and validation set, so that we can choose a good subset model.
## We will do it using a slightly different approach from what was done in the book.
dim(airbnb_fit)
set.seed(1)
train = sample(seq(1038), 500, replace=FALSE)
train
regfit.fwd = regsubsets(price ~ ., data=airbnb_fit[train,], method="forward")

## Now we will make predictions on the observation not used for training.
## We know there are 8 models, so we set up some vectors to record the errors.
## We have to do a bit of work here, because there is no predict mothod for `regsubsets`
val.errors = rep(NA, 8)
x.test = model.matrix(price ~ ., data=airbnb_fit[-train,])
for (i in 1:8){
  coefi = coef(regfit.fwd, id=i)
  pred = x.test[, names(coefi)]%*%coefi
  val.errors[i] = mean((airbnb_fit$price[-train]-pred)^2)
}
plot(sqrt(val.errors), ylab="Root MSE", ylim=c(70, 100), pch=19, type="b")
points(sqrt(regfit.fwd$rss[-1]/500), col="blue", pch=19, type="b")
legend("topright", legend=c("Training", "Validation"), col=c("blue", "black"), pch=19)
## As we expect, the training error goes down monotonically as the model gets bigger

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  mat[, names(coefi)]%*%coefi
}
