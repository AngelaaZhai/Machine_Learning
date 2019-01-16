## Bootstrap
## Minimum risk investment
alpha = function(x, y){
  vx = var(x)
  vy = var(y)
  cxy = cov(x, y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(airbnb$price, airbnb$reviews)

## What is the standard error of alpha?
alpha.fn = function(data, index){
  with(data[index, ], alpha(price, reviews))
}
alpha.fn(airbnb, 1:100)

set.seed(1)
alpha.fn(airbnb, sample(1:100, 100, replace=TRUE))

boot.out = boot(airbnb, alpha.fn, R=1000)
boot.out
plot(boot.out)

## Bootstrapping of time series (block bootstrap)
## Function tsboot()