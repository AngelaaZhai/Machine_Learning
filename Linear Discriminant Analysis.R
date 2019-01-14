library(ISLR)
library(MASS)

## Linear Discriminant Analysis
lda.fit = lda(positive ~ reviews + accommodates, data=airbnb, subset=room_id<2000000)
lda.fit
plot(lda.fit)
airbnb.2000 = subset(airbnb, room_id>=2000000)
lda.pred = predict(lda.fit, airbnb.2000)
lda.pred[1:5,]
## Not a data frame
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, airbnb.2000$positive)
mean(lda.pred$class == airbnb.2000$positive)

## K-Nearest Neighbors
library(class)
?knn
Xlag = cbind(airbnb$reviews, airbnb$accommodates)
train = airbnb$room_id<2000000
knn.pred = knn(Xlag[train, ], Xlag[!train, ], airbnb$positive[train], k=1)
table(knn.pred, airbnb$positive[!train])
mean(knn.pred == airbnb$positive[!train])
