set.seed(123)
x <- rnorm(1000)
y <- 2*x+rnorm(1000, 0, .1)
dat <- data.frame(y, x)
t <- train(y~x, data = dat, method="lm")
nd <- data.frame(x=rnorm(100))
p <- predict(t, nd)
pi <- predict(t, nd, interval="prediction")

data(iris)
tc <- train(Species ~ ., data=iris, method="rf")
pc <- predict(tc)

head(extractPrediction(list(t), testX = data.frame(rnorm(10)), testY = data.frame(rnorm(10))))

#tbst <- train(y~x, data = dat, method="bstSm")

library(doParallel)
registerDoParallel()
tbag <- train(Species ~ ., data=iris, method="AdaBag", trControl = trainControl(method="none"), tuneLength = 1)
stopImplicitCluster()

