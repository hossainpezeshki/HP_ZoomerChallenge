rm (list = ls())
graphics.off()
library (e1071)
library (caret)
library (randomForest)

zmr.train <- read.csv (file="Zoomer_take_home_challenge_training_set.csv", header=TRUE)

table (zmr.train$events)

mytrns <- function (indf) {
  outdf <- data.frame (weekday =as.factor (weekdays (as.Date (indf$X, '%Y-%m-%d'))))
  outdf$calendar_code <- as.factor (indf$calendar_code)
  outdf$restaurant_count <- as.numeric (indf$restaurant_count)
  
  outdf$max_temp <- indf$max_temp
  outdf$min_temp <- indf$min_temp
  outdf$precipitation <- indf$precipitation
  
  tmp <- grep ("Fog", indf$events)
  v <- vector ("logical", dim(indf)[1])
  v[tmp] <- TRUE; v[-tmp] <- FALSE
  outdf$Fog <- as.factor (v)
  
  tmp <- grep ("Rain", indf$events)
  v[tmp] <- TRUE; v[-tmp] <- FALSE
  outdf$Rain <- as.factor (v)
  
  tmp <- grep ("Snow", indf$events)
  v[tmp] <- TRUE; v[-tmp] <- FALSE
  outdf$Snow <- as.factor (v)
  
  outdf
}

trans.zmr.train <- mytrns (zmr.train)
y <- zmr.train$order_count  # The response variable


fitlm <- lm (y~0+., data=trans.zmr.train)
fitlm
prdlm <- predict (fitlm, newdata=trans.zmr.train)
str (prdlm)

RMSE <- function (x, y) {
  stopifnot (length(x) == length(y))
  s <- sqrt (mean ((x-y)^2))
  s
}



cat ("Linear model apparent error = ", RMSE (prdlm, y), '\n')

set.seed (339)

rffit <- randomForest (y~., data=trans.zmr.train, importance=TRUE)

X11()
plot (rffit$mse, type='l', xlab="number of trees",
      ylab = "out-of-bag MSE on log scale", log="y"); grid()

cat ("Most benefit gained with the first two hundred trees\n")
cat ("So we re-fit the forest with ntree=200\n")
rffit <- randomForest (y~., data=trans.zmr.train, importance=TRUE, ntree=200)





rfprd <- predict (rffit, newdata=trans.zmr.train)

cat ("Random forest apparent error = ", RMSE (rfprd, y), '\n')




X11()
par (mfrow = c(1,2))
axisRange <- extendrange (c(y, prdlm, rfprd))
plot (y, prdlm,
      xlim = axisRange, ylim = axisRange,
      xlab = "order counts", ylab = "prediction", col='blue',
		main="Linear regression predictions")
abline (0,1, col='darkgrey', lty=2); grid()

plot (y, rfprd,
      xlim = axisRange, ylim = axisRange,
      xlab = "order counts", ylab = "prediction",col='red',
		main="Random forest predictions")
abline (0,1, col='darkgrey', lty=2); grid()


sgnf <- importance (rffit, scale=TRUE)
ind <- order (sgnf[,2], decreasing = TRUE)
print (sgnf[ind,])









