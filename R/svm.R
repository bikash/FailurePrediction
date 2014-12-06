library(e1071)

x <- c(1:10)
y <- c(0,0,0,0,1,0,1,1,1,1)
test <- c(11:15)
DF <- data.frame(x = x, y = y)
mod <- svm(y ~ x, data = DF, kernel = "linear", gamma = 1, cost = 2, type="C-classification")
predict(mod, newdata = data.frame(x = test))