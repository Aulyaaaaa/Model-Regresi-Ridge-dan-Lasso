library(caret)
install.packages(c("glmnet", "caret", "dplyr", "car", "nnet", "GGally", "lmtest"))
library(glmnet)
library(caret)
library(dplyr)
library(car)
library(nnet)
library(GGally)
library(lmtest)
# Membaca dataset dengan path yang benar
data <- read.csv("C:\\Users\\user\\OneDrive\\Documents\\KULIAH AULYA SEMESTER 2\\SEMESTER 2 TUGAS\\STATISTIKA TERAPAN\\Praktikum (M4)\\mtcars.csv", header = TRUE, sep = ",")
# Menampilkan jumlah baris dan kolom
print(dim(data))
# Menampilkan struktur data
print(str(data))
# Menampilkan ringkasan statistik
print(summary(data))
data(mtcars)
mtcars <- mtcars %>%
select(-vs, -am)
head(mtcars)
# Menampilkan matriks korelasi
ggcorr(mtcars, label = T)
# Memisahkan data testing dan data train
train <- head(mtcars,24)
test <- tail(mtcars,8)[,-1]
## Split Data for Ridge and LASSO model
xtrain <- model.matrix(mpg~., train)[,-1]
ytrain <- train$mpg
test2 <-  tail(mtcars,8)
ytest <- tail(mtcars,8)[,1]
xtest <- model.matrix(mpg~., test2)[,-1]
ols <- lm(mpg~., train)
summary(ols)
cbind(cor.test(mtcars$mpg,mtcars$cyl)[[3]],
cor.test(mtcars$mpg,mtcars$disp)[[3]],
cor.test(mtcars$mpg,mtcars$hp)[[3]],
cor.test(mtcars$mpg,mtcars$drat)[[3]],
cor.test(mtcars$mpg,mtcars$wt)[[3]],
cor.test(mtcars$mpg,mtcars$qsec)[[3]],
cor.test(mtcars$mpg,mtcars$gear)[[3]],
cor.test(mtcars$mpg,mtcars$carb)[[3]])
shapiro.test(ols$residuals)
Shapiro-Wilk normality test
bptest(ols)
ols_pred <- predict(ols, newdata = train)
ols_pred <- predict(ols, newdata = train)
mean((ols_pred-ytrain)^2)
ols_pred <- predict(ols, newdata = test)
mean((ols_pred-ytest)^2)
stepwise_mod <- step(lm.all, direction="backward")
summary(stepwise_mod)
summary(stepwise_mod)
vif(stepwise_mod)
shapiro.test(stepwise_mod$residuals)
shapiro.test(stepwise_mod$residuals)
bptest(stepwise_mod)
predict.glmnet(ridge_cv, s = 0, type = 'coefficients')
predict.glmnet(ridge_cv, s = 0, type = 'coefficients')
ols$coefficients
set.seed(100)
ridge_cv <- cv.glmnet(xtrain, ytrain, alpha = 0, lambda =
lambdas_to_try)
set.seed(100)
lambdas_to_try)
install.packages("glmnet")
install.packages("glmnet")
ridge_cv <- cv.glmnet(xtrain, ytrain, alpha = 0, lambda =
lambdas_to_try)
lambdas_to_try)
set.seed(100)
ridge_cv <- cv.glmnet(xtrain, ytrain, alpha = 0, lambda =
lambdas_to_try)
lambdas_to_try <- 10^seq(3, -2, by = -0.1)
set.seed(100)
set.seed(100)
install.packages("glmnet")
install.packages("glmnet")
# Buat rentang nilai lambda yang akan diuji
# Jalankan Ridge Regression dengan cross-validation
set.seed(100)
set.seed(100)
install.packages("glmnet")
library(glmnet)
library(glmnet)
lambdas_to_try <- 10^seq(3, -2, by = -0.1)
# Jalankan Ridge Regression dengan cross-validation
set.seed(100)
ridge_cv <- cv.glmnet(xtrain, ytrain, alpha = 0, lambda = lambdas_to_try, nfolds = 5)
plot(ridge_cv)
best_lambda_ridge <- ridge_cv$lambda.min
best_lambda_ridge
predict.glmnet(ridge_mod,  type = 'coefficients')
lambdas_to_try <- 10^seq(-3, 7, length.out = 100)
set.seed(100)
lasso_cv <- cv.glmnet(xtrain, ytrain, alpha = 1, lambda = lambdas_to_try)
lasso_cv <- cv.glmnet(xtrain, ytrain, alpha = 1, lambda = lambdas_to_try)
plot(lasso_cv)
best_lambda_lasso
# Fit final model, get its sum of squared residuals and
multiple R-squared
lasso_mod <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda_lasso)
# MSE of Data Train (OLS)
ols_pred <- predict(ols, newdata = train)
mean((ols_pred-ytrain)^2)
# MSE of Data Test (OLS)
ols_pred <- predict(ols, newdata = test)
mean((ols_pred-ytest)^2)
# MSE of Data Test (Stepwise)
back_pred <- predict(stepwise_mod, newdata = test)
mean((back_pred-ytest)^2)
mean((back_pred-ytest)^2)
ridge_pred <- predict(ridge_mod,  newx = xtrain)
ridge_pred <- predict(ridge_mod,  newx = xtrain)
mean((ridge_pred-ytrain)^2)
ridge_pred <- predict(ridge_mod,  newx = xtest)
mean((ridge_pred-ytest)^2)
# MSE of Data Train (LASSO)
lasso_pred <- predict(lasso_mod,  newx = xtrain)
mean((lasso_pred-ytrain)^2)
mean((lasso_pred-ytrain)^2)
lasso_pred <- predict(lasso_mod,  newx = xtest)
mean((lasso_pred-ytest)^2)
predict_value
predict_value <- cbind(ytest, ols_pred, ridge_pred, lasso_pred,back_pred)
colnames(predict_value) <- c("y_actual", "ols_pred", "ridge_pred", "lasso_pred", "stepwise_pred")
predict_value
colnames(predict_value) <- c("y_actual", "ols_pred", "ridge_pred", "lasso_pred", "stepwise_pred")
predict_value
y_actual <- c(19.2, 27.3, 26.0, 30.4, 15.8, 19.7, 15.0, 21.4)
ols_pred <- c(17.82911, 28.90265, 31.136052, 27.691995, 24.928066, 16.325756, 9.759043, 25.508611)
ridge_pred <- c(15.98929, 27.25260, 27.35073, 26.61872, 18.67489, 19.47247, 12.83954, 24.69188)
lasso_pred <- c(17.18188, 28.22596, 29.31088, 26.99041, 21.75843, 17.19751, 10.55997, 25.19081)
lasso_pred <- c(17.18188, 28.22596, 29.31088, 26.99041, 21.75843, 17.19751, 10.55997, 25.19081)
stepwise_pred <- c(17.52899, 28.88586, 31.41857, 27.96911, 24.89406, 16.33122, 10.48615, 26.32918)
mse_ridge <- mean((y_actual - ridge_pred)^2)
mse_lasso <- mean((y_actual - lasso_pred)^2)
mse_stepwise <- mean((y_actual - stepwise_pred)^2)
mse_values <- c(mse_ols, mse_ridge, mse_lasso, mse_stepwise)
names(mse_values) <- c("OLS", "Ridge", "Lasso", "Stepwise")
names(mse_values) <- c("OLS", "Ridge", "Lasso", "Stepwise")
mse_values
r_squared_lasso <- 1 - sum((matrix_Y - predictions_lasso)^2) / sum((matrix_Y - mean(matrix_Y))^2)
r_squared_lasso <- 1 - sum((matrix_Y - predictions_lasso)^2) / sum((matrix_Y - mean(matrix_Y))^2)
r_squared_lasso <- 1 - sum((y_actual - lasso_pred)^2) / sum((y_actual - mean(matrix_Y))^2)
r_squared_lasso <- 1 - sum((y_actual - lasso_pred)^2) / sum((y_actual - mean(y_actual))^2)
r_squared_lasso
r_squared_lasso
r_squared_ridge
r_squared_ridge
r_squared_ridge <- 1 - sum((y_actual - ridge_pred)^2) / sum((y_actual - mean(y_actual))^2)
r_squared_ridge <- 1 - sum((y_actual - ridge_pred)^2) / sum((y_actual - mean(y_actual))^2)
> bbb
r_stepwise <- 1 - sum((y_actual - ridge_pred)^2) / sum((y_actual - mean(y_actual))^2)
r_stepwise
r_stepwise <- 1 - sum((y_actual - stepwise_pred)^2) / sum((y_actual - mean(y_actual))^2)
r_stepwise