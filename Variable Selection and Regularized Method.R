####### lab 4

library(MASS)
data("Boston")
str(Boston)

## splitting to training and testing sample
index= sample(1:nrow(Boston), 0.8*nrow(Boston))
boston.train= Boston[index, ]
boston.test= Boston[-index, ]   # no one will see this part until your model is ready

## several models in lab 3
model0 = lm(medv~lstat, data = boston.train)
model1 = lm(medv~., data = boston.train)
model2 = lm(medv~. - age - indus, data = boston.train)

AIC(model0); AIC(model1); AIC(model2)

predict.model1 = predict(model1, newdata = boston.train)
a = summary(predict.model1)
a$sigma^2
### best subset
library(leaps)
?regsubsets
model.subset= regsubsets(medv~., data = boston.train, nbest = 2, nvmax = 13)
names(Boston)
summary(model.subset)
# plot for visulization
plot(model.subset, scale = "bic")
# refit the optimal model (in my case, age and indus are dropped)
model.bestsub= lm(medv~.-age-indus, data = boston.train)


### forward/backward/stepwise
nullmodel= lm(medv~1, data = boston.train)
fullmodel = model1

# forward selection
model.forward= step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), 
                    direction = "forward")
# using BIC
model.forward.bic= step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), 
                    direction = "forward", k=log(nrow(boston.train)))
summary(model.forward.bic)

# backward
model.backward= step(fullmodel, direction = "backward")
model.backward.bic= step(fullmodel, direction = "backward", k=log(nrow(boston.train)))

# stepwise
model.step= step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), 
                 direction = "both")
model.step.bic=step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), 
                    direction = "both", k=log(nrow(boston.train)))

  
### Out-of-sample prediction (Exercise)
pred0= predict(model0, newdata = boston.test)
mspe0= mean((boston.test$medv-pred0)^2)
pred1= predict(model1, newdata = boston.test)
mspe1= mean((boston.test$medv-pred1)^2)
pred2= predict(model2, newdata = boston.test)
mspe2= mean((boston.test$medv-pred2)^2)

pred.subset= predict(model.bestsub, newdata = boston.test)
mspe.subset= mean((boston.test$medv-pred.subset)^2)
pred.forward = predict(model.forward, newdata = boston.test)
mspe.forward= mean((boston.test$medv-pred.forward)^2)
pred.forward.bic= predict(model.forward.bic, newdata = boston.test)
mspe.forward.bic= mean((boston.test$medv-pred.forward.bic)^2)
  
### cross-validation (exercise)
library(boot)
model.forward.glm= glm(medv~. -age-indus, data = boston.train)
cv.forward= cv.glm(model.forward.glm, data = boston.train, K=10)$delta[2]
  
### LASSO
library(glmnet)
# 
# X= as.matrix(boston.train[,-14])
# Y= as.matrix(boston.train[,14])
# lasso.fit= glmnet(x=X, y=Y)
# plot(lasso.fit, xvar = "lambda")

### standardization (recommended)
head(Boston[,14])
head(Boston)
Boston.X.std= scale(Boston[,-14])
head(Boston.X.std)
Boston.X.train= Boston.X.std[index,]
Boston.Y.train= Boston[index,14]
Boston.X.test= Boston.X.std[-index,]
Boston.Y.test= Boston[-index,14]

### fit lasso model
lasso.fit= glmnet(x=Boston.X.train, y=Boston.Y.train)
plot(lasso.fit, xvar = "lambda")

# cross validation to select lambda
cv.lasso= cv.glmnet(x=Boston.X.train, y=Boston.Y.train)
plot(cv.lasso)

# two optimal lambdas
cv.lasso$lambda.min
cv.lasso$lambda.1se

# obtain model with specific lambda
coef(lasso.fit, s=1)
model.lambda.min = coef(lasso.fit, s=cv.lasso$lambda.min)
model.lambda.1se = coef(lasso.fit, s=cv.lasso$lambda.1se)

dim(Boston)
# prediction
pred.lambda.min = predict(lasso.fit, newx = Boston.X.train, s = cv.lasso$lambda.min)
head(matrix(Boston.X.train))
mse.lambda.min = sum((Boston.Y.train - pred.lambda.min) ^ 2)/(nrow(matrix(Boston.Y.train)) - 13 - 1)
pred.lambda.1se = predict(lasso.fit, newx = Boston.X.train, s = cv.lasso$lambda.1se)
mse.lambda.1se = sum((Boston.Y.train - pred.lambda.1se) ^ 2)/(nrow(matrix(Boston.Y.train)) - 13 - 1)

pred.lasso1= predict(lasso.fit, newx = Boston.X.test, s=1)
mspe.lasso1= mean((Boston.Y.test-pred.lasso1)^2)
  
pred.lasso2= predict(lasso.fit, newx = Boston.X.test, s=cv.lasso$lambda.min)
mspe.lasso2= mean((Boston.Y.test-pred.lasso2)^2)
  
pred.lasso3= predict(lasso.fit, newx = Boston.X.test, s=cv.lasso$lambda.1se)
mspe.lasso3= mean((Boston.Y.test-pred.lasso3)^2)

## cv score
cv.lasso$cvm[which(cv.lasso$lambda==cv.lasso$lambda.min)]
cv.lasso$cvm[which(cv.lasso$lambda==cv.lasso$lambda.1se)]
?lasso2.cvm
## R^2
lasso.fit$dev.ratio[which(cv.lasso$lambda==cv.lasso$lambda.min)]
lasso.fit$dev.ratio[which(cv.lasso$lambda==cv.lasso$lambda.1se)]
