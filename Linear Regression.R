###### lab 3

library(MASS)
data("Boston")
str(Boston)
?Boston

### EDA
names(Boston)
summary(Boston)
sd(Boston$crim)
apply(Boston, 2, sd) # 2 means by column

pairs(Boston)

hist(Boston$medv, breaks = 20)

# correlation
cor(Boston[, c(1:3)])

###############
## splitting to training and testing sample
index= sample(1:nrow(Boston), 0.8*nrow(Boston))
boston.train= Boston[index, ]
boston.test= Boston[-index, ]   # no one will see this part until your model is ready

nrow(boston.test)

## simple linear regression
model0= lm(medv~lstat, data = boston.train)
names(model0)  # names of the components on this list
model0$coefficients
residual0= model0$residuals
fitted0= model0$fitted.values
cbind(boston.train$medv-fitted0,residual0) # just a check

# fitted line (only for simple linear regression)
plot(boston.train$lstat, boston.train$medv)
abline(model0, col="red", lwd=2)

# model summary
sum.model0=summary(model0)
names(sum.model0)
sum.model0$sigma^2  # MSE
sum.model0$adj.r.squared

# confidence interval
confint(model0)

### prediction
# in-sample prediction
pred.model0= predict(model0) 
mse0= sum((boston.train$medv-pred.model0)^2)/(nrow(boston.train)-1-1) # MSE
# out-of-sample prediction (to get test error)
pred.model0.test= predict(model0, newdata = boston.test)
mspe0= mean((boston.test$medv-pred.model0.test)^2)


#### multiple linear regression
model1= lm(medv~., data = boston.train)
dim(boston.train)
model1
sum.model1= summary(model1)
sum.model1$adj.r.squared
sum.model1$sigma^2
AIC(model1)
BIC(model1)

# prediction
pred.model1= predict(model1)
mse1= sum((boston.train$medv-pred.model1)^2)/(nrow(boston.train)-13-1)
# out-of-sample 
pred.nodel1.test= predict(model1, newdata = boston.test)
mspe1= mean((boston.test$medv-pred.nodel1.test)^2)

## cross-validation
library(boot)
?cv.glm
model0= glm(medv~lstat, data = boston.train)
cv.glm(boston.train, model0, K=10)$delta[2] 
#cost: default is MSPE, any type of error
# delta has two numbers, they are both CV-score, but the 2nd is adjusted for bias

model1= glm(medv~., data = boston.train)
cv.glm(boston.train, model1, K=10)$delta[2]

######## Exercise

str(nba)

index=sample(1:nrow(nba), .9*nrow(nba))

nba.train = nba[index,]
nba.test = nba[-index,]

nrow(nba.test)
nrow(nba.train)

# 3. multiple linear regression
modelnba = lm(PTS~., data=nba.train)
names(modelnba)
sum.modelnba1 = summary(modelnba)

# 4. Confidence interval
confint(modelnba)

# 5. 
summary(modelnba)
modelnba2=lm(PTS~. -GP -`3P%` -`FG%` -STL, data=nba.train)
sum.modelnba2=summary(modelnba2)

# 6
sum.modelnba1$adj.r.squared; sum.modelnba2$adj.r.squared
sum.modelnba1$sigma^2; sum.modelnba2$sigma^2
AIC(modelnba); AIC(modelnba2)
BIC(modelnba); BIC(modelnba2)

pred.modelnba1= predict(modelnba, newdata = nba.test)
mspenba1= mean((nba.test$PTS-pred.modelnba1)^2)

pred.modelnba2= predict(modelnba2, newdata = nba.test)
mspenba2= mean((nba.test$PTS-pred.modelnba2)^2)

mspenba1; mspenba2

# 7.
library(boot)
nba.glm= glm(PTS~., data = nba.train)
cv.nba1= cv.glm(nba.glm, data = nba.train, K=10)$delta[2]

nba.glm2= glm(PTS~. -GP -`3P%` -`FG%` -STL, data = nba.train)
cv.nba2= cv.glm(nba.glm2, data = nba.train, K=10)$delta[2]

cv.nba1; cv.nba2


































