Cali=read.csv("calischool.csv")
dim(Cali)
Cali[1,]
names(Cali)
str(Cali)

data=Cali[,-(1:4)]
str(data)

names(data)
data$grspan=as.factor(data$grspan)

# training and testing 
index = sample(nrow(data),nrow(data)*0.8)
data.train = data[index,]
data.test = data[-index,]

# Linear Regression 
data.lm=lm(calwpct~.,data=data.train)
a=summary(data.lm)
a$sigma^2
a$adj.r.squared
AIC(data.lm)
BIC(data.lm)

# in sample prediction 
pred.lm.train=predict(data.lm)
mse0=sum((data.train$calwpct-pred.lm.train)^2)/(nrow(data.train)-12-1)  
mse0
# out of sample prediction 
pred.lm.test=predict(data.lm,newdata=data.test)
mspe0=mean((data.test$calwpct-pred.lm.test)^2)
mspe0
#cross validation score 
library(boot)
cv.data.lm=glm(calwpct~.,data=data.train)
cv.glm(data.train, cv.data.lm, K=10)$delta[2]


#forward, backward and stepwise 
#set the base first 
nullmodel=lm(calwpct~1,data=data.train)
summary(nullmodel)
fullmodel=lm(calwpct~.,data=data.train)
summary(fullmodel)

#Forward 
data.forward=step(nullmodel,scope=list(lower=nullmodel,upper=fullmodel), direction="forward")
c=summary(data.forward)
AIC(data.forward)
BIC(data.forward)
c$sigma^2 
c$adj.r.squared
# in sample prediction 
pred.forward.train=predict(data.forward)
mse1=sum((data.train$calwpct-pred.forward.train)^2)/(nrow(data.train)-6-1)
mse1
# out of sample prediction 
pred.forward.test=predict(data.forward,newdata=data.test)
mspe1=mean((data.test$calwpct-pred.forward.test)^2)
mspe1
#CV 
cv.forward.glm= glm(calwpct ~ mealpct + elpct + expnstu + computer + mathscr + compstu, data = data.train)
cv.glm(cv.forward.glm, data =data.train, K=10)$delta[2]

#Backward 
data.backward=step(fullmodel,scope=list(lower=nullmodel,upper=fullmodel), direction="backward")
d=summary(data.backward)
AIC(data.backward)
BIC(data.backward)
d$sigma^2   
d$adj.r.squared
# in sample prediction 
pred.backward.train=predict(data.backward)
mse2=sum((data.train$calwpct-pred.backward.train)^2)/(nrow(data.train)-8-1)
mse2
# out of sample prediction 
pred.backward.test=predict(data.backward,newdata=data.test)
mspe2=mean((data.test$calwpct-pred.backward.test)^2)
mspe2
#CV 
cv.backward.glm= glm(calwpct ~ enrltot + teachers + mealpct + computer + 
                       testscr + compstu + expnstu + elpct, data = data.train)
cv.glm(cv.backward.glm, data =data.train, K=10)$delta[2]

#Stepwise
data.step=step(nullmodel,scope=list(lower=nullmodel,upper=fullmodel), direction="both")
e=summary(data.step)
AIC(data.step)
BIC(data.step)
e$sigma^2   
e$adj.r.squared
# in sample prediction 
pred.step.train=predict(data.step)
mse3=sum((data.train$calwpct-pred.step.train)^2)/(nrow(data.train)-6-1)
mse3
# out of sample prediction 
pred.step.test=predict(data.step,newdata=data.test)
mspe3=mean((data.test$calwpct-pred.step.test)^2)
mspe3
#CV 
cv.step.glm= glm(calwpct ~ mealpct + elpct + expnstu + computer + 
                   mathscr + compstu, data = data.train)
cv.glm(cv.step.glm, data =data.train, K=10)$delta[2]

#LASSO
library(glmnet)

data.train.std=cbind(data.train[,1],scale(data.train[,-1]))
data.test.std=cbind(data.test[,1],scale(data.test[,-1]))
data.X.train=data.train.std[,-4]
data.Y.train=data.train.std[,4]
data.X.test=data.test.std[,-4]
data.Y.test=data.test.std[,4]

#
lasso.fit=glmnet(x=data.X.train, y=data.Y.train)
plot(lasso.fit,xvar="lambda")
lasso.fit$dev.ratio[which(cv.lasso$lambda==cv.lasso$lambda.min)]  

cv.lasso=cv.glmnet(x=data.X.train, y=data.Y.train)
plot(cv.lasso)

#two lambdas
cv.lasso$lambda.min #for each lambda we gonna get a model) 
cv.lasso$lambda.1se #1se mean 

coef(lasso.fit, s=cv.lasso$lambda.min) # minimum cross validation error 
coef(lasso.fit, s=cv.lasso$lambda.1se) #1 se stand for the lambda associate with the 1 standard error tolerance  

pred.lasso=predict(lasso.fit,newx = data.X.train,s=cv.lasso$lambda.min)
mse.lasso.min=sum((data.train$calwpct-pred.lasso)^2)/(nrow(data.train)-6-1) #MSE (make sure that it equals to the sum.model1$sigma^2  #MSE )
mse.lasso.min

pred.lasso2= predict(lasso.fit, newx = data.X.test, s=cv.lasso$lambda.min)
mspe.lasso.min= mean((data.Y.test-pred.lasso2)^2)
mspe.lasso.min

pred.lasso=predict(lasso.fit,newx = data.X.train,s=cv.lasso$lambda.1se)
mse.lasso.1se=sum((data.train$calwpct-pred.lasso)^2)/(nrow(data.train)-3-1) #MSE (make sure that it equals to the sum.model1$sigma^2  #MSE )
mse.lasso.1se

pred.lasso2= predict(lasso.fit, newx = data.X.test, s=cv.lasso$lambda.1se)
mspe.lasso.1se= mean((data.Y.test-pred.lasso2)^2)
mspe.lasso.1se

#R^2 for lasso 
lasso.fit$dev.ratio[which(cv.lasso$lambda==cv.lasso$lambda.min)]  
lasso.fit$dev.ratio[which(cv.lasso$lambda==cv.lasso$lambda.1se)]  

#cv score 
which(cv.lasso$lambda==cv.lasso$lambda.min) #the x axis 
cv.lasso$cvm[51] #the y axis
which(cv.lasso$lambda==cv.lasso$lambda.1se)
cv.lasso$cvm[23]

# LOGISTIC MODEL 

Cali1=read.csv("calilogistic.csv")

data2=Cali1[,-(1:4)]
str(data2)
data2$grspan=as.factor(data3$grspan)
names(data2)
# training and testing 
index = sample(nrow(data2),nrow(data2)*0.8)
data2.train = data2[index,]
data2.test = data2[-index,]


#model 
data2.glm0=glm(calwpct.25~., family=binomial, data=data2.train)
sum.glm0=summary(data2.glm0)
sum.glm0$deviance
AIC(data2.glm0)
BIC(data2.glm0)








#pruning tree 
library(rpart)
library(rpart.plot)
data.tree1=rpart(calwpct~., data=data.train)
prp(data.tree1, digits = 4, extra =1)
printcp(data.tree1)

data.tree1
summary(data.tree1)
#deviance is SSE , yval is the average predicted value in that particular node 
prp(data.tree1, digits = 4, extra =1)
pred.tree1.train=predict(data.tree1)
pred.tree1.test=predict(data.tree1,newdata=data.test)
#MSE
sum((data.train$calwpct-pred.tree1.train)^2)/(nrow(data.train)-6-1)
mean((pred.tree1.train-data.train$calwpct)^2)
#MSPE 
mean((data.test$calwpct-pred.tree1.test)^2)




##prune the tree 
boston.tree2=rpart(calwpct~., data=data.train, cp=0.001)
prp(boston.tree2, digits=4, extra=1)
#dash line is smallest CV + 1 standard error 
# Nsplit+1=size of tree
plotcp(boston.tree2)
printcp(boston.tree2)

#choose tree size =6
boston.tree3=rpart(calwpct~., data=data.train, cp=  0.021)
boston.tree3
prp(boston.tree3, digits = 4, extra =1)
pred.tree3.train=predict(boston.tree3)
pred.tree3.test=predict(boston.tree3,newdata=data.test)

pred.tree3.train=predict(boston.tree3)
pred.tree3.test=predict(boston.tree3,newdata=data.test)
#MSE
sum((data.train$calwpct-pred.tree3.train)^2)/(nrow(data.train)-4-1)
mean((pred.tree1.train-data.train$calwpct)^2)
#MSPE 
mean((data.test$calwpct-pred.tree.test)^2)






