### lab 5

# load the data
credit.data <- read.csv("http://homepages.uc.edu/~lis6/DataMining/Data/credit_default.csv", header=T)
#credit.data$EDUCATION[credit.data$EDUCATION<1 | credit.data$EDUCATION>4]=4

dim(credit.data)
names(credit.data)

names(credit.data)[24]= "Default"  # 24th variable

## EDA
mean(credit.data$Default)  # overall default rate

str(credit.data)

## convert sex, education, marriage to categorical variable
credit.data$SEX= as.factor(credit.data$SEX)
credit.data$EDUCATION= as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE= as.factor(credit.data$MARRIAGE)

str(credit.data)


## graph -- scatter plot

plot(credit.data$EDUCATION, credit.data$Default)

# Chi-square test  (works the best for two categorical variables)
twowaytable= table(credit.data$EDUCATION, credit.data$Default)
chisq.test(twowaytable) #a pvalue of < 0.05 indicates significant. > 0.05 indicates insignificant


## random split data
index= sample(nrow(credit.data), 0.8*nrow(credit.data))
credit.train= credit.data[index,]
credit.test= credit.data[-index,]

### logistic model
credit.glm0= glm(Default~., family = binomial, data = credit.train)
sum.glm0=summary(credit.glm0)

## extract some statistics
sum.glm0$deviance
sum.glm0$aic
AIC(credit.glm0) # alternative way
BIC(credit.glm0)

## in-sample
credit.glm0.pred= predict(credit.glm0, type = "response") # type= "response" gives you predicted probability 

# ROC
library(ROCR)
pred= prediction(credit.glm0.pred, credit.train$Default)
?prediction
perf= performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
# get AUC
unlist(slot(performance(pred, "auc"), "y.values"))
?slot
?unlist

### exercise -- out-of-sample prdiction
## testing sample
credit.glm0.pred.test= predict(credit.glm0, newdata= credit.test, type = "response") # type= "response" gives you predicted probability 
# ROC
pred= prediction(credit.glm0.pred.test, credit.test$Default)
perf= performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
# get AUC
unlist(slot(performance(pred, "auc"), "y.values"))

### classification

## choice of p-cut

## grid search of optimal pcut

# define a cost function -- MR / weighted MR
cost= function(obs, pred.prob, pcut){
  
  weight0= 8 #FN
  weight1= 1 #FP
  
  c0= obs==1 & pred.prob<pcut # FN
  c1= obs==0 & pred.prob>=pcut # FP
  
  cost= mean(weight0*c0+weight1*c1)  # weighted MR
  
  return(cost)
  
}

cost(credit.train$Default, credit.glm0.pred, pcut=0.22)

## exercise: grid search the optimal pcut
pcut.seq= seq(0, 1, length.out = 100)
cost.seq= rep(0, length(pcut.seq))
for(i in 1:length(pcut.seq)){
  cost.seq[i]= cost(credit.train$Default, credit.glm0.pred, pcut=pcut.seq[i])
}
plot(pcut.seq, cost.seq)
opt.pcut= pcut.seq[which(cost.seq==min(cost.seq))]
# note: in comparison, you have to compare all cost with the same weight.

# confusion matrix based on the optimal cost
class.glm0.train_opt= (credit.glm0.pred> opt.pcut)*1 #insample
table(credit.train$Default, class.glm0.train_opt, dnn = c("TRUE", "PRED"))

## exercise (HW bonus): investigate the cost vs. weight (1:1 through 10:1)
## exercise: out-of-sample performance (cost, FP, FN)
cost(credit.test$Default, credit.glm0.pred.test, pcut=opt.pcut)
# confusion matrix
table(credit.test$Default, (credit.glm0.pred.test>opt.pcut)*1)

### variable selection
# stepwise selection
nullmodel= glm(Default~1, data = credit.train)
fullmodel= credit.glm0
backmodel= step(fullmodel, direction = "backward")
summary(backmodel)

stepwise= step(nullmodel, scope = list(lower= nullmodel, upper= fullmodel), direction = "both")
summary(stepwise)

## compute all metrics for comparison (omitted)



### LASSO

library(glmnet)
# to manually create design matrix (the X matrix in the model)
# this step is automatically done for glm() but not LASSO
dummy= model.matrix(~., data = credit.data)[,-1] 
dummy1 = model.matrix(~., data = credit.data)
names(credit.data)
names(dummy)
?model.matrix
# standardization
dummy.stand= scale(dummy[,-27])


credit.train1.X= dummy.stand[index,]
credit.test1.X= dummy.stand[-index,]
credit.train1.Y= credit.train$Default
credit.test1.Y= credit.test$Default

dim(credit.test1)

fit.lasso= glmnet(x= as.matrix(credit.train1.X), y=credit.train1.Y, family = "binomial") # 27th column is the "y" for this specific dataset
?glmnet
?cv.glmnet
cv.lasso= cv.glmnet(x= as.matrix(credit.train1.X), y=credit.train1.Y, family = "binomial")
plot(cv.lasso)

# prediction
credit.pred.lasso= predict(fit.lasso, newx = credit.train1.X, s=cv.lasso$lambda.min, type = "response")


#########  Cross validation
library(boot)
costforcv= function(obs, pred.prob){
  
  weight0= 8 #FN
  weight1= 1 #FP
  
  c0= obs==1 & pred.prob<opt.pcut # FN
  c1= obs==0 & pred.prob>=opt.pcut # FP
  
  cost= mean(weight0*c0+weight1*c1)  # weighted MR
  
  return(cost)
  
}
?cv.glm
cvglm0= cv.glm(glmfit=credit.glm0, data = credit.train, K=3, cost = costforcv)
cvglm0$delta[2]
























































































































