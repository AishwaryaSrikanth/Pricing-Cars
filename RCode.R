########################Lasso#####################################################
Cars=read.csv("Cars.csv")
Cars_X_out=read.csv("Cars_X_out.csv")
attach(Cars)
attach(Cars_X_out)
names(Cars)

train = data.frame(Cars)
test = data.frame(Cars)
tr = sample(1:29466,23573)
train = train[tr,]
test = test[-tr,]
library(glmnet)

x = model.matrix(price~.,data=train)
x=x[,-17]

CAdata = data.frame(train$price,x)
Lasso.Fit = glmnet(x,train$price, lambda = 35)
par(mfrow=c(1,1))
plot(Lasso.Fit)

CV.L = cv.glmnet(x, train$price)
plot(CV.L)

LamL = CV.L$lambda.1se
bestlam=CV.L$lambda.min # best lamda from lasso = 35

plot(log(CV.L$lambda),sqrt(CV.L$cvm),main="LASSO CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)


abline(v=log(LamL),lty=2,col=2,lwd=2)
coef.L = predict(CV.L,type="coefficients",s=LamL)

coef.L

################Ridge######################################################################################

Ridge.Fit = glmnet(x,train$price, alpha = 0)

par(mfrow=c(1,1))
plot(Ridge.Fit)

CV.R = cv.glmnet(x, train$price)
plot(CV.R)

LamR = CV.R$lambda.1se
bestlam=CV.R$lambda.min

plot(log(CV.R$lambda),sqrt(CV.R$cvm),main="RIDGE CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)


abline(v=log(LamR),lty=2,col=2,lwd=2)
coef.R = predict(CV.R,type="coefficients",s=LamL)
coef.R

############################MULTIPLE REGRESSION#################################################################
library(DAAG)
train = data.frame(Cars)
test = data.frame(Cars)
## 80:20 
tr = sample(1:29466,23573)
train = train[tr,] 
test = test[-tr,]

step(lm(price~1),data=data.frame(train),direction='both',scope=~trim+isOneOwner+year+ log(mileage)+color+displacement+
       fuel+soundSystem + wheelType + featureCount+condition+state+region+condition*year+isOneOwner*year+log(mileage)*isOneOwner
     +log(mileage)*condition+log(mileage)*year + log(mileage)*displacement+fuel*log(mileage)+soundSystem*featureCount
     +displacement*fuel + poly(mileage,2)+poly(year,2)+poly(featureCount,2))

reg_final = lm(price ~ poly(year, 2) + displacement + log(mileage) + 
                 trim + poly(mileage, 2) + condition + fuel + wheelType + 
                 poly(featureCount, 2) + isOneOwner + 
                 displacement:log(mileage) + log(mileage):condition + displacement:fuel + 
                 log(mileage):fuel + log(mileage):isOneOwner) #removed soundsystem,state and color based on p values, 8097 se

summary(reg_final)
#test$subTrim= test$color = test$state = test$X = test$soundSystem = test$region = test$wheelSize = NULL
names(test)


resultColNum <- grep("price", names(test))
prediction <- predict(reg_final, test)
prediction
rmsqe <- sqrt(mean((prediction - test$price)^2))
rmsqe #8087.604

###############Multiple reg with lasso variables################################################################
step(lm(price~1),data=data.frame(train),direction='both',scope=~trim + displacement + mileage +
       soundSystem + year + fuel + wheelType + condition + displacement*mileage + year*mileage+
       mileage*fuel + mileage*condition+condition*year, data = ca)
reg_final = lm(price ~ condition + trim + year + displacement + 
                 mileage + fuel + soundSystem + wheelType + displacement:mileage + 
                 year:mileage + condition:year + mileage:fuel + condition:mileage, data = ca)

summary(reg_final)
a = predict(reg_final,test)
rmse = sqrt(mean((test$price - (a))^2))
rmse
#8893(out of sample)
# Residual standard error (in-sample): 8905
# Multiple regression




########################Random forest###########################################################################
Cars=read.csv("Cars.csv")
Cars_X_out=read.csv("Cars_X_out.csv")
attach(Cars)
attach(Cars_X_out)
names(Cars)

train = data.frame(Cars)
test = data.frame(Cars)

tr = sample(1:29466,23573) #80% as the training set

train = train[tr,]
test = test[-tr,] #remaining 20% as the test set
dim(test)
dim(test_new)
test_new = data.frame(Cars_X_out)
library(randomForest)

###########################ntrees=200, mtry=6########################################

forest_200_6 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                               fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=200, mtry=6,importance=TRUE)
forest_200_6
# Mean of squared residuals: 52653483
# % Var explained: 97.38
pred_200_6 = predict(forest_200_6,test)
#plot(pred_200_6,test)
rmse_200_6 = sqrt(mean((test$price - (pred_200_6))^2))
rmse_200_6   ##We get an RMSE of 6241.839
importance(forest_200_6)
varImpPlot(forest_200_6) #Node purity is measured by Gini Index which is the the difference between RSS before and after the split on that variable.
#to know imp variables . represents the mean decrease in node impurity (and not the mean decrease in accuracy). 
plot(forest_200_6)
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 


###########################ntrees=300, mtry=6, nodesize=20########################################
set.seed(3)
forest_300_6_20 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                              fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=300, mtry=6,nodesize=20, importance=TRUE)
forest_300_6_20
# Mean of squared residuals: 51312261
# % Var explained: 97.45
pred_300_6_20 = predict(forest_300_6_20,test)
#plot(pred_200_6,test)
rmse_300_6_20 = sqrt(mean((test$price - (pred_300_6_20))^2))
rmse_300_6_20   ##We get an RMSE of 6240.967
importance(forest_300_6_20)
varImpPlot(forest_300_6_20) 
plot(forest_300_6_20)



###########################ntrees=500, mtry=6, nodesize=20########################################

set.seed(3)
forest_500_6_20 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                                 fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=500, mtry=6,nodesize=20, importance=TRUE)
forest_500_6_20
# Mean of squared residuals: 51247510
# % Var explained: 97.45
pred_500_6_20 = predict(forest_500_6_20,test)
#plot(pred_200_6,test)
rmse_500_6_20 = sqrt(mean((test$price - (pred_500_6_20))^2))
rmse_500_6_20   ##We get an RMSE of 6236.509
importance(forest_500_6_20)
varImpPlot(forest_500_6_20) 
plot(forest_500_6_20)


###########################ntrees=500, mtry=6, nodesize=5 DH########################################

set.seed(3)
forest_500_6_5 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                                 fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=500, mtry=6,nodesize=5, importance=TRUE)
forest_500_6_5
# Mean of squared residuals: 52403038
# % Var explained: 97.4
pred_500_6_5 = predict(forest_500_6_5,test)
#plot(pred_200_6,test)
rmse_500_6_5 = sqrt(mean((test$price - (pred_500_6_5))^2))
rmse_500_6_5   ##We get an RMSE of 6254.839
importance(forest_500_6_5)
varImpPlot(forest_500_6_5) 
plot(forest_500_6_5)



###########################ntrees=500, mtry=7, nodesize=5########################################

set.seed(3)
forest_500_7_5 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                                fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=500, mtry=7,nodesize=5, importance=TRUE)
forest_500_7_5
# Mean of squared residuals: 52884278
# % Var explained: 97.37
pred_500_7_5 = predict(forest_500_7_5,test)
#plot(pred_200_6,test)
rmse_500_7_5 = sqrt(mean((test$price - (pred_500_7_5))^2))
rmse_500_7_5   ##We get an RMSE of 6281.207
importance(forest_500_7_5)
varImpPlot(forest_500_7_5) 
plot(forest_500_7_5)


###########################ntrees=700, mtry=7########################################

set.seed(3)
forest_700_7 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                                fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=700, mtry=7, importance=TRUE)
forest_700_7
# Mean of squared residuals: 52682925
# % Var explained: 97.38
pred_700_7 = predict(forest_700_7,test)
#plot(pred_200_6,test)
rmse_700_7 = sqrt(mean((test$price - (pred_700_7))^2))
rmse_700_7   ##We get an RMSE of 6272.531
importance(forest_700_7)
varImpPlot(forest_700_7) 
plot(forest_700_7)


###########################ntrees=1000, mtry=7########################################
set.seed(3)
forest_1000_7 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                              fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=1000, mtry=7, importance=TRUE)
forest_1000_7
# Mean of squared residuals: 52611565
# % Var explained: 97.39
# % Var explained: 97.38
pred_1000_7 = predict(forest_1000_7,test)
#plot(pred_200_6,test)
rmse_1000_7 = sqrt(mean((test$price - (pred_1000_7))^2))
rmse_1000_7   ##We get an RMSE of 6269.006
importance(forest_1000_7)
varImpPlot(forest_1000_7) 
plot(forest_1000_7)


###########################ntrees=1000, mtry=5########################################
set.seed(3)
forest_1000_5 = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +subTrim+wheelSize+
                               fuel+soundSystem + wheelType + featureCount+condition+state+region, data = train, ntree=1000, mtry=5, importance=TRUE)
forest_1000_5
# Mean of squared residuals: 51978039
# % Var explained: 97.42
pred_1000_5 = predict(forest_1000_5,test)
#plot(pred_200_6,test)
rmse_1000_5 = sqrt(mean((test$price - (pred_1000_5))^2))
rmse_1000_5   ##We get an RMSE of 6269.35
importance(forest_1000_5)
varImpPlot(forest_1000_5) 
plot(forest_1000_5)


###########################ntrees=1000, mtry=5, limited variables########################################
set.seed(3)
forest_1000_5_lv = randomForest(price~ trim+isOneOwner+ mileage+color+displacement+ year +
                               fuel+soundSystem + wheelType + featureCount+condition, data = train, ntree=1000, mtry=5, importance=TRUE)
forest_1000_5_lv
# Mean of squared residuals: 52486620
# % Var explained: 97.39

pred_1000_5_lv = predict(forest_1000_5_lv,test)
#plot(pred_200_6,test)
rmse_1000_5_lv = sqrt(mean((test$price - (pred_1000_5_lv))^2))
rmse_1000_5_lv   ##We get an RMSE of 6390.024
importance(forest_1000_5_lv)
varImpPlot(forest_1000_5_lv) 
plot(forest_1000_5_lv)



#######Random Forest with Lasso variables#####################################################################
###########################ntrees=500, mtry=8, limited variables########################################
set.seed(3)
levels(Cars_X_out$X)<-levels(train$X)
levels(Cars_X_out$trim)<-levels(train$trim)
levels(Cars_X_out$displacement)<-levels(train$displacement)
levels(Cars_X_out$fuel)<-levels(train$fuel)
levels(Cars_X_out$soundSystem)<-levels(train$soundSystem)
levels(Cars_X_out$condition)<-levels(train$condition)
levels(Cars_X_out$wheelType)<-levels(train$wheelType)
levels(Cars_X_out$mileage)<-levels(train$mileage)
levels(Cars_X_out$year)<-levels(train$year)
levels(Cars_X_out$subTrim)<-levels(train$subTrim)
levels(Cars_X_out$color)<-levels(train$color)
levels(Cars_X_out$state)<-levels(train$state)
levels(Cars_X_out$region)<-levels(train$region)
levels(Cars_X_out$wheelSize)<-levels(train$wheelSize)
levels(Cars_X_out$isOneOwner)<-levels(train$isOneOwner)
levels(Cars_X_out$featureCount)<-levels(train$featureCount)


forest_500_8_lv = randomForest(price~ trim+ mileage+displacement+ year +
                                  fuel+soundSystem + wheelType + condition, data = train, ntree=500, mtry=8, nodesize=20, importance=TRUE)

forest_500_8_lv
# Call:
#   randomForest(formula = price ~ trim + mileage + displacement +      year + fuel + soundSystem + wheelType + condition, data = train,      ntree = 500, mtry = 8, nodesize = 20, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 8
# 
# Mean of squared residuals: 53466774
# % Var explained: 97.34
pred_500_8_lv = predict(forest_500_8_lv,test)
#plot(pred_200_6,test)
rmse_500_8_lv = sqrt(mean((test$price - (pred_500_8_lv))^2))
rmse_500_8_lv   ##We get an RMSE of 6390.024
importance(forest_500_8_lv)
varImpPlot(forest_500_8_lv) 
plot(forest_500_8_lv)


#######Random Forest with Lasso variables.v1#####################################################################
###########################ntrees=500, mtry=7, limited variables########################################
set.seed(3)
forest_500_8_lvV1 = randomForest(price~ trim+ mileage+displacement+ year +
                                 fuel + wheelType + condition, data = train, ntree=500, mtry=7, nodesize=20, importance=TRUE)
forest_500_8_lvv1
#############################OUT SAMPLE###############################################
pred_500_8_lv_test = predict(forest_500_8_lv,test_new)
test_new
fix(test_new)
