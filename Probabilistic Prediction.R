library(rJava)
library(Hmisc)
library(rms)
library(ROCR)
library(rmda)
library(glmnet)

heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg","thalach","exang", "oldpeak","slope", "ca", "thal", "hd")

#Look at the data
heart.data

#Change the outcome hd into 0-1
heart.data$hd[heart.data$hd > 0] <- 1

#Make factor variables when appropriate
convert.type <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}
chclass <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","numeric")

heart.data <- convert.type(heart.data,chclass)

#Descriptive statistics
Hmisc::describe(heart.data)

# Using complete data, run the logistic regression.
heart.fit=glm((hd==1)~.,data=heart.comp, family="binomial")
summary(heart.fit)

#Create training-test split
set.seed(597)   # use set.seed to get the same results, each time you run the code
train_rows=sample(1:nrow(heart.comp), 153) 
summary(heart.comp[train_rows,])

#Fit full model on the training data
heart.fit=glm(hd~.,data=heart.comp[train_rows,], family="binomial")
summary(heart.fit)

#Calibration plot and ROC curve on the test data.
pred.test <- predict(heart.fit,newdata=heart.comp[-train_rows,],type="response")
heart.pred = prediction(pred.test,heart.comp[-train_rows,]$hd)
heart.perf = performance(heart.pred,"tpr","fpr")

#Compute area under ROC curve
auc <- performance(heart.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
plot(heart.perf, main="ROC Curve for logistic model on the test data",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#Calibration plot, through the function val.prob from the rms package
v <- val.prob(p=pred.test,y=heart.comp[-train_rows,]$hd==1)
c(auc=auc,cal.slope=v["Slope"])

#Decision curve analysis on the test data:
library(rmda)
testdata <- heart.comp[-train_rows,]
testdata$predicted.hd <-predict(heart.fit,newdata=testdata,type="response")

test.decisioncurve <- decision_curve(hd~predicted.hd,
                                     data = testdata,
                                     fitted.risk = TRUE, 
                                     thresholds = seq(0, 1, by = .05),
                                     bootstraps = 25)


plot_decision_curve(test.decisioncurve, legend.position = "topright")

#Q1: check how much ovefitting there is, by determining AUC, calibration and decision curve on the training data
#Severe overfitting as can be seen in the calibration plot. 
pred.train_test <- predict(heart.fit,newdata=heart.comp[train_rows,],type="response")
heart.train_pred = prediction(pred.train_test,heart.comp[train_rows,]$hd)
heart.train_perf = performance(heart.train_pred,"tpr","fpr")

#Compute the ROC with AUC
auc_train <- performance(heart.train_pred,"auc")
auc_train <- unlist(slot(auc_train, "y.values"))
plot(heart.train_perf, main="ROC Curve for logistic model on the test data",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#Compute the calibration plot
v <- val.prob(p=pred.train_test,y=heart.comp[train_rows,]$hd==1)
c(auc=auc_train,cal.slope=v["Slope"])

#Compute the decision curve
train_data <- heart.comp[train_rows,]
train_data$predicted.hd <-predict(heart.fit,newdata=train_data,type="response")

train.decisioncurve <- decision_curve(hd~predicted.hd,
                                     data = train_data,
                                     fitted.risk = TRUE, 
                                     thresholds = seq(0, 1, by = .05),
                                     bootstraps = 25)


plot_decision_curve(train.decisioncurve, legend.position = "topright")

#Q2: repeat the training test split 100 times and take the average of auc and calibration slope on the test data sets, to be less prone to chance findings
#Only 4 values 1 for restecg attribute, delete values since this raise error
table(heart.comp$restecg)
heart.comp_w <- heart.comp[heart.comp$restecg!=1,]

#Repeat the test split 100 times
set.seed(789)
resultMat <- matrix(NA,nrow=100,ncol=2) # Matrix to store the results in
colnames(resultMat) <- c("auc","cal.slope")

for (i in (1:100)){
  train_rows=sample(1:nrow(heart.comp_w), 151) 
  heart.fit=glm(hd~.,data=heart.comp_w[train_rows,], family="binomial")
  pred.test <- predict(heart.fit,newdata=heart.comp_w[-train_rows,],type="response")
  heart.pred = prediction(pred.test,heart.comp_w[-train_rows,]$hd)
  #compute area under ROC curve
  auc <- performance(heart.pred,"auc")
  auc <- unlist(slot(auc, "y.values"))
  
  # calibration plot, through the function val.prob from the rms package
  v <- val.prob(p=pred.test,y=heart.comp_w[-train_rows,]$hd==1,pl=FALSE)
  resultMat[i,] <- c(auc=auc,cal.slope=v["Slope"])
}
#Plot the results
hist(resultMat[,1], main='AUC')
hist(resultMat[,2], main='Calibration slope')

#Calculate the means 
apply(resultMat, 2, 'mean')

#Q3: make a better calibrated (less overfitted) model
#Using ridge regression
set.seed(6723)
# use model.matrix to create dummy variables for factor levels. Have to remove the Intercept column 
cvridge = cv.glmnet(x=model.matrix(hd~.,heart.comp_w)[,-1], 
                    y=heart.comp_w$hd,
                    family = "binomial",
                    alpha=0,keep=TRUE)
plot(cvridge)
pred.cv <- predict(cvridge,newx=model.matrix(hd~.,heart.comp_w)[,-1],s=cvridge$lambda.min,type="response")
heart.pred.cv = prediction(pred.cv,heart.comp_w$hd)
heart.perf.cv = performance(heart.pred.cv,"tpr","fpr")
#compute area under ROC curve
auc.cv <- performance(heart.pred.cv,"auc")
auc.cv <- unlist(slot(auc.cv, "y.values"))
plot(heart.perf.cv, main="ROC Curve for logistic model on the test data",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# calibration plot, through the function val.prob from the rms package
v <- val.prob(p=pred.cv,y=heart.comp_w$hd==1)
c(auc=auc.cv,cal.slope=v["Slope"])

#Repeat 100 times
set.seed(597)   # use set.seed to get the same results, each time you run the code
resultMat.ridge <- matrix(NA,nrow=100,ncol=2) # Matrix to store the results in
colnames(resultMat.ridge) <- c("auc","cal.slope")
for (i in (1:100)){
  train_rows=sample(1:nrow(heart.comp_w), 151) 
  cvridge = cv.glmnet(x=model.matrix(hd~.,heart.comp_w[train_rows,])[,-1], 
                      y=heart.comp_w[train_rows,]$hd,
                      family = "binomial",
                      alpha=0,keep=TRUE)
  
  test.pred.ridge <- predict(cvridge,newx=model.matrix(hd~.,heart.comp_w[-train_rows,])[,-1], s=cvridge$lambda.min, type="response")
  test.pred1.ridge = prediction(test.pred.ridge,heart.comp_w[-train_rows,]$hd)
  #compute area under ROC curve
  auc.ridge <- performance(test.pred1.ridge,"auc")
  auc.ridge <- unlist(slot(auc.ridge, "y.values"))
  
  # calibration plot, through the function val.prob from the rms package
  v <- val.prob(p=test.pred.ridge,y=heart.comp_w[-train_rows,]$hd==1,pl=FALSE)
  resultMat.ridge[i,] <- c(auc=auc.ridge,cal.slope=v["Slope"])
}

#Plot the results
hist(resultMat.ridge[,1], main='AUC')
hist(resultMat.ridge[,2], main='Calibration slope')

#Calculate the means 
apply(resultMat.ridge, 2, 'mean')

#Repeat with lasso 
set.seed(597)   # use set.seed to get the same results, each time you run the code
resultMat.lasso <- matrix(NA,nrow=100,ncol=2) # Matrix to store the results in
colnames(resultMat.lasso) <- c("auc","cal.slope")
for (i in (1:100)){
  train_rows=sample(1:nrow(heart.comp_w), 151) 
  cvridge = cv.glmnet(x=model.matrix(hd~.,heart.comp_w[train_rows,])[,-1], 
                      y=heart.comp_w[train_rows,]$hd,
                      family = "binomial",
                      alpha=1,keep=TRUE)
  
  test.pred.ridge <- predict(cvridge,newx=model.matrix(hd~.,heart.comp_w[-train_rows,])[,-1], s=cvridge$lambda.min, type="response")
  test.pred1.ridge = prediction(test.pred.ridge,heart.comp_w[-train_rows,]$hd)
  #compute area under ROC curve
  auc.ridge <- performance(test.pred1.ridge,"auc")
  auc.ridge <- unlist(slot(auc.ridge, "y.values"))
  
  # calibration plot, through the function val.prob from the rms package
  v <- val.prob(p=test.pred.ridge,y=heart.comp_w[-train_rows,]$hd==1,pl=FALSE)
  resultMat.lasso[i,] <- c(auc=auc.ridge,cal.slope=v["Slope"])
}

#Plot the results
hist(resultMat.lasso[,1], main='AUC')
hist(resultMat.lasso[,2], main='Calibration slope')

#Calculate the means 
apply(resultMat.lasso, 2, 'mean')
apply(resultMat.ridge, 2, 'mean')