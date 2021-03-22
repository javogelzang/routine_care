library(rJava)
library(Hmisc)
library(rms)
library(ROCR)
library(rmda)

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

#Q2: repeat the training test split 100 times and take the average of auc and calibration slope on the test data sets, to be less prone to chance findings
#Q3: make a better calibrated (less overfitted) model
