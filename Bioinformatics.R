#Bioinformatics 11-3
library(glmnet)
library(caret)
library(boot)
library(affy)

#Load the data 
setwd("Documents/ADS Master/Routine Care/Lab Sessions")
df_expr <- read.table("exprsVijver.txt", sep = '\t', header = TRUE,
                      check.names = FALSE)
df_expr <- as.matrix(df_expr)
df_phen <- read.table("pdatavijver.txt", sep = '\t', header = TRUE,
                      check.names = FALSE)
description <- read.table('pdataVijver.txt', sep = '\t')

#Check whether the order of samples in the expression data is the same as in the phenodata
table(colnames(df_expr)==row.names(df_phen))

#Make dataframe for use in the caret package
vijverdata <- as.data.frame(t(df_expr))
vijverdata$event_death <- df_phen$event_death

#Lasso penalised logistic regression using glmnet
lasso.fit <- glmnet(x = t(df_expr), y = vijverdata$event_death, family = 'binomial', alpha = 1)
plot(lasso.fit)

#Ridge penalised logistic regression using glmnet
ridge.fit <- glmnet(x = t(df_expr), y = vijverdata$event_death, family = 'binomial', alpha = 0)
plot(ridge.fit)

#Cross-validation k-fold lasso to find optimal lambda
cvlasso <- cv.glmnet(x = t(df_expr), y = vijverdata$event_death, family = 'binomial',
                         keep = TRUE, alpha = 1)
plot(cvlasso)

#Cross-validation k-fold ridge to find optimal lambda
cvridge <- cv.glmnet(x = t(df_expr), y = vijverdata$event_death, family = 'binomial',
                         keep = TRUE, alpha = 0)
plot(cvridge)

#Select genes with optimal lambda
beta.min <- cvlasso$glmnet.fit$beta[,cvlasso$index["min",]]
beta.min[beta.min!=0]

#selected genes with 1se lambda
beta.1se <- cvlasso$glmnet.fit$beta[,cvlasso$index["1se",]]
beta.1se[beta.1se!=0]

#Compare
beta.min.1se <- cbind(beta.min,beta.1se)[beta.min!=0,]
beta.min.1se

#Tuning: crossvalidation to find optimal lambda. loss function misclassification error
cvlasso.class = cv.glmnet(x=t(df_expr),
                          y=vijverdata$event_death,
                          family = "binomial",
                          alpha=1,
                          type.measure="class")
plot(cvlasso.class)

#Elastic net using the caret package/ default loss: accuracy
cv_5 = trainControl(method = "cv", number = 5)

enet1 = train(
  event_death ~ ., data = vijverdata,
  method = "glmnet",
  trControl = cv_5,
  #tuneLength = 10
)
enet1

#Simply extract the best result (or tuning parameters) using a quick helper function.
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(enet1)

#SVM using the caret package/ default loss: accuracy
cv_5 = trainControl(method = "cv", number = 5)

svmlin1 = train(
  event_death ~ ., data = vijverdata,
  method = "svmLinear",
  trControl = cv_5,
  tuneGrid = data.frame(C=c(0.0001,0.001,0.01,0.1,1,10) )
)
svmlin1

#Use Recursive Feature Elimination to find the simplest set of genes predicting best in repeated cross validation
svmProfile <- rfe(x=t(df_expr),
                  y=vijverdata$event_death,
                  sizes = c(2, 5, 10, 20),
                  rfeControl = rfeControl(functions = lmFuncs,
                                          method = "repeatedcv",
                                          repeats = 5,
                                          verbose = FALSE),
                  method = "svmLinear")

svmProfile
plot(lmProfile, type = c("o"))