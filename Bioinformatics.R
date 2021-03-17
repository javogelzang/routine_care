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

cv.lasso <- cv.glmnet(x - x_features, y - analysis_frame$y, alpha - 1, family = 'binomial')
coef(cv.lasso, cv.lasso$lambda.min)
