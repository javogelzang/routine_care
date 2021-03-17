#Bioinformatics 11-3
library(glmnet)
library(caret)
library(boot)
library(affy)

setwd("Documents/ADS Master/Routine Care/Lab Sessions")
df_expr <- read.delim("exprsVijver.txt")
df_phen <- read.delim("pdatavijver.txt")
description <- read.table('pdataVijver.txt', sep = '\t')

df_expr_t <- t(df_expr)
y <- df_phen$event_death
analysis_frame <- data.frame(df_expr_t,y=as.factor(y))

x_features <- model.matrix(y ~ ., analysis_frame)

cv.lasso <- cv.glmnet(x - x_features, y - analysis_frame$y, alpha - 1, family = 'binomial')
coef(cv.lasso, cv.lasso$lambda.min)
