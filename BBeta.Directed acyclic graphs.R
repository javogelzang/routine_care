#Confounding
C = rnorm(100000, 0, 1)
Beta.xc = 2
X = Beta.xc * C + rnorm(100000, 0, 1)
Beta.yc = 1.5
Beta.yx = 1
Y = Beta.yx * X + Beta.yc * C + rnorm(100000, 0, 1)

lm(Y ~ X)
lm(Y ~ X + C)

#Conditioning on an intermediate 
X <- rnorm(100000, 0, 1)
Beta.mx <- 2
M <- Beta.mx * X + rnorm(100000, 0, 1)
Beta.yx <- 1
Beta.ym <- 0.5
Y <- Beta.yx * X + Beta.ym * M + rnorm(100000, 0, 1)
lm(Y ~ X)
lm(Y ~ X + M)

#Collider stratification
X <- rnorm(100000, 0, 1)
Beta.yx <- 0.67
Y <- Beta.yx * X + rnorm(100000, 0, 1)
Beta.sx <- 2
Beta.sy <- 1
S <- Beta.sx * X + Beta.sy * Y + rnorm(100000, 0, 1)
lm(Y ~ X)
lm(Y ~ X + S)

#Propensity score analysis
data_PS <- read.delim("~/Documents/ADS Master/Routine Care/Self Study/data_PS.txt", comment.char="#")
summary(data_PS)

#Step 2: Construct a propensity score
mean(data_PS$age[data_PS$vacc==0]); mean(data_PS$age[data_PS$vacc==1])
table(data_PS$vacc, data_PS$sex)
table(data_PS$vacc, data_PS$cvd)
table(data_PS$vacc, data_PS$death)

#What is the odds ratio of the crude association between vaccination status and mortality?
fit <- glm(death ~ vacc, data = data_PS, family = 'binomial')
log.or <- fit$coef[2]
se.log.or <- sqrt(diag(vcov(fit))[2])
exp(c(log.or, log.or - 1.96 * se.log.or, log.or + 1.96 * se.log.or))

#Fit model 
fit <- glm(vacc ~ age + sex + cvd + cvd_drug + pulm + pulm_drug + DM + contact, data = data_PS, family = 'binomial')

#Obtain predicted value of the model  
PS <- fit$fitted.values
data_PS$PS = PS

#Compare the mean of the (un)exposed
mean(data_PS$PS[data_PS$vacc==0]); mean(data_PS$PS[data_PS$vacc==1])

#Split PS
n.cat	<- 5	# no. categories to split PS
PS_cat <- ceiling(rank(PS)*n.cat/length(PS)) # split PS
for (i in 1:max(PS_cat)){
  print(sapply(split(data_PS$age[PS_cat==i], data_PS$vacc[PS_cat==i]),mean))} 

#Step 3: Estimate the effect of influenza vaccination on mortality risk
fit <- glm(death ~ vacc + PS, family= 'binomial', data = data_PS)
log.or <- fit$coef[2]
se.log.or <- sqrt(diag(vcov(fit))[2])
exp(c(log.or, log.or - 1.96 * se.log.or, log.or + 1.96 * se.log.or))

fit <- glm(death ~ vacc + factor(PS_cat), family='binomial', data = data_PS)
log.or <- fit$coef[2]
se.log.or <- sqrt(diag(vcov(fit))[2])
exp(c(log.or, log.or - 1.96 * se.log.or, log.or + 1.96 * se.log.or))