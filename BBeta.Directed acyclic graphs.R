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