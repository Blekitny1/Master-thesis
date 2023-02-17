
X = matrix(rnorm(500000), 500, 100)
svd(X, nu = 500, nv = 500)$d
dim(svd(X, nu = 500, nv = 500)$u)
dim(svd(X, nu = 500, nv = 500)$v)



# REAL DATA - Continous Case

library(bigstep)
library(L0Learn)
library('Rcpp')

load("Sangerdata.Rdata")

y = as.numeric(data[24266,-1])
x = t(as.matrix(data[-24266,-1]))

quan = quantile(as.vector(x), probs = 0.25)
out1 = which(apply(x, 2, max) < quan)
length(out1)
x1 = x[, -out1]

range = apply(x1, 2, max) - apply(x1, 2, min)
out2 = which(range < 2)
length(out2)
x2 = x1[, -out2]
xx = x2
p = dim(xx)[2]
n = dim(xx)[1]
X = as.matrix(xx)
X = matrix(X, ncol = ncol(X), dimnames = NULL)
colnames(X) = colnames(xx)
rownames(X) = rownames(xx)

y = scale(y)
X = scale(X)/sqrt(n)

results.CD = L0Learn.fit(X, y, loss = "SquaredError", penalty = "L0",
                         algorithm = "CD", maxSuppSize = 30, nLambda = 200, nGamma = 10,
                         gammaMax = 10, gammaMin = 1e-03, intercept = TRUE)

cvfit = L0Learn.cvfit(X, y, loss = "SquaredError", penalty = "L0", nFolds = 10,
                         algorithm = "CD", maxSuppSize = 30, nLambda = 200, nGamma = 10,
                         gammaMax = 10, gammaMin = 1e-03, intercept = FALSE)

res.CD = print(results.CD)
res.CD

optimalLambdaIndex = which.min(as.vector(cvfit$cvMeans[[1]]))
optimalLambdaIndex
optimalLambda = cvfit$fit$lambda[[1]][optimalLambdaIndex]
optimalLambda

vcv = as.vector(coef(cvfit, lambda=optimalLambda, gamma=0))
significant = which(vcv != 0)
significant
vcv[significant]

Xdwa = X[,significant]#via L0Learn
summary(lm(y ~ Xdwa))
#str(lm(y ~ Xdwa))
sum(lm(y ~ Xdwa)$residuals^2)
cor(cbind(y, Xdwa))

Xjeden = X[,c(206, 682, 1004, 1354, 1370)]#via mbic2
summary(lm(y ~ Xjeden))
sum(lm(y ~ Xjeden)$residuals^2)
cor(cbind(y, Xjeden))

