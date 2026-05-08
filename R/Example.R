library(MASS)		# generate multivariate normal random numbers
library(glmnet)	# implement Lasso

## generate cov matrix with AR structure
## p: dimension of cov matrix generated
AR = function(r, p)
{
	cov_design = array(0, dim = c(p,p))
	for(i in 1:p)
	{
		for(j in 1:i)
      	{
   			cov_design[i,j] = r^(abs(i-j))
			if(abs(cov_design[i,j]) < 1e-3)
			{
				cov_design[i,j] = 0
			}
			cov_design[j,i] = cov_design[i,j]
      	}
	}
	return(cov_design)
}


###############################
#####  Simulation #############
###############################
n = 150
p = 100				## include intercept
cap_sigma = AR(r = 0.5, p-1)	## covariance matrix of multi-normal
beta_true = rep(0, p)
beta_true[1:10] = runif(10, min = 2, max = 5)	
sigma_true = sqrt(2)





## Generate data matrix X and response y
data = mvrnorm(n, rep(0,p-1), cap_sigma)    # use package MASS
X = cbind(rep(1,n), data)					# add intercept
eps = rnorm(n, mean = 0, sd = sigma_true)	# error term
y = X %*% beta_true + eps

	
## Lasso
glmmod = glmnet(X[,-1], y, family = 'gaussian', alpha = 1)
lambda = cv.glmnet(X[,-1], y, nfolds=5, alpha = 1)$lambda.min
as.vector(coef(glmmod, s = lambda))  # Lasso coefficient estimate


## LSE
fit1 = lm(y ~ X[,-1])
summary(fit1)
as.numeric(coef(fit1))	# LSE coefficient estimate

