library(tibble)
library(ggplot2)
library(tidyr)
library(dplyr)
library(invgamma)
library(ggforce)
library(broom)
library(rjags)

###Problem1###
# Given parameters
N1 = 2820; Y1 = 563; N2 = 27; Y2 = 10
# Define string model
model_string = textConnection("model{
    # Likelihood
    Y1 ~ dpois(N1*lambda1)
    Y2 ~ dpois(N2*lambda2)
    # Priors
    lambda1 ~  dunif(0, 10)
    lambda2 ~  dunif(0, 10)
    r =lambda2/lambda1
 }")
# Initialize the parameters
inits      = list(lambda1= Y1/N1,lambda2 = Y2/N2)
# Load the data and compile the MCMC code
data       = list(N1 = N1,Y1 = Y1,N2 = N2,Y2 = Y2)
model      = jags.model(model_string,data = data, inits=inits, n.chains=2)
#Burn-in for 15000 samples
update(model, 15000, progress.bar="none")
params     = c("lambda1","lambda2","r")
samples    = coda.samples(model, 
                          variable.names=params, 
                          n.iter=30000, progress.bar="none")

plot(samples)

summary(samples)
effectiveSize(samples)
gelman.diag(samples)

###Problem2###

set.seed(100)
Y1 = c(2.0, -3.1, -1.0,0.2,0.3,0.4)
Y2 = c(-3.5, -1.6, -4.6,-0.9,-5.1,0.1)

Ybar1 = mean(Y1)
s21   = mean((Y1-Ybar1)^2)
n1    = length(Y1)


Ybar2 = mean(Y2)
s22   = mean((Y2-Ybar2)^2)
n2    =length(Y2)

# Posterior of the difference assuming equal variance
delta_hat =Ybar2-Ybar1
s2        =(n1*s21 + n2*s22)/(n1+n2)
scale     =sqrt(s2)*sqrt(1/n1+1/n2)
df        =n1+n2
cred_int  =delta_hat + scale*qt(c(0.025,0.975),df=df)
delta_hat
cred_int

# Posterior of delta assuming unequal variance using MC sampling
mu1      = Ybar1 + sqrt(s21/n1)*rt(1000000,df=n1)
mu2      = Ybar2 + sqrt(s22/n2)*rt(1000000,df=n2)
delta    = mu2-mu1

hist(delta,main="Posterior distribution of the difference in means",xlim = c(-9,4), breaks = 100)
quantile(delta,c(0.025,0.975)) # 95% credible set


data = list(n=6,Y1=Y1,Y2=Y2)

model_string = textConnection("model{

 # Likelihood
 for(i in 1:n){
   Y1[i] ~ dnorm(mu,tau)
   Y2[i] ~ dnorm(mu+delta,tau)
 }

 # Priors
 mu    ~  dnorm(0, 0.0001)
 delta ~  dnorm(0, 0.0001)
 tau   ~  dgamma(0.1, 0.1)
 sigma = 1/sqrt(tau)
}")

model   = jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
params  = c("delta")
samples = coda.samples(model, 
                       variable.names=params, 
                       n.iter=50000, progress.bar="none")
summary(samples)

###Problem 3###


Y = Boston%>%
  dplyr::select(medv)
Y = as.matrix(Y)
X = Boston%>%
  dplyr::select(-medv)
#Standardize covariates
X = scale(X)
X = cbind(1,X)
colnames(X)[1]="Intercept"
names=colnames(X)
#Load the data.
data = list(n=length(Y),p=ncol(X),Y=Y,X=X)
#define model string
model_string = textConnection("model{
# Likelihood
for(i in 1:n){
Y[i,] ~ dnorm(inprod(X[i,],beta[]),tau)
}
# Priors
beta[1]~dnorm(0, 0.0001)
for(j in 2:p){beta[j] ~ dnorm(0,taub*tau)}
tau ~ dgamma(0.1,0.1)
taub ~ dgamma(0.1, 0.1)
}")
model = jags.model(model_string, data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
params = c("beta")
samples = coda.samples(model, variable.names=params, n.iter=10000,progress.bar="none")

effectiveSize(samples)
gelman.diag(samples)

sum                      = summary(samples)
rownames(sum$statistics) = names
rownames(sum$quantiles)  = names
sum$statistics           = round(sum$statistics,4)
sum$quantiles            = round(sum$quantiles,4)
sum

ols_data = cbind(Y,X[,2:14])
ols_data = data.frame(ols_data)
ols_model = lm(medv~.-medv, data = ols_data)
#ols_model$coefficients
tidy(ols_model)


model_string = textConnection("model{
 # Likelihood
  for(i in 1:n){
    Y[i,] ~ dnorm(inprod(X[i,],beta[]),taue)
  }
 # Priors
 beta[1] ~ dnorm(0,0.001)
  for(j in 2:p){
    beta[j] ~ ddexp(0,taue*taub)
  }
  taue  ~ dgamma(0.1, 0.1)
  taub  ~ dgamma(0.1, 0.1)
}")

model = jags.model(model_string,data = data, n.chains = 2,quiet=TRUE)
params  = c("beta")
update(model, 10000, progress.bar="none")
samples2 = coda.samples(model, variable.names=params, n.iter=20000,progress.bar="none")


effectiveSize(samples2)
gelman.diag(samples2)
sum                      = summary(samples2)
rownames(sum$statistics) = names
rownames(sum$quantiles)  = names
sum$statistics           = round(sum$statistics,4)
sum$quantiles            = round(sum$quantiles,4)
sum

for(j in 2:14){
  # Collect the MCMC iteration from both chains for the two priors
  s1 = c(samples[[1]][,j],samples[[2]][,j])
  s2 = c(samples2[[1]][,j],samples2[[2]][,j])
  
  # Get smooth density estimate for each prior
  d1 = density(s1)
  d2 = density(s2)
  # Plot the density estimates
  mx = max(c(d1$y,d2$y))
  plot(d1$x,d1$y,type="l",ylim=c(0,mx),xlab=expression(beta),ylab="Posterior density",main=names[j])
  lines(d2$x,d2$y,lty=2)
  abline(v=0)
  legend(1, 95, legend=c("Uninformative Gaussian", "Bayesian LASSO"),
         col=c("red", "blue"), lty=1:2, cex=0.8)
}


Y_train = Y[1:500,]
Y_test  = Y[501:506,]
X_train = X[1:500,]
X_test  = X[501:506,]
n_train = length(Y_train)
n_test  = length(Y_test)
p       = ncol(X_train)

model_string <- textConnection("model{
   # Likelihood
  for(i in 1:no){
    Yo[i]   ~ dnorm(muo[i],inv.var)
    muo[i] = inprod(Xo[i,],beta[])
  }

  # Prediction
  for(i in 1:np){
    Y_test[i]  ~ dnorm(mup[i],inv.var)
    mup[i] = inprod(Xp[i,],beta[])
  }

  # Priors
beta[1] ~ dnorm(0,0.001)
for(j in 2:p){beta[j] ~ dnorm(0,taub*inv.var)}
taub ~ dgamma(0.1, 0.1)
inv.var   ~ dgamma(0.01, 0.01)
sigma     = 1/sqrt(inv.var)
}")

data  = list(Yo=Y_train,no=n_train,np=n_test,p=p,Xo=X_train,Xp=X_test)
model = jags.model(model_string, data = data)



update(model, 10000, progress.bar="none")
samp = coda.samples(model, 
                    variable.names=c("beta","sigma","Y_test"), 
                    n.iter=20000, progress.bar="none")

summary(samp[,-c(1:n_test)])


samps           = samp[[1]]
Y_test.samps    = samps[,1:n_test] 
beta.samps      = samps[,n_test+1:p]
sigma.samps     = samps[,ncol(samps)]


# Compute the posterior mean for the plug-in predictions  

beta.mn         = colMeans(beta.samps)
sigma.mn        = mean(sigma.samps)

# Plot the PPD and plug-in
for(j in 1:6){
  # Plug-in
  mu <- sum(X_test[j,]*beta.mn)
  y  <- rnorm(20000,mu,sigma.mn)
  plot(density(y),col=2,xlab="Y",main="PPD")
  
  # PPD
  lines(density(Y_test.samps[,j]))
  
  # Truth
  abline(v=Y_test[j],col=3,lwd=2)
  legend("topright",c("PPD","Plug-in","Truth"),col=1:3,lty=1,inset=0.05)
}


# plug-in 95% intervals
low1   =  X_test%*%beta.mn - 1.96*sigma.mn
high1  =  X_test%*%beta.mn + 1.96*sigma.mn
cover1 =  mean(Y_test>low1 & Y_test<high1)
mean(cover1)

# PPD 95% intervals
low2   =  apply(Y_test.samps,2,quantile,0.025)
high2  =  apply(Y_test.samps,2,quantile,0.975)
cover2 =  mean(Y_test>low2 & Y_test<high2)
mean(cover2)