library(tibble)
library(ggplot2)
library(tidyr)
library(dplyr)
library(invgamma)
library(ggforce)
library(rjags)

set.seed(100)
#given values in table
Y = c(64,72,55,27,75,24,28,66,40,13)
n = c(75,95,63,39,83,26,41,82,54,16)
q = c(0.845, 0.847, 0.880, 0.674, 0.909, 0.898, 0.770, 0.801, 0.802, 0.875)
N=10
#parameters to start MCMC
m = 0;
theta = q; 
iters = 3*10^4;
burn= 10^4; 
MCMC=matrix(0,iters-burn,N+1); # save only after burn-in
can_sd = 1
#log posterior for m
log_post_m = function(theta, Y, n, q, m){
  like = 0
  for(i in 1:10){
    like = like + dbeta(theta[i],exp(m)*q[i],exp(m)*(1-q[i]),log = TRUE )
  }
  prior = dnorm(m,0,sqrt(10),log=TRUE)
  return(like + prior)}

for(iter in 1:iters){
  # Gibbs for each theta
  for(i in 1:N){
    alpha = Y[i] + exp(m) * q[i]
    beta = n[i] - Y[i] + exp(m) * (1 - q[i])
    theta[i] = rbeta(1, alpha, beta)
  }
  # Metropolis for m
  can = rnorm(1, m, can_sd) #proposal distribution
  logR = log_post_m(theta, Y, n, q, can) - log_post_m(theta, Y, n, q, m) 
  R=exp(logR)
  if(runif(1) < R){m = can}
  if(iter>burn){ 
    MCMC[iter - burn, ] = c(theta,m)}
}

acc_rate = mean(MCMC[2:(iters-burn),11]!=MCMC[1:(iters-burn-1),11])
MCMC=data.frame(MCMC) 
colnames(MCMC)=c("Russell Westbrook","James Harden","Kawhi Leonard","LeBron James",
                 "Isaiah Thomos","Stephen Curry","Giannis Antetokounmpo","John Wall",
                 "Anthony Davis","Kevin Durant","m")

MCMC%>%
  pivot_longer(cols = "Russell Westbrook":"m", names_to = "Parameters", 
               values_to = "Posterior_Distributions")%>%
  ggplot(aes(x=rep(seq(burn+1,iters), 11), y = Posterior_Distributions))+
  xlab("Iterations")+
  geom_line(size=.1)+theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(" Trace plot of the MCMC samples of each posteriors")+
  facet_wrap_paginate(~Parameters,scales = "free", ncol = 2, nrow = 2, page = 1)

MCMC%>%
  pivot_longer(cols = "Russell Westbrook":"m", names_to = "Parameters", 
               values_to = "Posterior_Distributions")%>%
  ggplot(aes(x=rep(seq(burn+1,iters), 11), y = Posterior_Distributions))+
  xlab("Iterations")+
  geom_line(size=.1)+theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(" Trace plot of the MCMC samples of each posteriors")+
  facet_wrap_paginate(~Parameters,scales = "free", ncol = 2, nrow = 2, page = 2)

MCMC%>%
  pivot_longer(cols = "Russell Westbrook":"m", names_to = "Parameters", 
               values_to = "Posterior_Distributions")%>%
  ggplot(aes(x=rep(seq(burn+1,iters), 11), y = Posterior_Distributions))+
  xlab("Iterations")+
  geom_line(size=.1)+theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(" Trace plot of the MCMC samples of each posteriors")+
  facet_wrap_paginate(~Parameters,scales = "free", ncol = 2, nrow = 2, page = 3)
```
```{r,echo=TRUE,out.width="50%"}
plot(seq(burn+1,iters),MCMC[ ,11],type="l",xlab="Iteration",ylab="Sample for m",
     main=paste("Acceptance prob =",round(acc_rate,2)))


table=sapply(MCMC, quantile,  probs = c(.5, 0.025, 0.975))
rownames(table) = c("Means","2.5 %","97.5 %") 
knitr::kable(t(table))

#given data
Y = c(64,72,55,27,75,24,28,66,40,13)
n = c(75,95,63,39,83,26,41,82,54,16)
q= c(0.845, 0.847, 0.880, 0.674, 0.909, 0.898, 0.770, 0.801, 0.802, 0.875)
N = 10
# define string model
model_string = textConnection("model{
   # Likelihood
    for(i in 1:N){
      Y[i] ~ dbin(theta[i], n[i])
    }
   # Priors
    for(i in 1:N){
      theta[i] ~ dbeta(exp(m)*q[i], exp(m)*(1-q[i]))
    }
    
    m   ~  dnorm(0, 0.1)
 }")
# Load the data and compile the MCMC code
inits = list(theta=q, m = 0)
data  = list(Y = Y, n = n, q = q, N= N)
model = jags.model(model_string,data = data, inits=inits, n.chains=2)
#Burn-in for 10000 samples
update(model, 10000, progress.bar="none")
# Generate 20000 post-burn-in samples


params  = c("theta","m")
samples = coda.samples(model, 
                       variable.names=params, 
                       n.iter=20000, progress.bar="none")

summary(samples)

plot(samples)

