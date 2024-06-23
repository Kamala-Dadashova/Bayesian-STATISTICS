# Load required libraries
library(choroplethr)
library(choroplethrMaps) # Required for county_choropleth
# Load the dataset
load("/Users/kamaladadashova/Documents/DoctoralCourses/Applied Bayesian Statistics/Lecture Notes with Assignments/Week 7 Model Comparisions/election_2008_2016.RData")
# Standardize the covariates and add an intercept
X = scale(X)
X = cbind(Intercept = 1, X)
# Define short names for the covariates
short_names = c("Intercept", "Pop change", "65+", "African American",
                "Hispanic", "HS grad", "Bachelor's",
                "Homeownership rate", "Home value",
                "Median income", "Poverty")
colnames(X) = short_names

# Define a function to create county maps
county_plot = function(fips, Y, main = "", units = "") {
  data = data.frame(region = fips, value = Y)
  county_choropleth(data, title = main, legend = units)
}
# Plot the map
county_plot(fips, Y, main = "Percent increase in GOP support", units = "")


# Plot the map for the Bachelor's covariate (X[,7])
county_plot(fips, X[,7], main = "Bachelor's", units = "")

# Remove AK, HI and DC due to missing data
set.seed(5656)
state = as.character(all_dat[,3])
AKHI  = state=="AK" | state=="HI" | state=="DC"
fips  = fips[!AKHI]
Y     = Y[!AKHI]
X     = X[!AKHI,]
state = state[!AKHI]
# Assign a numeric id to the counties in each state
st    = unique(state)
id    = rep(NA,length(Y))
for(j in 1:48){
  id[state==st[j]]=j
}
n = length(Y) # number of counties
N = 48        # number of states
p = ncol(X)   # number of features
iters = 50000
burn  = 10000

##Model 1###

model1_string = "model{

   # Likelihood
   for(i in 1:n){
      Y[i]   ~ dnorm(mu[i],taue)
      mu[i] <- inprod(X[i,],beta[])
   }
   # Priors
   for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
   taue ~ dgamma(0.1,0.1)
   sig <- 1/sqrt(taue)

   # WAIC calculations
   for(i in 1:n){
     like[i]    <- dnorm(Y[i],mu[i],taue)
   }
 }"

library(rjags)

# Load the model
dat    = list(Y=Y,n=n,X=X,p=p)
init   = list(beta=rep(0,p))
model1 = jags.model(textConnection(model1_string),n.chains=2,
                    inits=init,data = dat,quiet=TRUE)
# Generate samples
update(model1, burn, progress.bar="none")
samp1   = coda.samples(model1, 
                       variable.names="beta", 
                       n.iter=iters, progress.bar="none")

# Compile results
ESS1    = effectiveSize(samp1)
out1    = summary(samp1)$quantiles
rownames(out1)=short

# Compute DIC
dic1    = dic.samples(model1,n.iter=iters,progress.bar="none")

# Compute WAIC
waic1   = coda.samples(model1, 
                       variable.names=c("like"), 
                       n.iter=iters, progress.bar="none")
like1   = waic1[[1]]
fbar1   = colMeans(like1)
P1      = sum(base::apply(log(like1),2,var))
WAIC1   = -2*sum(log(fbar1))+2*P1

###Model 2###

model2_string = "model{

   # Likelihood
   for(i in 1:n){
      Y[i]    ~ dnorm(mnY[i],taue)
      mnY[i] <- inprod(X[i,],beta[id[i],])
   }

   # Slopes
   for(j in 1:p){for(i in 1:N){
       beta[i,j] ~ dnorm(0,0.01)
   }}

   # Priors
   taue ~ dgamma(0.1,0.1)


   # WAIC calculations
   for(i in 1:n){
     like[i]    <- dnorm(Y[i],mnY[i],taue)
   }
  }"

# Load the model
dat    = list(Y=Y,n=n,N=N,X=X,p=p,id=id)
init   = list(beta=matrix(0,N,p))
model2 = jags.model(textConnection(model2_string),n.chains=2,
                    inits=init,data = dat,quiet=TRUE)

# Generate samples
update(model2, burn, progress.bar="none")
samp2  = coda.samples(model2, 
                      variable.names="beta", 
                      n.iter=iters, progress.bar="none")

# Compile results  
ESS2     = effectiveSize(samp2)
sum      = summary(samp2)$stat
post_mn2 = matrix(sum[,1],N,p)
post_sd2 = matrix(sum[,2],N,p)

# Compute DIC
dic2   = dic.samples(model2,n.iter=iters,progress.bar="none")

# Compute WAIC
waic2   = coda.samples(model2, 
                       variable.names=c("like"), 
                       n.iter=iters, progress.bar="none")
like2   = waic2[[1]]
fbar2   = colMeans(like2)
P2      = sum(base::apply(log(like2),2,var))
WAIC2   = -2*sum(log(fbar2))+2*P2

###Model 3###

model3_string = "model{

   # Likelihood
   for(i in 1:n){
      Y[i]    ~ dnorm(mnY[i],taue)
      mnY[i] <- inprod(X[i,],beta[id[i],])
   }

   # Random slopes
   for(j in 1:p){
     for(i in 1:N){
       beta[i,j] ~ dnorm(mu[j],taub[j])
     }
     mu[j]    ~ dnorm(0,0.01)
     taub[j]  ~ dgamma(0.1,0.1)
   }

   # Priors
   taue ~ dgamma(0.1,0.1)


   # WAIC calculations
   for(i in 1:n){
     like[i]    <- dnorm(Y[i],mnY[i],taue)
   }
  }"


# Load the model
dat    = list(Y=Y,n=n,N=N,X=X,p=p,id=id)
init   = list(beta=matrix(0,N,p))
model3 = jags.model(textConnection(model3_string),n.chains=2,
                    inits=init,data = dat,quiet=TRUE)

# Generate samples
update(model3, burn, progress.bar="none")
samp3  = coda.samples(model3, 
                      variable.names="beta", 
                      n.iter=iters, progress.bar="none")

# Compile results
ESS3     = effectiveSize(samp3)
sum      = summary(samp3)$stat
post_mn3 = matrix(sum[,1],N,p)
post_sd3 = matrix(sum[,2],N,p)

# Compute DIC
dic3    = dic.samples(model3,n.iter=iters,progress.bar="none")

# Compute WAIC
waic3   = coda.samples(model3, 
                       variable.names=c("like"), 
                       n.iter=iters, progress.bar="none")
like3   = waic3[[1]]
fbar3   = colMeans(like3)
P3      = sum(base::apply(log(like3),2,var))
WAIC3   = -2*sum(log(fbar3))+2*P3

ESS1

hist(ESS2)

hist(ESS3)

library(kableExtra)
kbl(round(out1,2))

dic1
dic2
dic3

WAIC1; P1
WAIC2; P2
WAIC3; P3

boxplot(post_mn3, main = "Boxplot of post_mn3", ylab = "Values", col = "lightblue")

# Posterior mean
county_plot(fips,post_mn3[id,7],
            main="Proportion of college graduates - posterior mean")


# Posterior sd
county_plot(fips,post_sd3[id,7],
            main="Proportion of college graduates - posterior SD")




