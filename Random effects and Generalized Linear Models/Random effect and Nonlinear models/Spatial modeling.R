set.seed(0820)
load("~/guns.RData")

Y     = log(10000*Y/N)
Z[,1] = log(Z[,1])
X     = cbind(1,Z,rowSums(X))
# Remove AK and HI
Y = Y[-c(2,11)]
X = X[-c(2,11),]
n = length(Y)
p = ncol(X)

library(rjags)
library(ggplot2)
ns_model = "model{
   # Likelihood
   for(i in 1:n){
      Y[i]   ~ dnorm(mu[i],taue)
      mu[i] <- inprod(X[i,],beta[])
   }
   # Priors
   for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
   taue ~ dgamma(0.1,0.1)
   sig <- 1/sqrt(taue)

 }"

dat    = list(Y=Y,n=n,X=X,p=p)
init   = list(beta=rep(0,p))
model1 = jags.model(textConnection(ns_model),
                    inits=init,data = dat,quiet=TRUE)
update(model1, 10000, progress.bar="none")
samp1   = coda.samples(model1, 
                       variable.names=c("beta","sig"), 
                       n.iter=20000, progress.bar="none")
summary(samp1)


library(maps)
library(sf)
library(spdep)
library(rmapshaper)

# Create the USA state map
usa.state = map(database = "state", fill = TRUE, plot = FALSE)

# Convert to an sf object
usa.sf = st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Clean the geometries to fix any issues
usa.sf = st_make_valid(usa.sf)

# If there are still issues, simplify the geometries
usa.sf = ms_simplify(usa.sf, keep_shapes = TRUE)

# Create neighborhood structure
usa.nb = poly2nb(usa.sf)

# Convert to adjacency matrix
A = nb2mat(usa.nb, style = "B")

# Remove DC (8th row/column)
A = A[-8, ]
A = A[, -8]

# Create the diagonal matrix
M = diag(rowSums(A))

sp_model = "model{

   # Likelihood
   for(i in 1:n){
      Y[i]  ~ dnorm(mu[i]+S[i],taue)
   }
   S[1:n] ~ dmnorm(zero[1:n],taus*Omega[1:n,1:n])
   for(i in 1:n){
      mu[i]   <- inprod(X[i,],beta[])
      zero[i] <- 0
   }
   Omega[1:n,1:n]<-M[1:n,1:n]-rho*A[1:n,1:n]

   # Priors
   for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
   taue ~ dgamma(0.1,0.1)
   taus ~ dgamma(0.1,0.1)
   rho  ~ dunif(0,1)
   sig[1] <- 1/sqrt(taue)
   sig[2] <- 1/sqrt(taus)
  }"

dat    = list(Y=Y,n=n,X=X,A=A,M=M,p=p)
init   = list(rho=0.99,beta=lm(Y~X-1)$coef)
model2 = jags.model(textConnection(sp_model),
                    inits=init,data = dat,quiet=TRUE)
update(model2, 10000, progress.bar="none")
samp2  = coda.samples(model2, 
                      variable.names=c("beta","rho","sig"), 
                      n.iter=20000, progress.bar="none")

summary(samp2)


rho = samp2[[1]][,8]
hist(rho,breaks=100)

b1  = samp1[[1]][,7]
b2  = samp2[[1]][,7]
r   = c(-0.035,0.015)
# Combine data into a data frame for ggplot2
data <- data.frame(
  Beta = c(b1, b2),
  Group = factor(rep(c("Non-spatial", "Spatial"), c(length(b1), length(b2))))
)
ggplot(data, aes(x = Beta, color = Group, linetype = Group)) +
  geom_density(adjust = 1.5) +
  xlim(r) +
  labs(x = "Beta", y = "Posterior density") +
  theme_minimal() +
  theme(legend.position = "topright")

mean(b1<0)
mean(b2<0)

