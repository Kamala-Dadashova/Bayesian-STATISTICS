load("/guns.RData")
X  <- rowSums(X)
n  <- length(Y)


# (1) Poisson regression

model_string1 <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i]           ~ dpois(lambda[i])
    log(lambda[i]) <- log(N[i]) + alpha + inprod(Z[i,],beta[1:5]) + X[i]*beta[6]
  }

  #Priors
   for(j in 1:6){
      beta[j] ~ dnorm(0,0.1)
   }
   alpha ~ dnorm(0,0.1)


  # Posterior preditive checks
  for(i in 1:n){
    Y2[i]    ~ dpois(lambda[i])
    rate[i] <- Y2[i]/N[i]
  }
  D[1] <- min(Y2[])
  D[2] <- max(Y2[])
  D[3] <- max(Y2[])-min(Y2[])
  D[4] <- min(rate[])
  D[5] <- max(rate[])
  D[6] <- max(rate[])-min(rate[])

 }"


# (2) Over-dispersed Poisson
model_string2 <- "model{

  # Likelihood (note hierarchical centering)
  for(i in 1:n){
    Y[i]            ~ dnegbin(q[i],m)
    q[i]           <- m/(m+N[i]*lambda[i])
    log(lambda[i]) <- alpha + inprod(Z[i,],beta[1:5]) + X[i]*beta[6]
  }

  #Priors
   for(j in 1:6){
      beta[j] ~ dnorm(0,0.1)
   }
   alpha ~ dnorm(0,0.1)
   m     ~ dgamma(0.1,0.1)

  # Posterior preditive checks
  for(i in 1:n){
    Y2[i]    ~ dnegbin(q[i],m)
    rate[i] <- Y2[i]/N[i]
  }

  D[1] <- min(Y2[])
  D[2] <- max(Y2[])
  D[3] <- max(Y2[])-min(Y2[])
  D[4] <- min(rate[])
  D[5] <- max(rate[])
  D[6] <- max(rate[])-min(rate[])

 }"



###Fit the models

library(rjags)
model1 <- jags.model(textConnection(model_string1), 
                     data = list(Y=Y,N=N,n=n,X=X,Z=Z),n.chains=1,
                     quiet=TRUE)
update(model1, 10000, progress.bar="none")
samps1 <- coda.samples(model1, 
                       variable.names=c("D","beta"), 
                       n.iter=20000, progress.bar="none")
plot(samps1)
print(summary(samps1))



D1  <- samps1[[1]]
model2 <- jags.model(textConnection(model_string2), 
                     data = list(Y=Y,N=N,n=n,X=X,Z=Z),n.chains=1,
                     quiet=TRUE)
update(model2, 10000, progress.bar="none")
samps2 <- coda.samples(model2, 
                       variable.names=c("D","beta"), 
                       n.iter=20000, progress.bar="none")
plot(samps2)



print(summary(samps2))
D2  <- samps2[[1]]


##Bayesian p-values

# Compute rate as the ratio of Y to N
rate <- Y / N

# Create a vector with min, max, and range for Y and rate
D0 <- c(min(Y), max(Y), diff(range(Y)),
        min(rate), max(rate), diff(range(rate)))

# Define names for the vector D0
Dnames <- c("Min Y", "Max Y", "Range Y", "Min rate", "Max rate", "Range rate")

# Initialize vectors for p-values
pval1 <- numeric(6)
names(pval1) <- Dnames
pval2 <- pval1

# Loop through each element to compute test statistics and plot densities
for (j in 1:6) {
  # Plot density for D1
  plot(density(D1[, j]), xlim = range(c(D0[j], D1[, j], D2[, j])),
       xlab = "D", ylab = "Posterior probability", main = Dnames[j])
  
  # Add density line for D2
  lines(density(D2[, j]), col = 2)
  
  # Add a vertical line for the value in D0
  abline(v = D0[j], col = 3)
  
  # Add a legend to the plot
  legend("topleft", legend = c("Poisson", "NB", "Data"), lty = 1, col = 1:3, bty = "n")
  
  # Compute p-values
  pval1[j] <- mean(D1[, j] > D0[j]) 
  pval2[j] <- mean(D2[, j] > D0[j])  
}

# Print p-values for inspection
print(pval1)
print(pval2)


