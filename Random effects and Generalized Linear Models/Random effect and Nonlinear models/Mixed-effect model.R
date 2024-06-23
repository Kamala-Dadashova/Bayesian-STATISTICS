library(rjags)
library(ggplot2)
m   = 4
n   = 20
age = c(8.0, 8.5, 9.0, 9.5)
Y   = c(47.8, 48.8, 49.0, 49.7,
        46.4, 47.3, 47.7, 48.4,
        46.3, 46.8, 47.8, 48.5,
        45.1, 45.3, 46.1, 47.2,
        47.6, 48.5, 48.9, 49.3,
        52.5, 53.2, 53.3, 53.7,
        51.2, 53.0, 54.3, 54.5,
        49.8, 50.0, 50.3, 52.7,
        48.1, 50.8, 52.3, 54.4,
        45.0, 47.0, 47.3, 48.3,
        51.2, 51.4, 51.6, 51.9,
        48.5, 49.2, 53.0, 55.5,
        52.1, 52.8, 53.7, 55.0,
        48.2, 48.9, 49.3, 49.8,
        49.6, 50.4, 51.2, 51.8,  
        50.7, 51.7, 52.7, 53.3,
        47.2, 47.7, 48.4, 49.5,
        53.3, 54.6, 55.1, 55.3,
        46.2, 47.5, 48.1, 48.4,
        46.3, 47.6, 51.3, 51.8) 


Y = matrix(Y, n, m, byrow = TRUE)
# Convert the matrix to a data frame for ggplot
df = data.frame(age = rep(age, each = n), 
                id = rep(1:n, times = m), 
                bone_density = as.vector(t(Y)))
ggplot(df, aes(x = age, y = bone_density, group = id)) +
  geom_line() +
  geom_point() +
  labs(x = "Age", y = "Bone density") +
  theme_minimal()


data     = list(Y=Y,age=age,n=n,m=m)
burn     = 10000
n.iter   = 20000
thin     = 15
n.chains = 2



library(rjags)
model_string = textConnection("model{
   # Likelihood
    for(i in 1:n){for(j in 1:m){
      Y[i,j] ~ dnorm(alpha[i,1]+alpha[i,2]*age[j],taue)
    }}

   # Random effects
    for(i in 1:n){alpha[i,1:2] ~ dmnorm(mu[1:2],Omega[1:2,1:2])}

   # Priors
    for(j in 1:2){mu[j] ~ dnorm(0,0.0001)}
    taue ~ dgamma(0.1,0.1)
    Omega[1:2,1:2] ~ dwish(R[,],2.1)

    R[1,1]<-1/2.1
    R[1,2]<-0
    R[2,1]<-0
    R[2,2]<-1/2.1
 }")

params  = c("mu","alpha","taue","Omega")
model   = jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples = coda.samples(model, variable.names=params,
                       n.iter=n.iter, thin=thin, progress.bar="none")
samples = rbind(samples[[1]],samples[[2]])
Omega   = samples[,1:4]
a1      = samples[,5:24]
a2      = samples[,25:44]
mu      = samples[,45:46]
sig     = 1/sqrt(samples[,47])
S       = Omega
for(i in 1:nrow(S)){
  S[i,]<-as.vector(solve(matrix(Omega[i,],2,2)))
} 

r = S[,2]/sqrt(S[,1]*S[,4])
hist(r,breaks=50,prob=TRUE,main="",xlab="Correlation between random slopes and intercepts")


##Predictions###

# Pick three subjects and define parameters
selected_subjects =c(1, 11, 12)
num_ages = 10
age_range =seq(8, 10, length = num_ages)


plot(NA, xlim = range(age_range), ylim = c(45, 60), xlab = "Age", ylab = "Bone density")

# Loop over selected subjects to plot the data
for (subject in selected_subjects) {
  
  # Calculate posterior of the mean for each age
  fit = sapply(age_range, function(age) a1[, subject] + age * a2[, subject])
  quantiles = apply(fit, 2, quantile, c(0.025, 0.5, 0.975))
  
  # Plot observed data points
  points(age, Y[subject, ], pch = which(selected_subjects == subject))
  
  # Plot the quantile lines
  lines(age_range, quantiles[1, ], lty = 2)
  lines(age_range, quantiles[2, ], lty = 1)
  lines(age_range, quantiles[3, ], lty = 2)
  
  # Posterior predictive distribution at age 10
  Y10 =a1[, subject] + a2[, subject] * 10 + rnorm(length(sig), 0, sig)
  q_Y10 = quantile(Y10, c(0.025, 0.975))
  
  # Plot the posterior predictive interval at age 10
  lines(c(10, 10), q_Y10, lty = which(selected_subjects == subject))
  lines(10 + 0.05 * c(-1, 1), rep(q_Y10[1], 2), lty = which(selected_subjects == subject))
  lines(10 + 0.05 * c(-1, 1), rep(q_Y10[2], 2), lty = which(selected_subjects == subject))
}

legend("topleft", paste("Patient", 1:3), pch = 1:3, cex = 1.5, bty = "n")
