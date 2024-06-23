# Load libraries and data
library(knitr)
library(kableExtra)
library(rjags)
library(maps)
library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(purrr)
load("~/homes.RData")
homes_data = as.data.frame(homes)
column_names = names(homes_data)
print(column_names)
column_types =sapply(homes_data, class)
print(column_types)

##Covariates(features)
city     = homes[,2]
state    = homes[,3]
lat      = homes[,4]
long     = homes[,5]
temp     = homes[,6]
precip   = homes[,7]
NPP      = homes[,8]
elev     = homes[,9]
house    = ifelse(homes[,10]=="One-family house detached from any other house",1,0)
bedrooms = as.numeric(homes[,11])


#Data Prep
OTU      = as.matrix(OTU)
nspecies = rowSums(OTU>0)
Y        = log(nspecies)
X        = cbind(long,lat,temp,precip,NPP,elev,house,bedrooms)
names    = c("Longitude","Latitude",
             "Temperature","Precipitation","NPP",
             "Elevation","Single-family home",
             "Number of bedrooms")

#Remove observations with missing data
#Combine the data into a data frame
data    = data.frame(Y, X, city, state)
#Filter out rows with any NA values
complete_rows = complete.cases(data)
Y = Y[complete_rows]
X = X[complete_rows, ]
city = city[complete_rows]
state = state[complete_rows]

#Standardize the covariates
X = as.matrix(scale(X))


# Plot the sample locations
homes_data = as.data.frame(homes)
states_map =map_data("state")
ggplot() +
  geom_polygon(data = states_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = homes_data, aes(x = homes_data[, 5], y = homes_data[, 4]), color = "blue", size = 0.5) +
  ggtitle("Sample locations") +
  theme_minimal()


n        = length(Y)
p        = ncol(X)
data   = list(Y=Y,X=X,n=n,p=p)
params = c("beta")
burn     = 10000
n.iter   = 20000
thin     = 10
n.chains = 2 #parallel chains

#### Fit the uninformative Gaussian model###
model_string = textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
    }
   # Priors
    for(j in 1:p){
      beta[j] ~ dnorm(0,0.001)
    }
    alpha ~ dnorm(0,0.001)
    taue  ~ dgamma(0.1, 0.1)
 }")
model = jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples1 = coda.samples(model, variable.names=params, thin=thin, n.iter=n.iter, progress.bar="none")
par(mar=c(2,2,2,2))
plot(samples1)
round(effectiveSize(samples1),1)

sum                      = summary(samples1)
rownames(sum$statistics) = names
rownames(sum$quantiles)  = names
sum$statistics           = round(sum$statistics,3)
sum$quantiles            = round(sum$quantiles,3)
sum


### Fit the Gaussian shrinkage model###

model_string = textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
    }
   # Priors
    for(j in 1:p){
      beta[j] ~ dnorm(0,taue*taub)
    }
    alpha ~ dnorm(0,0.001)
    taue  ~ dgamma(0.1, 0.1)
    taub  ~ dgamma(0.1, 0.1)
 }")

model = jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples2 = coda.samples(model, variable.names=params, thin=thin, n.iter=n.iter, progress.bar="none")
par(mar=c(2,2,2,2))
plot(samples2)
round(effectiveSize(samples2),1)


sum                      = summary(samples2)
rownames(sum$statistics) = names
rownames(sum$quantiles)  = names
sum$statistics           = round(sum$statistics,3)
sum$quantiles            = round(sum$quantiles,3)
sum


###Fit the Bayesian LASSO model###
model_string = textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
    }
   # Priors
    for(j in 1:p){
      beta[j] ~ ddexp(0,taue*taub)
    }
    alpha ~ dnorm(0,0.001)
    taue  ~ dgamma(0.1, 0.1)
    taub  ~ dgamma(0.1, 0.1)
 }")

model = jags.model(model_string,data = data, n.chains=n.chains,quiet=TRUE)
update(model, burn, progress.bar="none")
samples3 = coda.samples(model, variable.names=params, thin=thin, n.iter=n.iter, progress.bar="none")
par(mar=c(2,2,2,2))
plot(samples3)

round(effectiveSize(samples3),1)
sum                      = summary(samples3)
rownames(sum$statistics) = names
rownames(sum$quantiles)  = names
sum$statistics           = round(sum$statistics,3)
sum$quantiles            = round(sum$quantiles,3)
sum


###Comparisons


# Create an empty list to store the data frames
df_list <- list()

for(j in 1:p){
  s1 <- c(samples1[[1]][,j], samples1[[2]][,j])
  s2 <- c(samples2[[1]][,j], samples2[[2]][,j])
  s3 <- c(samples3[[1]][,j], samples3[[2]][,j])
  
  # Smooth densities
  d1 <- density(s1)
  d2 <- density(s2)
  d3 <- density(s3)
  
  # Convert density objects to data frames
  df1 <- data.frame(x = d1$x, y = d1$y, group = "s1")
  df2 <- data.frame(x = d2$x, y = d2$y, group = "s2")
  df3 <- data.frame(x = d3$x, y = d3$y, group = "s3")
  
  # Combine data frames
  df_combined <- rbind(df1, df2, df3)
  df_combined$parameter <- names[j]
  
  # Store in list
  df_list[[j]] <- df_combined
}

# Combine all data frames in the list into one data frame
df_final <- do.call(rbind, df_list)

ggplot(df_final, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  facet_wrap(~ parameter, scales = "free_y") +
  labs(x = expression(beta), y = "Posterior density") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()




















