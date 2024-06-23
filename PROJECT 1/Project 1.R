# Load libraries
suppressPackageStartupMessages({
  library(tibble)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  ###(B)
  # Define constants and initial values
  n1=44700; Y1=20034; n2=45489; Y2=20119; a=1; b= 1
  
  # Generate theta sequence
  theta = seq(0.43, 0.46, 0.0001)
  
  # Compute beta density values for each theta
  theta1 = dbeta(theta, Y1 + a, n1 - Y1 + b)
  theta2 = dbeta(theta,  Y2 + a,  n2 - Y2 + b)
  
  # Create data frame for plotting
  df = data.frame(theta, theta1, theta2)
  
  # Rename columns for clarity
  colnames(df) = c("theta", "theta1", "theta2")
  
  # Transform data frame for ggplot
  # Transform data frame for ggplot
  df <- df %>%
    pivot_longer(cols = c("theta1", "theta2"), names_to = "Levels", values_to = "Posterior")
  
  # Plot the beta distributions with custom legend labels
  plt <- ggplot(df, aes(x = theta, y = Posterior, color = Levels, linetype = Levels)) +
    geom_line() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(x = expression(theta)) +
    scale_color_manual(labels = c("Level 30", "Level 40"), values = c("blue", "red")) +
    scale_linetype_manual(labels = c("Level 30", "Level 40"), values = c("solid", "dashed"))
  
  plot(plt)
  
  ##(C)
  theta1 =rbeta(1000000,Y1+a,n1-Y1+b) 
  theta2 =rbeta(1000000,Y2+a,n2-Y2+b) 
  mean(theta1>theta2) 
  
  ###(D)
  ab_vals= c(0.01,0.1,1,10,100) ;  p=ab_vals; options(scipen = 999)
  for(j in 1:length(ab_vals)){ theta1 =rbeta(1000000,Y1+ab_vals[j],n1-Y1+ab_vals[j]) 
  theta2=rbeta(1000000,Y2+ab_vals[j],n2-Y2+ab_vals[j]) 
  p[j]  <-mean(theta1>theta2) } 
  cbind(ab_vals,p)
  
  
  ###Problem 2
  
  ###A
  
  # Likelihood: Y_j ~ Binom(n_j,theta_j) j=1,2
  # Prior: theta_j~Beta(a,b) 
  # Inputs: Y1,Y2,n1,n2,a,b 
  # Output: group to be sample, i.e.,  1 or 2
  # Thompson Sampling Function
  Thompson_sampling = function(Y1, n1, Y2, n2, a = 1, b = 1) {
    theta1_sample = rbeta(1, a + Y1, b + n1 - Y1)
    theta2_sample = rbeta(1, a + Y2, b + n2 - Y2)
    if (theta1_sample > theta2_sample) {
      return(1) # Level 30
    } else {
      return(2) # Level 40
    }
  }
  
###B
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  
  # Initial parameters
  days = 1000
  p =c(20034 / 44700, 20119 / 45489) # True probabilities for levels 30 and 40
  a=b=1 # Prior hyperparameters
  
  # Initialize matrices to store results
  n =Y =matrix(0, days + 1, 2)
  
  # Simulation loop for 1000 days
  for (t in 1:days) {
    set.seed(919 * t) # Set seed for reproducibility
    
    # Thompson Sampling to pick the level
    level= Thompson_sampling(Y[t, 1], n[t, 1], Y[t, 2], n[t, 2], a, b)
    
    # Generate new data based on selected level
    y = rbinom(1, 100, p[level])
    
    # Update counts for the selected level
    Y[t + 1, ] = Y[t, ]
    n[t + 1, ] = n[t, ]
    
    if (level == 1) {
      Y[t + 1, 1] = Y[t, 1] + y
      n[t + 1, 1] = n[t, 1] + 100
    } else {
      Y[t + 1, 2] = Y[t, 2] + y
      n[t + 1, 2] = n[t, 2] + 100
    }
  }
  # Remove initial row (time 0)
  Y = Y[-1, ]
  n = n[-1, ]
  
  # Calculate posterior means
  posterior_mean= (Y + a) / (n + a + b)
  
  # Calculate cumulative sampling proportions
  cum_n = cbind(n[, 1] / (n[, 1] + n[, 2]), n[, 2] / (n[, 1] + n[, 2]))
  
  # Prepare data for plotting
  results = data.frame(
    day = 1:days,
    theta1_mean = posterior_mean[, 1],
    theta2_mean = posterior_mean[, 2],
    cum_prop_30 = cum_n[, 1],
    cum_prop_40 = cum_n[, 2]
  )
  
  # Plot posterior means of theta1 and theta2
  ggplot(results, aes(x = day)) +
    geom_line(aes(y = theta1_mean, color = "Theta 1 Mean")) +
    geom_line(aes(y = theta2_mean, color = "Theta 2 Mean")) +
    labs(title = "Posterior Means of Theta1 and Theta2",
         x = "Day", y = "Posterior Mean", color = "Theta") +
    theme_minimal()
  
  # Plot cumulative sampling proportions
  ggplot(results, aes(x = day)) +
    geom_line(aes(y = cum_prop_30, color = "Level 30")) +
    geom_line(aes(y = cum_prop_40, color = "Level 40")) +
    labs(title = "Cumulative Proportion of Days Given Each Level",
         x = "Day", y = "Cumulative Proportion", color = "Level") +
    theme_minimal()
  
  ###C
  
  
  
  
  
  