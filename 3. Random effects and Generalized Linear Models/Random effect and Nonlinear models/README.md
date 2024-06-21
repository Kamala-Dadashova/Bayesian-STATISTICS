
This repository contains detailed implementations and analyses for various Bayesian models, focusing on linear regression with different priors and spatial modeling of correlated data. The provided files include comprehensive examples, explanations, and the datasets used in the analyses.

- **Model5.pdf**: One-way random effects model for jaw data.
- **Model6.pdf**: Bayesian linear mixed model for the jaw data
- **Model7.pdf**: Spatial modeling of gun-related homicide rates.
- **Model8.pdf**: Advanced Bayesian analysis for correlated data.

## Topics Covered

### Model5.pdf: One-way Random Effects Model
- **One-way random effects model**: Analysis of jaw bone density data using a hierarchical Bayesian model.
- **Data loading and plotting**: Instructions to load and visualize the dataset.
- **Model fitting with Gamma priors**: Fitting the random effects model with Gamma priors.
- **Model fitting with half-Cauchy priors**: Comparison of results with half-Cauchy priors.
- **Prior sensitivity analysis**: Examining the impact of different priors on the model.
- **Comparison with naive model**: Evaluating the advantages of the random effects model over a simple model.

### Model7.pdf: Spatial Modeling of Gun-related Homicide Rates
- **Data from Kalesan et al. (2016)**: Analysis based on firearm legislation and mortality data in the USA.
- **Non-spatial linear model**: Fitting a standard linear regression model.
- **Spatial linear model**: Incorporating spatial dependence using conditionally-autoregressive models.
- **Adjacency matrix creation**: Generating spatial structure for the data.
- **Model comparison**: Evaluating the differences between spatial and non-spatial models.

### Model8.pdf: Advanced Bayesian Analysis for Correlated Data
- **Hierarchical models**: Advanced techniques for analyzing correlated data.
- **Gibbs Sampling**: Implementation and utilization in Bayesian computation.
- **Markov Chain Monte Carlo (MCMC)**: Detailed explanation of MCMC methods for parameter estimation.
- **Convergence diagnostics**: Techniques for ensuring the reliability of MCMC results.

## Requirements

- **PDF Reader**: To view the provided PDF documents.
- **R and RStudio**: For running the R scripts and viewing the `.Rmd` files.
- **R Packages**: Install the necessary R packages with the following command:
  ```r
  install.packages(c("rjags", "ggplot2", "tidyverse", "ggforce", "lme4", "ggthemes", "maps", "sf", "spdep", "rmapshaper"))

