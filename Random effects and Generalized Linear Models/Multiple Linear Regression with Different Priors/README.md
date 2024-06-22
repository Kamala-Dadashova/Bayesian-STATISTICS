# Bayesian Multiple Linear Regression

This repository contains implementations of multiple linear regression using different priors. The provided files include detailed examples, explanations, and data used for the analyses.

## Files

- **Model1.pdf**, **Model2.pdf**, **Model3.pdf**: These documents explain the methodology, models, and results of the multiple linear regression analyses with different priors, evaluate predictive posterior distribution, and generalized linear model, respectively.
- **Model1.Rmd**, **Model2.Rmd**, **Model3.Rmd**: R Markdown files that contain the code and explanations to reproduce the analyses and results shown in `Model1.pdf`, `Model2.pdf`, and `Model3.pdf`.
- **homes.RData**: The dataset used in `Model1.Rmd`.
- **election_2008_2016.RData**: The dataset used in `Model2.Rmd`.

### Multiple Linear Regression with Different Priors
- **Gaussian Priors**: Utilizing Gaussian distributions as priors in the Bayesian linear regression model.
- **Jeffreys' Priors**: Applying non-informative priors for objective Bayesian analysis.
- **Shrinkage Priors**: Using priors that induce shrinkage to enhance model robustness.

### Posterior Predictive Distribution
The posterior predictive distribution allows us to predict new data points based on our Bayesian model, incorporating the uncertainty of the model parameters.

- **Incorporates Uncertainty**: Accounts for the uncertainty in the parameter estimates.
- **Predictive Power**: Provides a distribution of possible outcomes, not just a single point estimate.



