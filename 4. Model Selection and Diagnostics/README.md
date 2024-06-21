
This repository contains various techniques for selecting and comparing statistical models, especially within a Bayesian framework.

\section*{Outline}

\subsection*{1. Methods for Model Comparison}
\begin{itemize}
    \item \textbf{Bayes Factors (BF):} Gold standard for hypothesis testing in Bayesian statistics.
    \item \textbf{Stochastic Search Variable Selection (SSVS):} Bayesian method analogous to stepwise variable selection.
    \item \textbf{Cross-Validation (CV):} Common method for model validation and selection.
    \item \textbf{Information Criteria:}
    \begin{itemize}
        \item \textbf{Akaike Information Criteria (AIC):}
        \item \textbf{Bayesian Information Criteria (BIC):}
        \item \textbf{Deviance Information Criteria (DIC):}
        \item \textbf{Watanabe-Akaike Information Criteria (WAIC):}
    \end{itemize}
\end{itemize}

![Bayesian-STATISTICS](./model_diag.jpeg)

\subsection*{2. Posterior Predictive Checks}
\begin{itemize}
    \item Evaluates model fit by comparing observed data to data simulated from the model.
\end{itemize}

\section*{Key Concepts}

\subsection*{Bayes Factors (BF)}
\begin{itemize}
    \item Used to compare models by calculating the ratio of the posterior probabilities of the models.
    \item A high BF indicates strong evidence in favor of one model over another.
\end{itemize}

\subsection*{Stochastic Search Variable Selection (SSVS)}
\begin{itemize}
    \item Places priors on all possible models using variable inclusion indicators.
    \item MCMC is used to approximate the posterior probabilities of each model.
\end{itemize}

\subsection*{Cross-Validation (CV)}
\begin{itemize}
    \item Splits the data into K groups and iteratively trains and tests the model.
    \item Measures prediction accuracy to select the best model.
\end{itemize}

\subsection*{Information Criteria}
\begin{itemize}
    \item \textbf{AIC:} Penalizes models based on the number of parameters to avoid overfitting.
    \item \textbf{BIC:} Similar to AIC but includes a stronger penalty for the number of parameters.
    \item \textbf{DIC:} Popular Bayesian alternative to AIC/BIC, taking into account the posterior distribution.
    \item \textbf{WAIC:} Approximation to leave-one-out CV, theoretically justified and often similar to DIC.
\end{itemize}

\subsection*{Posterior Predictive Checks}
\begin{itemize}
    \item Uses the posterior predictive distribution to evaluate model fit.
    \item Bayesian p-value helps determine if the model fits the observed data well.
\end{itemize}

\section*{Requirements}
To view and run the examples provided, you will need:
\begin{itemize}
    \item \textbf{PDF Reader:} To view the \texttt{Bayesian\_computation.pdf} file.
    \item \textbf{R and RStudio:} To run the R code and view the \texttt{Bayesian\_computation.Rmd} file.
    \item \textbf{Web Browser:} To view the \texttt{Bayesian\_computation.html} file.
    \item \textbf{R Packages:}
    \begin{itemize}
        \item \texttt{rjags:} For running MCMC methods.
        \item \texttt{ggplot2:} For creating visualizations.
        \item \texttt{tidyverse:} For data manipulation.
        \item \texttt{ggforce:} For enhanced \texttt{ggplot} functionalities, including \texttt{facet\_wrap\_paginate}.
    \end{itemize}
\end{itemize}

You can install the required R packages using the following commands:

\begin{verbatim}
install.packages(c("rjags", "ggplot2", "tidyverse", "ggforce"))
\end{verbatim}
