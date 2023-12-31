% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mclust.R
\name{GMM}
\alias{GMM}
\title{Title}
\usage{
GMM(data, K, max_iter = 1000, tol = 1e-06)
}
\arguments{
\item{data}{input data for GMM fitting}

\item{K}{the number of Clusters in the GMM}

\item{max_iter}{maximum number of iteration for the EM algorithm}

\item{tol}{the convergence tolerance for the EM algorithm}
}
\value{
list of model parameter, log likelihood, cluster assignments, etc.
}
\description{
Title
}
\examples{



# Load the required libraries
library(mvtnorm)
library(data.table)
library(stats)
set.seed(123)
data <- rbind(
mvtnorm::rmvnorm(100, mean = c(0, 0), sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)),
mvtnorm::rmvnorm(100, mean = c(3, 3), sigma = matrix(c(1, -0.5, -0.5, 1), nrow = 2))
)

result <- GMM(data, K = 2)
# Print the result
print(result)



}
