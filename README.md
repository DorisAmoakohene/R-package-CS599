# R-package-CS599
CS599GMM.DA is a comprehensive package designed for my class in CS599.The package will provide a range of functions for the Guassian Mixture Models(GMM) and other related tasks.
This will aid users in performing various operations such as fitting GMM models  and estimating model parameters. 


To install my GMM Package
remotes::install_github("DorisAmoakohene/R-package-CS599")

Usage:
library(gmm)

generate data
gmm_model
print results

Generate some random dataset
set.seed(123)
data <- rbind(
mvtnorm::rmvnorm(100, mean = c(0, 0), sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)),
mvtnorm::rmvnorm(100, mean = c(3, 3), sigma = matrix(c(1, -0.5, -0.5, 1), nrow = 2))

# Fit the GMM using your function
#'result <- GMM(data, K = 2)
#'# Print the result
#'print(result)

