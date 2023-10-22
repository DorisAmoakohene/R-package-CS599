library(data.table)
library(stats)
library(mvtnorm)


# Function to initialize GMM parameters using K-Means

#' Fitting a GMM to a data using the EM algorithm
#'
#' @param data The input data for GMM initialization
#' @param K The number of clusters in the GMM
#'
#' @return a list containing the initial parameter for each cluster in the GMM
#' @export
#'
#' @examples
#'
initialize_gmm <- function(data, K) {

  # Initialize with K-Means
  n.clusters <- K


  kmeans.result <- kmeans(data, n.clusters)
  data.dt <- data.table::data.table(data, cluster=factor(kmeans.result[["cluster"]]))
  prob.mat <- matrix(
    0, nrow(data), n.clusters,
    dimnames=list(data=1:nrow(data), cluster=NULL))

  prob.mat[cbind(1:nrow(data), kmeans.result$cluster)] <- 1

  cluster.param.list <- list()

  for(cluster in 1:n.clusters){
    prob.vec <- prob.mat[,cluster]
    these.params <- list(
      prior.weight=mean(prob.vec),
      mean.vec=colSums(prob.vec * data)/sum(prob.vec))
    diff.mat <- t(data)-these.params$mean.vec
    these.params$cov.mat <-
      diff.mat %*% (prob.vec * t(diff.mat))/sum(prob.vec)

    cluster.param.list[[cluster]] <- these.params
  }

  return(cluster.param.list)

}



#' Perform the E-step of the EM algorithm for the GMM
#'
#' @param data The input step for the E-step
#' @param model GMM model with initial parameters
#'
#' @return matrix of responsibilities indicating the probablity of each data point belonging to each cluster
#' @export
#'
#' @examples
e_step <- function(data, model) {

  n.clusters <- length(model)

  # Cluster probabilities updated

  numerator.mat = matrix(NA, nrow(data), n.clusters)
  for(cluster in 1:n.clusters){
    numerator.mat[,cluster] = with(
      model[[cluster]],
      prior.weight*mvtnorm::dmvnorm(
        data, mean.vec, cov.mat))
  }
  prob.mat <- numerator.mat/rowSums(numerator.mat)

  return(prob.mat)


}


# Function to update the M-step of the EM algorithm

#' perform the M-step of the EM algorithm for the GMM
#'
#' @param data input data for the M-step
#' @param responsibilities matrix obtained from the E-step
#'
#' @return list containing the updated parameter for each cluster in the GMM
#' @export
#'
#' @examples
m_step <- function(data, responsibilities) {

  n.clusters <- ncol(responsibilities)

  cluster.param.list <- list()

  for(cluster in 1:n.clusters){
    prob.vec <- responsibilities[,cluster]
    these.params <- list(
      prior.weight=mean(prob.vec),
      mean.vec=colSums(prob.vec * data)/sum(prob.vec))
    diff.mat <- t(data)-these.params$mean.vec
    these.params$cov.mat <-
      diff.mat %*% (prob.vec * t(diff.mat))/sum(prob.vec)

    cluster.param.list[[cluster]] <- these.params
  }

  return(cluster.param.list)



}

# Function to fit a GMM using EM

#' Title
#'
#' @param data input data for GMM fitting
#' @param K the number of Clusters in the GMM
#' @param max_iter maximum number of iteration for the EM algorithm
#' @param tol the convergence tolerance for the EM algorithm
#'
#' @return list of model parameter, log likelihood, cluster assignments, etc.
#' @export
#'
#' @examples
#'
#'
#'
#' # Load the required libraries
#' library(mvtnorm)
#' library(data.table)
#' library(stats)

# Generate some random dataset
#'set.seed(123)
#'data <- rbind(
#'mvtnorm::rmvnorm(100, mean = c(0, 0), sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)),
#'mvtnorm::rmvnorm(100, mean = c(3, 3), sigma = matrix(c(1, -0.5, -0.5, 1), nrow = 2))
#')
#'
# Fit the GMM using your function
#'result <- GMM(data, K = 2)
#'# Print the result
#'print(result)
#'
#'
#'
GMM <- function(data, K, max_iter = 1000, tol = 1e-6) {
  # Function implementation
}
GMM <- function(data, K, max_iter = 1000, tol = 1e-6) {

  n.clusters <- K

  # Initialize GMM parameters

  model <- initialize_gmm(data, K)


  for (iter in 1:max_iter) {

    # Perform E-step
    responsibilities <- e_step(data, model)


    # Store the current model for convergence check
    old_model <- model

    # Perform M-step
    model <- m_step(data, responsibilities)

    # Check for convergence

    meanss.new <- c()
    meanss.old <- c()
    for(i in 1:K){

      meanss.new <- rbind(meanss.new,model[[i]]$mean.vec)
      meanss.old <- rbind(meanss.old,old_model[[i]]$mean.vec)
    }



    if(sum((meanss.new - meanss.old)^2, na.rm = T) < tol) {
      break
    }
  }

  # Compute the log likelihood


  ll.mat = matrix(NA, nrow(data), n.clusters)

  for(cluster in 1:n.clusters){
    ll.mat[,cluster] = with(
      model[[cluster]],
      prior.weight*mvtnorm::dmvnorm(
        data, mean.vec, cov.mat))
  }

  loglik <- data.table(log.lik=sum(log(rowSums(ll.mat)),na.rm = T))



  # Assign data points to clusters

  cluster_assignments <- apply(responsibilities, 1, which.max)

  # Create a result list similar to mclust::Mclust

  result <- list(
    modelName = "DIAG",
    loglik = loglik,
    classification = cluster_assignments,
    parameters = model
  )

  return(result)
}


