#' Generate Bootstrap Samples and Statistics
#'
#' This function generates bootstrap replicates and provides key statistics.
#' @param data A vector, matrix, or data frame for bootstrapping.
#' @param statistic A user-supplied function that computes a statistic on the data.
#' @param nboot Number of bootstrap replicates (default: 1000).
#' @param return_samples Logical; whether to return bootstrap samples (default: TRUE).
#' @return A list containing the original statistic, bootstrap replicates, and optionally, the bootstrap samples.
#' @export

bootstrap <- function(data, statistic, nboot = 1000, return_samples = TRUE) {
  # Ensure statistic is a function
  if (!is.function(statistic)) stop("'statistic' must be a function")

  # Determine sample size
  n <- if (is.data.frame(data) || is.matrix(data)) nrow(data) else length(data)

  # Compute observed statistic
  thetahat <- statistic(data)

  # Initialize storage
  bootstrap_samples <- if (return_samples) vector("list", nboot) else NULL
  bootstrap_statistics <- numeric(nboot)

  # Generate bootstrap samples and compute statistics
  for (i in seq_len(nboot)) {
    indices <- sample(seq_len(n), size = n, replace = TRUE)
    bsample <- if (is.data.frame(data) || is.matrix(data)) data[indices, , drop = FALSE] else data[indices]
    if (return_samples) bootstrap_samples[[i]] <- bsample
    bootstrap_statistics[i] <- statistic(bsample)
  }

  # Return as a class object
  structure(
    list(
      thetahat = thetahat,
      thetastar = bootstrap_statistics,
      bsamples = if (return_samples) bootstrap_samples else NULL,
      data = data,
      statistic = statistic
    ),
    class = "bootstrap_result"
  )
}


