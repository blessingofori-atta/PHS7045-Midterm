bootstrap <- function(data, statistic, nboot = 1000, return_samples = FALSE) {
  if (!is.function(statistic))
    stop("'statistic' must be a function")

  n <- if (is.data.frame(data) || is.matrix(data)) nrow(data) else length(data)

  bootstrap_samples <- if (return_samples) vector("list", nboot) else NULL
  bootstrap_statistics <- numeric(nboot)

  for (i in seq_len(nboot)) {
    indices <- sample(seq_len(n), size = n, replace = TRUE)
    bsample <- if (is.data.frame(data) || is.matrix(data)) data[indices, , drop = FALSE] else data[indices]
    if (return_samples) bootstrap_samples[[i]] <- bsample
    bootstrap_statistics[i] <- statistic(bsample)
  }

  list(
    thetastar = bootstrap_statistics,
    bootstrap_samples = if (return_samples) bootstrap_samples else NULL
  )
}


