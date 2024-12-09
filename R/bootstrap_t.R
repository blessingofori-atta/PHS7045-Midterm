#' Compute Bootstrap Confidence Intervals
#'
#' Computes confidence intervals using:
#' `boott-plugin`:bootstrap-t, equal-tailed with plugin standard error,
#' `boott-nested`:bootstrap-t, equal-tailed with nested bootstrap (this is when there is no plugin se),
#' `bootsym-plugin`:bootstrap-t, symmetric with plugin standard error,
#' `bootsym-nested`:bootstrap-t, symmetric with nested bootstrap (this is when there is no plugin se),
#' @param boot_obj An object created by the `bootstrap` function.
#' @param sdfun Optional; function to compute standard errors.
#' @param alpha Confidence level (default: 0.05).
#' @param method Confidence interval method: "boott-plugin", "boott-nested", "bootsym-plugin" or "bootsym-nested".
#' @param parallel A logical value indicating whether parallel processing should be used in the
#'                 bootstrap computation (default is `FALSE`).
#' @param cores The number of cores used for parallel processing if `parallel` is set to `TRUE`.
#'              This parameter is ignored if `parallel` is `FALSE`.
#' @param Bsd Number of bootstrap samples to estimate the standard error (default: 2).
#' @param ... Additional arguments to be passed to the `sdfun` function.
#' @return A list containing the confidence intervals, bias, and standard errors.
#' @export
#' @importFrom stats sd
#' @importFrom parallel makeCluster stopCluster parLapply
bootstrap_t <- function(boot_obj,
                        sdfun = NULL, Bsd = 2,
                        method = c("boott-plugin", "boott-nested", "bootsym-plugin", "bootsym-nested"),
                        alpha = 0.05,
                        parallel = FALSE,
                        cores = NULL,
                        ...) {
  # Validate inputs
  if (!inherits(boot_obj, "bootstrap_result")) stop("'boot_obj' must be of class 'bootstrap_result'.")
  if (!is.null(sdfun) && !is.function(sdfun)) stop("'sdfun' must be a function.")

  # Validate method argument
  method <- match.arg(method)

  # Ensure 'plugin' methods are only used if sdfun is not NULL
  if (method %in% c("boott-plugin", "bootsym-plugin") && is.null(sdfun)) {
    stop("For 'plugin' methods ('boott-plugin', 'bootsym-plugin'), 'sdfun' must be provided.")
  }

  # Validate parallel and cores arguments
  if (!is.logical(parallel)) {
    stop("'parallel' must be a logical value (TRUE or FALSE).")
  }

  if (parallel) {
    if (is.null(cores)) {
      stop("If 'parallel' is TRUE, 'cores' must be provided.")
    }

    if (!is.numeric(cores) || cores <= 0) {
      stop("'cores' must be a positive integer.")
    }

    # Create the cluster and ensure it's stopped afterward
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }


  # Extract necessary components from bootstrap_result
  thetahat <- boot_obj$thetahat # observed statistic
  thetastar <- boot_obj$thetastar # vector of bootstrap statistics
  data <- boot_obj$data # data
  statistic <- boot_obj$statistic # statistic function
  bsamples <- boot_obj$bsamples # list of bootstrap samples
  B <- length(thetastar) # number of bootstrap samples

  # Bootstrap estimate of the standard error of thetahat
  bootse <- sd(thetastar)

  # Estimate standard errors
  # Ensure sdfun is provided, then apply parallelization or sequential execution
  if(!is.null(sdfun)){
    sehat <- sdfun(data)
    if (!is.numeric(sehat) || length(sehat) != 1) stop("'sdfun' must return a single numeric value.")

    # Use parallelization if specified
    if(parallel){
      sestar_list <- parallel::parLapply(cl, bsamples, sdfun, ...) # Parallel execution
    } else {
      #if no parallel
      sestar_list <- lapply(bsamples, sdfun, ...) # Sequential execution
    }

    sestar <- unlist(sestar_list)

  } else { #When sdfun is NULL
    sehat <- bootse

    # Define the sdfun function when it is NULL
    sdfun <- function(i){
      bresults <- bootstrap(
        data = bsamples[[i]],
        statistic = statistic,
        nboot = Bsd,
        return_samples = FALSE
      )
      return(sd(bresults$thetastar))
    }

    # Use parallelization if specified
    if(parallel){
      sestar <- parallel::parLapply(cl, 1:B, sdfun) # Parallel execution
    } else {
      #if no parallel
      sestar <- sapply(1:B, sdfun) # Sequential execution
    }
  }


  # Method-specific confidence interval calculations
  methods_functions <- list(
    "boott-plugin" = function() {
      tstar <- (thetastar - thetahat) / sestar
      tstar <- sort(tstar)
      t_bounds <- c(tstar[floor(B * alpha / 2)], tstar[floor(B * (1 - alpha / 2))])
      ci <- c(lower = thetahat - t_bounds[1] * sehat, upper = thetahat + t_bounds[2] * sehat)
      return(ci)
    },
    "boott-nested" = function() {
      tstar <- (thetastar - thetahat) / sestar
      tstar <- sort(tstar)
      t_bounds <- c(tstar[floor(B * alpha / 2)], tstar[floor(B * (1 - alpha / 2))])
      ci <- c(lower = thetahat - t_bounds[1] * sehat, upper = thetahat + t_bounds[2] * sehat)
      return(ci)
    },
    "bootsym-plugin" = function() {
      tstar <- (thetastar - thetahat) / sestar
      tstar <- sort(abs(tstar))
      t_bounds <- c(tstar[floor(B * alpha / 2)], tstar[floor(B * (1 - alpha / 2))])
      ci <- c(lower = thetahat - t_bounds[1] * sehat, upper = thetahat + t_bounds[2] * sehat)
      return(ci)
    },
    "bootsym-nested" = function() {
      tstar <- (thetastar - thetahat) / sestar
      tstar <- sort(abs(tstar))
      t_bounds <- c(tstar[floor(B * alpha / 2)], tstar[floor(B * (1 - alpha / 2))])
      ci <- c(lower = thetahat - t_bounds[1] * sehat, upper = thetahat + t_bounds[2] * sehat)
      return(ci)
    }
  )

  result <- methods_functions[[method]]()

  results <- list(
    method = method,
    alpha = alpha,
    ci = result,
    bias = mean(thetastar) - thetahat,
    sehat = sehat,
    thetahat = thetahat,
    sestar = sestar
  )
  class(results) <- "bootstrap_t"
  return(results)
}








#' Custom Print Method for `bootstrap_t` Objects
#'
#' This function provides a custom print method for objects of class `bootstrap_t`.
#' It outputs a formatted summary of the bootstrap results, including the method used,
#' observed theta, standard error, bias, and the confidence interval.
#'
#' @param x An object of class `bootstrap_t` (output of the `bootstrap_t` function).
#' @param ... Additional arguments (not used).
#'
#' @return Prints a summary of the bootstrap-t results to the console.
#' @export
print.bootstrap_t <- function(x, ...) {
  cat("\nBootstrap-t Results\n")
  cat("=====================\n")
  cat("Method:         ", x$method, "\n")
  cat("Alpha:          ", x$alpha, "\n")
  cat("Theta (Observed):", x$thetahat, "\n")
  cat("Standard Error: ", x$sehat, "\n")
  cat("Bias:           ", x$bias, "\n")
  cat("Confidence Interval:\n")
  cat("  Lower: ", unname(x$ci[1]), "\n")
  cat("  Upper: ", unname(x$ci[2]), "\n")
  cat("\n")
  invisible(x)
}
