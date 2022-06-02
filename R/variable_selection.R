#' Functions for environmental covariable selection
#'
#' @description
#' These functions provide different approaches to selecting environmental covariables for use in quantitative genetic analysis or prediction. \code{stepwise_rfa} implements a stepwise recursive feature addition algorithm based on optimizing a metric (the root mean squared error or the coefficient of determination) by sequentially adding (or removing) variables.
#'
#'
#' @param object The base fitted model object. Can be a \code{lm} or \code{merMod} object.
#' @param data The data frame that includes phenotypic observations and values for the environmental covariables.
#' @param env.col The column in \code{data} corresponding to environments.
#' @param scope A named list of two formulas defining the lower and upper scope of the variable search.
#' @param metric The metric to optimize: either minimize the root mean squared error (\code{"RMSE"}) or maximize the coefficient of determination (\code{"R2"}).
#' @param cv.indices A list of indices that define each training set. Metrics are calculated using the predicted and observed values from the holdout sets (i.e. opposite the training set indicies). Metrices will be averaged over multiple nested lists.
#'
#' @details
#' Note that inputs for this function are only sparsely checked for correctness.
#'
#' @importFrom lme4 findbars
#'
#' @export
#'
stepwise_rfa <- function(object, data, env.col, scope, metric = c("RMSE", "R2"), cv.indices) {

  # Match arguments
  metric <- match.arg(metric)
  maximize <- switch(metric, RMSE = FALSE, R2 = TRUE)

  # Get the class of object to determine the CV function to use
  stopifnot(inherits(object, c("lm", "merMod", "lmerMod")))


  # Assuming environments are here, find the maximum number of covariates that could be used
  J <- length(unique(data[[env.col]]))
  H <- J - 2 # H is the maximum number of covariates

  ## Perform the base cv
  base_resample_pred <- rapid_crossv(object = object, cv.indices = cv.indices)
  # Get the formula of the object
  object_formula <- formula(object)

  ## Start the algorithm
  # Initiate an object to store the current formula
  form_use <- object_formula
  # Define the available covariates and the used covariates
  cov_available <- add.scope(terms1 = scope$lower, terms2 = scope$upper)
  # Add parentheses if missing
  barsnoparen <- grepl(pattern = "\\|", x = cov_available) & !grepl(pattern = "\\(", x = cov_available)
  cov_available[barsnoparen] <- paste0("(", cov_available[barsnoparen], ")")

  # Vector of covariates that were removed by backwards elimination
  cov_used <- cov_remove <- cov_retain <- NULL

  ## Create a pseudoformula that documents the addition/removal of covariates
  traverse_formula <- list(as.character(scope$lower)[3])

  # Define a flag to stop the algorithm
  optMetric <- FALSE
  allCovUsed <- length(cov_available) == 0

  # A list to store output
  step_out <- list()
  i = 1

  # While loop
  while (!optMetric & !allCovUsed) {

    ## Initiate backwards elimination if the length of cov_used is >= 2
    if (length(cov_used) >= 3) {

      ## Create formulae where all covariates except line name and the last covariate
      ## added are eligible for removal
      cov_removable <- setdiff(cov_used, cov_retain)

      # Perform single variable elimination
      cov_removal_pred <- lapply(X = setNames(cov_removable, cov_removable), FUN = function(term) {
        # Construct a new formula
        form1 <- update.formula(old = form_use, new = as.formula(paste0("~ . - ", term)))
        # Update the model and return
        suppressMessages(newmod <- update(object = object, formula = form1))
        # Calculate the cross-validation results and return
        rapid_crossv(object = newmod, cv.indices = cv.indices, rapid = FALSE)
      })

      # Analyze the metrics
      cov_removal_metrics <- if (length(cov_removal_pred) == 0) NULL else t(do.call("rbind", cov_removal_pred))
      cov_removal_metrics <- t(cbind(cov_removal_metrics, `(none)` = base_resample_pred))
      cov_removal_metrics <- cov_removal_metrics[order(cov_removal_metrics[,metric], decreasing = maximize),, drop = FALSE]

      # Select the covariate to remove
      cov_remove <- row.names(cov_removal_metrics)[1]

      # If not none, edit "cov available" so the removed covariate is not considered.
      if (cov_remove != "(none)") {

        # add to the formula list
        traverse_formula[[length(traverse_formula)+1]] <- paste0("- ", cov_remove)

        # Edit cov_available and cov_used
        # Remove the covariate from cov_used
        cov_used <- setdiff(cov_used, cov_remove)
        # This removed covariate is not available to add
        cov_available <- setdiff(cov_available, cov_remove)

      }

    } # End if statement

    # Iterate over cov_available and build models
    cov_addition_pred <- lapply(X = setNames(cov_available, cov_available), FUN = function(term) {
      # Add lower level main effects if interactions are present
      if (grepl(pattern = ":", x = term)) {
        termsplit <- strsplit(x = term, split = ":")[[1]]
        term <- c(termsplit[sapply(X = data[termsplit], inherits, "numeric")], term)
      }

      # Construct a new formula
      form1 <- update.formula(old = form_use, new = reformulate(termlabels = c(".", term)))
      # Update the model and return
      suppressMessages(newmod <- update(object = object, formula = form1))
      # Calculate the cross-validation results and return
      rapid_crossv(object = newmod, cv.indices = cv.indices, rapid = FALSE)
    })

    # Analyze the metrics
    cov_addition_metrics <- if (length(cov_addition_pred) == 0) NULL else t(do.call("rbind", cov_addition_pred))
    cov_addition_metrics <- t(cbind(cov_addition_metrics, `(none)` = base_resample_pred))
    cov_addition_metrics <- cov_addition_metrics[order(cov_addition_metrics[,metric], decreasing = maximize),, drop = FALSE]

    # Select the covariate to add
    cov_retain <- row.names(cov_addition_metrics)[1]

    # If "none", stop
    if (cov_retain == "(none)") {
      optMetric <- TRUE

    } else {

      # add to the formula list
      traverse_formula[[length(traverse_formula)+1]] <- paste0("+ ", cov_retain)

      # Edit cov_available and cov_used
      cov_used <- union(cov_used, cov_retain)
      cov_available <- setdiff(cov_available, cov_used)

      # Has the maximum number of covariates been used?
      allCovUsed <- length(cov_available) == 0 | length(cov_used) == H

      # Edit the metrics to compare
      base_resample_pred <- cov_addition_metrics[1,]

      # Update the formula to edit
      form_use <- update.formula(old = formula(object), new = reformulate(termlabels = c(".", cov_used)))
    }

    step_out[[i]] <- cov_addition_metrics
    i <- i + 1

  } # End the while loop


  # Return a list
  list(optVariables = cov_used, finalResults = step_out[[length(step_out)]][1,],
       stepTestResults = step_out, stepTraverseFormula = paste0(unlist(traverse_formula), collapse = " "))

} # Close the function










#' Determine feature importance using the LASSO procedure
#'
#'
#'
lasso_fi <- function(object, data, env.col, scope, metric = c("RMSE", "R2"), cv.indices) {

  stop("This function is not yet working.")

  # Estimate feature importance using the LASSO
  # x terms
  predictors <- c(intersect("line_name", names(data)), covariates_use)


  ## First convert data to a model frame
  mf <- model.frame(formula = reformulate(termlabels = predictors, response = "value"), data = data)
  # Pull response vector
  y <- model.response(mf)
  # Create matrix of covariates
  X <- model.matrix(reformulate(termlabels = predictors, intercept = FALSE), mf)
  # X <- model.matrix(reformulate(termlabels = c(covariates_use), intercept = FALSE), mf)

  ## Conduct leave-one-out cross-validation
  # Determine which observation is in what fold
  fold_id <- as.numeric(data[[env.col]])
  cv_lasso_loo_out <- cv.glmnet(x = X, y = y, type.measure = "mse", foldid = fold_id, alpha = 1, grouped = FALSE,
                                standardize = FALSE, nlambda = 100)

  # Record the lambda value and MSE for that lambda
  min_RMSE_loo <- sqrt(min(cv_lasso_loo_out$cvm))
  min_lambda <- cv_lasso_loo_out$lambda[which.min(cv_lasso_loo_out$cvm)]

  # Calculate R2
  R2 <- c(cor(y, predict(object = cv_lasso_loo_out, newx = X, s = min_lambda))^2)

  # Get the coefficients for each ec
  ec_coef <- as.matrix(coef(object = cv_lasso_loo_out, s = min_lambda))[covariates_use,,drop = FALSE]

  # Calculate relative importance as the ratio of the absolute value of each coefficient
  # to the sum of the absolute value of the coefficients
  ec_importance <- abs(ec_coef) / sum(abs(ec_coef))
  colnames(ec_importance) <- "importance"
  ec_importance[is.na(ec_importance),] <- 0

  # Create a list to output
  lasso_out <- list(
    optVariables = ec_importance,
    finalResults = c(R2 = R2, MSE = min_RMSE_loo^2, RMSE = min_RMSE_loo)
  )


  ## Create a tibble
  res <- tibble(
    feat_sel_type = "lasso_cv",
    model = c("model2", "model3"),
    selection_output = list(lasso_out, lasso_out),
  )

}





