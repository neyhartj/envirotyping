#' Ancillary cross-validation functions
#'
#' @param data A grouped data frame.
#' @param id Name of variable that gives each training set a unique integer id.
#'
#' @details
#' Exported from the \href{https://github.com/neyhartj/neyhart}{neyhart} package.
#'
#' @importFrom dplyr is_grouped_df ungroup group_keys group_rows
#' @importFrom modelr crossv_mc
#'
#' @export
#'
crossv_loo_grouped <- function(data, id = ".id") {

  stopifnot(is.data.frame(data))
  stopifnot(is_grouped_df(data))
  stopifnot(is.character(id))

  # Get the group keys
  grp_keys <- group_keys(data)
  ## Split rows by group
  grp_rows <- group_rows(data)

  ## Ungroup the data frame
  df <- ungroup(data)

  # For each key, outer join for train and inner join for test
  test_list <- lapply(X = grp_rows, FUN = modelr::resample, data = df)
  train_list <- lapply(X = grp_rows, FUN = function(rows) modelr::resample(data = df, idx = setdiff(unlist(grp_rows), rows)))

  # Package into tibble
  grp_keys[["train"]] <- train_list
  grp_keys[["test"]] <- test_list
  grp_keys[[id]] <- seq_along(grp_keys[[1]])

  return(grp_keys)

}


#'
#' @describeIn crossv_loo_grouped
#'
#' @param n The number of cross validation iterations.
#' @param test The proportion of data to save for model testing.
#'
#' @importFrom dplyr tibble
#'
#' @export
#'
crossv_mc_grouped <- function(data, n, test = 0.25, id = ".id") {

  stopifnot(is.data.frame(data))
  stopifnot(is_grouped_df(data))
  stopifnot(is.character(id))


  # Get the group keys
  grp_keys <- group_keys(data)
  ## Split rows by group
  grp_rows <- group_rows(data)

  ## Ungroup the data frame
  df <- ungroup(data)
  # List of all data rows
  data_rows <- seq_len(nrow(df))

  # Generate training sets based on the grp keys
  train_test_list <- crossv_mc(data = tibble(grp_rows), n = n, test = test, id = id)
  # Get the indices
  train_indices <- lapply(X = train_test_list$train, FUN = "[[", "idx")
  train_indices <- lapply(X = train_indices, FUN = function(tindx) unlist(grp_rows[tindx]))

  # Create training resamples
  train_list <- lapply(X = train_indices, FUN = function(rows) modelr::resample(data = df, idx = rows))
  test_list <- lapply(X = train_indices, FUN = function(rows) modelr::resample(data = df, idx = setdiff(data_rows, rows)))

  out <- tibble(train = train_list, test = test_list, id = train_test_list[[id]])
  names(out)[3] <- id

  return(out)

}


#'
#' @describeIn crossv_loo_grouped
#'
#' @param object A fitted model object
#' @param cv.indices A list of indices that define each training set. Metrics are calculated using the predicted and observed values from the holdout sets (i.e. opposite the training set indicies). Metrices will be averaged over multiple nested lists.
#' @param rapid Logical; should the rapid method be used?
#'
#' @importFrom MASS ginv
#'
#' @export
#'
rapid_crossv <- function(object, cv.indices, rapid) {
  UseMethod("rapid_crossv", object = object)
}


#'
#' @export
#'
rapid_crossv.lm <- function(object, cv.indices, rapid = TRUE) {

  # Get the model frame
  mf <- model.frame(object)
  # Model matrix and crossprod inverse
  X <- model.matrix(object)
  XtX_inv <- solve(crossprod(X))
  # Response
  y <- model.response(mf)
  # Coefficients
  beta_hat <- coef(object)

  # Iterate over the upper nest of cv.indices
  metrics_out <- sapply(X = cv.indices, FUN = function(index) {

    # Iterate over indices
    cv_out <- lapply(X = index, FUN = function(modelInd) {
      # Subset X for the d testing datapoints
      X_d <- X[-modelInd,,drop = FALSE]
      d <- nrow(X_d)
      # H matrix
      H_d <- tcrossprod(X_d %*% XtX_inv, X_d)
      H_d_inv <- ginv(diag(d) - H_d)
      # Residuals
      e_d <- y[-modelInd] - X_d %*% beta_hat

      # New betas
      beta_hat_holdout <- beta_hat - (tcrossprod(XtX_inv, X_d) %*% H_d_inv %*% e_d)

      # yhat
      y_hat <- X_d %*% beta_hat_holdout
      # return
      y_hat

    })

    ## Pull out y
    y_hat_all <- unlist(cv_out)

    # Calculate metrics and return
    R2 <- cor(y_hat_all, y)^2
    mse <- mean((y - y_hat_all)^2)

    c(R2 = R2, MSE = mse, RMSE = sqrt(mse))

  })

  # Calculate the mean of the metrics
  rowMeans(x = metrics_out, na.rm = TRUE)

}




#'
#' @importFrom lme4 lmer
#'
#' @export
#'
rapid_crossv.merMod <- function(object, cv.indices, rapid = FALSE) {

  # Get the model frame
  mf <- model.frame(object)
  # All indices
  mf_i <- seq_len(nrow(mf))


  # Split by rapid
  if (rapid) {

    # Stop because this is not enabled
    stop("rapid is not yet enabled for this function.")

    # # Model matrix and crossprod inverse
    # X <- model.matrix(object)
    # XtX_inv <- solve(crossprod(X))
    #
    # # Coefficients
    # beta_hat <- coef(object)




  } else {

    # Iterate over the upper nest of cv.indices
    suppressWarnings({
      metrics_out <- sapply(X = cv.indices, FUN = function(index) {

        # List of training data
        train_list <- lapply(X = index, FUN = function(i) mf[i,])
        test_list <- lapply(X = index, FUN = function(i) mf[setdiff(mf_i, i),])

        # Fit models with the training set
        suppressMessages(models <- lapply(X = train_list, FUN = lmer, formula = formula(object)))
        # Predict the test set
        test_pred_list <- mapply(models, test_list, FUN = lme4:::predict.merMod, SIMPLIFY = FALSE)
        test_pred <- do.call("rbind", mapply(test_list, test_pred_list, FUN = function(.x, .y) cbind(.x, pred = .y), SIMPLIFY = FALSE))

        ## Pull out y
        y_hat_all <- test_pred$pred
        y <- test_pred$value

        # Calculate metrics and return
        R2 <- cor(y_hat_all, y)^2
        mse <- mean((y - y_hat_all)^2)

        c(R2 = R2, MSE = mse, RMSE = sqrt(mse))

    }) })

  }

  # Calculate the mean of the metrics
  rowMeans(x = metrics_out, na.rm = TRUE)

}

