#' Select environmental covariables based on phenotype data
#'
#' @description
#' Perform variable selection using observed phenotype data to determine the most relevant set of environmental covariables
#'
#' @param pheno.data A \code{data.frame} of phenotype data that will be used in the analysis or to train predictive models. This must contain columns for genotypes, environments, and the value of the phenotypic observation.
#' @param env.data A \code{data.frame} of environmental variables in long format. Must be provided if \code{ec.matrix} is not passed; ignored otherwise. Must include information on the environments/trials/location in \code{pheno.data}.
#' @param ec.matrix A \code{matrix} of (ideally) centered and scaled environmental covariable data (i.e. from the \code{\link{ec_matrix}} function. Supercedes \code{env.data} if passed. Must include information on the environments/trials/location in \code{pheno.data}.
#' @param env.col The column in \code{pheno.data} and (if passed) \code{env.data} that designates environments.
#' @param gen.col The column in \code{pheno.data} that designates genotypes.
#' @param y.col The column in \code{pheno.data} that designates phenotypic values.
#' @param ec.subset A \code{character} vector of covariables in \code{env.data} or \code{ec.matrix} that should be considered. If \code{NULL} (default), all covariables in \code{env.data} or \code{ec.matrix} are considered.
#' @param search.method The covariable selection search method. See \emph{Details} for more information.
#' @param cv.type The type of cross-validation used to select variables. Can be \code{"loo"} for leave-one-out (environments) or \code{"mc"} for random train-test partitions.
#' @param cv.ptrain The proportion of data to use in model training if \code{cv.type = "mc"}.
#' @param cv.iter The number of cross-validation replications if \code{cv.type = "mc"}.
#' @param ... Other arguments to pass to the \code{\link{ec_matrix}} function (if \code{ec.matrix} is not passed).
#'
#' @details
#' TBD.
#'
#' @return
#' The function \code{ec_variable_selection} returns a list with the following elements:
#'
#' \describe{
#'   \item{\code{optVariables}}{A n x 1 \code{matrix} where the row names are the n environmental covariables selected and the elements of the matrix are the importance value of that variable.}
#'   \item{\code{finalCVResults}}{A vector with the cross-validation results of the final model.}
#'   \item{\code{CVStepResults}}{A matrix detailing the cross-validation results at each step of the stepwise linear model variable selection approach (i.e. \code{search.method = "stepwise.lmm"})}
#'
#' }
#'
#' The function \code{ec_variable_selection_slide} returns a list with the following elements:
#'
#' \describe{
#'   \item{\code{timeframeFinalCVResults}}{A \code{data.frame} of cross-validation results for each timeframe.}
#'   \item{\code{variableSelectionResults}}{A list containing the same elements as the output of \code{ec_variable_selection}, but only for the optimal timeframe.}
#'   \item{\code{ecMatrixSelected}}{The n x p matrix of environmental covariables summarized across the optimal timeframe.}
#'
#' }
#'
#'
#' @examples
#' \dontrun{
#'
#' pheno.data <- pheno_data_train
#' env.data <- env_data_merged
#'
#' variable_selection_out <- ec_variable_selection(
#'   pheno.data = pheno.data, env.data = env.data, env.col = "trial",
#'   gen.col = "line_name", y.col = "value", var.col = "variable",
#'   val.col = "value"
#' )
#'
#' }
#'
#'
#' @importFrom dplyr group_by_at
#'
#' @export
#'
ec_variable_selection <- function(pheno.data, env.data, ec.matrix, env.col, gen.col, y.col, ec.subset = NULL,
                                  search.method = c("stepwise.lmm", "lasso"), cv.type = c("loo", "mc"),
                                  cv.ptrain = 0.60, cv.iter = 25, ...) {


  ## Error check ##
  # Capture other arguments
  other.args <- list(...)

  # Check classes
  stopifnot(is.data.frame(pheno.data))
  # Choose between env.data and ec.matrix; both can't be missing
  if (missing(env.data) & missing(ec.matrix)) {
    stop("One of 'env.data' or 'ec.matrix' must be passed.")

  } else if (!missing(ec.matrix)) {
    stopifnot(is.matrix(ec.matrix))
    # It must have row and columnames
    if (any(sapply(dimnames(ec.matrix), is.null))) stop("'ec.matrix' must have both row and column names.")
    calc_ec_mat <- FALSE

  } else if (!missing(env.data)) {
    stopifnot(is.data.frame(env.data))
    # If env.data is passed, but ec.matrix is not, verify column names were passed to other.args
    var.col <- other.args$var.col
    val.col <- other.args$val.col
    # Error if any of these are null
    if (is.null(var.col) | is.null(val.col)) stop("If 'ec.matrix' is not passed, you must pass both 'var.col' and 'val.col'. See the documentation for the 'ec_matrix' function for details:\n\nrun `help(ec_matrix)`")
    calc_ec_mat <- TRUE

  }

  stopifnot(is.character(env.col))
  stopifnot(is.character(gen.col))
  stopifnot(is.character(y.col))
  stopifnot(is.character(ec.subset)|is.null(ec.subset))
  search.method <- match.arg(search.method)
  cv.type <- match.arg(cv.type)

  # If cv.type is "mc", verify cv.ptrain and cv.iter
  if (cv.type == "mc") {
    stopifnot(is.numeric(cv.ptrain))
    stopifnot(is.numeric(cv.iter))
    stopifnot(cv.iter <= 0)
    stopifnot(cv.ptrain > 0 & cv.ptrain < 1)

  }

  # Check pheno.dat for other names
  if (!env.col %in% names(pheno.data)) stop("The column '", env.col, "' is not in 'pheno.data'.")
  if (!gen.col %in% names(pheno.data)) stop("The column '", gen.col, "' is not in 'pheno.data'.")
  if (!y.col %in% names(pheno.data)) stop("The column '", y.col, "' is not in 'pheno.data'.")

  # Create the EC matrix, if it is missing
  if (calc_ec_mat) {
    ec.matrix <- do.call("ec_matrix", c(env.data = quote(env.data), env.col = env.col,
                                        subset(other.args, names(other.args) %in% formalArgs(def = "ec_matrix"))))

  }

  # Subset the matrix for envs in pheno.data and ecs in ec.subset
  ec.subset <- if (is.null(ec.subset)) colnames(ec.matrix)
  ec.matrix1 <- subset.matrix(ec.matrix, subset = rownames(ec.matrix) %in% unique(pheno.data[[env.col]]),
                              select = ec.subset, drop = FALSE)

  # Convert to a data.frame and merge with pheno.data
  ec.df <- as.data.frame(ec.matrix1)
  ec.df[[env.col]] <- row.names(ec.df)

  pheno.data1 <- droplevels(merge.data.frame(x = pheno.data, y = ec.df, by = env.col))

  # Convert to factor
  pheno.data1[[env.col]] <- fct_contr_sum(x = as.factor(pheno.data1[[env.col]]))
  pheno.data1[[gen.col]] <- fct_contr_sum(x = as.factor(pheno.data1[[gen.col]]))

  # Determine indices for cross-validation
  if (cv.type == "loo") {
    cv_df <- crossv_loo_grouped(data = group_by_at(.tbl = pheno.data1, .vars = env.col))
    # Extract the indices for training in each cv fold
    cv.indices <- list(lapply(X = cv_df$train, FUN = "[[", "idx"))

  } else if (cv.type == "mc") {
    cv_df <- crossv_mc_grouped(data = group_by_at(.tbl = pheno.data1, .vars = env.col), n = cv.iter, test = 1 - cv.ptrain)
    # Extract the indices for training in each cv fold
    cv.indices <- lapply(X = cv_df$train, FUN = function(x) list(x$idx))

  }

  # Control based on search.method
  if (search.method == "stepwise.lmm") {

    # Fit a base model
    base_form <- reformulate(termlabels = paste0("(1|", gen.col, ")"), response = y.col)
    base_fit <- lmer(formula = base_form, data = pheno.data1)

    # Define the scope
    scope <- list(lower = formula(base_fit), upper = reformulate(c(paste0("(1|", gen.col, ")"), colnames(ec.matrix1)), response = "value"))

    # Use the stepwise variable selection
    var_sel_out <- stepwise_rfa(object = base_fit, data = pheno.data1, env.col = env.col, scope = scope,
                                metric = "RMSE", cv.indices = cv.indices)

    # Get the optimized variables
    optVariables <- var_sel_out$optVariables

    # Designate variable importance
    if (length(optVariables) != 0) {
      # Add importance to optVariables
      var_sel_out$optVariables <- matrix(1, nrow = length(optVariables), dimnames = list(optVariables, "importance"))

    } else {
      var_sel_out$optVariables <- matrix(1, nrow = 0, ncol = 1, dimnames = list(NULL, "importance"))

    }

    # Assemble the cv step results
    CVStepResults <- rbind(rapid_crossv(object = base_fit, cv.indices = cv.indices),
                           do.call("rbind", lapply(X = var_sel_out$stepTestResults, FUN = "[", 1, , drop = FALSE)))

    # Remove any rows where the row-name is '(none)'
    CVStepResults <- CVStepResults[row.names(CVStepResults) != "(none)",, drop = FALSE]

    # Designate row.names for this matrix
    stepFormula <- as.formula(paste0("~ ", var_sel_out$stepTraverseFormula))
    addSub <- apply(X = attr(terms(stepFormula), "factors"), MARGIN = 1, FUN = function(x) ifelse(any(x == 1), "+", "-"))
    rownames(CVStepResults) <- paste(addSub, names(addSub))

    # Reorganize into a output list
    output <- list(
      optVariables = var_sel_out$optVariables,
      finalCVResults = var_sel_out$finalResults,
      CVStepResults = CVStepResults
    )

  } else if (search.method == "lasso") {

    stop("This option is not yet implemented.")


  }

  # Return a list
  return(output)

}


#'
#' @rdname ec_variable_selection
#'
#' @param var.col See \code{\link{ec_matrix}}.
#' @param val.col See \code{\link{ec_matrix}}.
#' @param time.col The column in \code{env.data} used to create time frames (this is usually year).
#' @param time.list A list of vectors, each containing elements of \code{time.col} that delineate the timeframes. If not passed, a list is created that accumulates elements of \code{time.col} from maximum to minimum in steps of 1 unit.
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' # Replace location
#' pheno.data <- merge(pheno_data_train[-2], trial_info[c("trial", "location")])
#' env.data <- hist_env_data_merged
#'
#' variable_selection_out <- ec_variable_selection_slide(
#'   pheno.data = pheno.data, env.data = env.data, env.col = "location",
#'   gen.col = "line_name", y.col = "value", var.col = "variable",
#'   val.col = "value", time.col = "year"
#' )
#'
#' }
#'
#'
#' @importFrom dplyr group_by_at
#'
#' @export
#'
ec_variable_selection_slide <- function(pheno.data, env.data, env.col, gen.col, y.col, var.col, val.col, ec.subset = NULL,
                                        search.method = c("stepwise.lmm", "lasso"), time.col, time.list,
                                        cv.type = c("loo", "mc"), cv.ptrain = 0.60, cv.iter = 25) {


  ## Error check ##

  # Check classes
  stopifnot(is.data.frame(pheno.data))
  stopifnot(is.data.frame(env.data))
  stopifnot(is.character(env.col))
  stopifnot(is.character(gen.col))
  stopifnot(is.character(y.col))
  stopifnot(is.character(var.col))
  stopifnot(is.character(val.col))
  stopifnot(is.character(time.col))
  stopifnot(is.character(ec.subset)|is.null(ec.subset))
  search.method <- match.arg(search.method)
  cv.type <- match.arg(cv.type)

  # If cv.type is "mc", verify cv.ptrain and cv.iter
  if (cv.type == "mc") {
    stopifnot(is.numeric(cv.ptrain))
    stopifnot(is.numeric(cv.iter))
    stopifnot(cv.iter <= 0)
    stopifnot(cv.ptrain > 0 & cv.ptrain < 1)

  }

  # Check pheno.dat for other names
  if (!env.col %in% names(pheno.data)) stop("The column '", env.col, "' is not in 'pheno.data'.")
  if (!gen.col %in% names(pheno.data)) stop("The column '", gen.col, "' is not in 'pheno.data'.")
  if (!y.col %in% names(pheno.data)) stop("The column '", y.col, "' is not in 'pheno.data'.")

  # Check env.data for time names
  if (!time.col %in% names(env.data)) stop("The column '", time.col, "' is not in 'env.data'.")
  if (!var.col %in% names(env.data)) stop("The column '", var.col, "' is not in 'env.data'.")
  if (!val.col %in% names(env.data)) stop("The column '", val.col, "' is not in 'env.data'.")
  if (!env.col %in% names(env.data)) stop("The column '", env.col, "' is not in 'env.data'.")



  ## Summarize data from env.data over windows

  # Create the timeframes
  # If time.list is missing, accumulate elements of time.col from max to min in steps of 1
  if (missing(time.list)) {
    # Find the range in time
    time_col_range <- range(env.data[[time.col]])
    # Create an accumulating sequence
    time.list <- Reduce(f = c, x = seq(from = time_col_range[1], to = time_col_range[2], by = 1), accumulate = TRUE, right = TRUE)

  } else {
    # Check class
    stopifnot(is.list(time.list))
    # Make sure elements of time.list are elements of env.data[[time.col]]
    if (!all(unlist(time.list) %in% env.data[[time.col]])) stop("Not all elements of 'time.list' are elements of of 'time.col'.")

  }

  # Break up env.data by time.list
  ec_matrix_time_list <- lapply(X = time.list, FUN = function(time) {
    # subset env.data
    env.data_sub <- subset.data.frame(env.data, subset = env.data[[time.col]] %in% time)
    # Summarize variables
    env.data_sub_sum <- aggregate(formula = reformulate(termlabels = c(env.col, var.col), response = val.col), data = env.data_sub, FUN = mean)
    # Calculate the ec_matrix and return
    ec_matrix(env.data = env.data_sub_sum, env.col = env.col, var.col = var.col, val.col = val.col)

  })

  # Create an empty matrix to store variable selection results
  ec_varsel_list <- vector("list", length = length(ec_matrix_time_list))

  # Loop over ec_varsel_list
  for (i in seq_along(ec_varsel_list)) {
    # Use the ec_variable_selection function
    out <- ec_variable_selection(pheno.data = pheno.data, ec.matrix = ec_matrix_time_list[[i]], env.col = env.col,
                                 gen.col = gen.col, y.col = y.col, ec.subset = ec.subset, search.method = search.method,
                                 cv.type = cv.type, cv.ptrain = cv.ptrain, cv.iter = cv.iter)

    # Add to the list
    ec_varsel_list[[i]] <- c(out, timeframe = list(time.list[[i]]))

  }

  # Name the list
  names(ec_varsel_list) <- sapply(X = lapply(X = ec_varsel_list, FUN = "[[", "timeframe"),
                                  FUN = function(time) paste0("timeframe_", min(time), "_", max(time)))

  # Create a data.frame of final CV results
  timeFinalCVResults <- data.frame(timeframe = names(ec_varsel_list),
                                   start_time = sapply(X = ec_varsel_list, FUN = function(x) min(x$timeframe)),
                                   end_time = sapply(X = ec_varsel_list, FUN = function(x) max(x$timeframe)),
                                   t(sapply(X = ec_varsel_list, "[[", "finalCVResults")),
                                   row.names = NULL)

  # Add a column for selected
  timeFinalCVResults$selected <- character(length = nrow(timeFinalCVResults))
  timeFinalCVResults$selected[which.min(timeFinalCVResults$RMSE)] <- "*"

  # Return a list with these results, the output from ec_variable_selection,
  # and the summarized ec_matrix from this timeframe
  output <- list(
    timeframeFinalCVResults = timeFinalCVResults,
    variableSelectionResults = ec_varsel_list[[which.min(timeFinalCVResults$RMSE)]],
    ecMatrixSelected = ec_matrix_time_list[[which.min(timeFinalCVResults$RMSE)]]
  )

  # return this output
  return(output)

}





















