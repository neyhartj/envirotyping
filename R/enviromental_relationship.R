#' Construct an environmental "relationship" matrix from environmental covariables
#'
#' @param env.data A \code{data.frame} of environmental variables in long format. Must be provided if \code{ec.matrix} is not passed; ignored otherwise. Must include information on the environments/trials/location in \code{pheno.data}.
#' @param ec.matrix A \code{matrix} of (ideally) centered and scaled environmental covariable data (i.e. from the \code{\link{ec_matrix}} function. Supercedes \code{env.data} if passed. Must include information on the environments/trials/location in \code{pheno.data}.
#' @param ec.weights A \code{matrix} of weights for individual covariables.
#' @param method The method of calculating the relationship matrix. See \emph{Details}.
#' @param ... Other arguments to pass to the \code{\link{ec_matrix}} function (if \code{ec.matrix} is not passed).
#'
#'
#' @details
#'
#' The methods of calculaing the relationship matrix are defined as follows. In each, \emph{X} represents the centered and scaled \emph{n} x \emph{p} matrix of \emph{p} covariables for \emph{n} environments, and \eqn{Z = Xw}, where \emph{w} is a matrix of weights from \code{ec.weights}.
#'
#' \itemize{
#'   \item{\code{method1}: \eqn{ (ZZ') / p } }
#'   \item{\code{method2}: \eqn{ 1 - \sum^p_{i=1}\frac{z_{ij} - z_{ij'}}{max(z_i) - min(z_i)} } }
#'   \item{\code{method3}: \eqn{ \frac{dist(Z)}{max(dist(Z))} } }
#'
#' }
#'
#'
#' @references
#' Jarquin, D., J. Crossa, X. Lacaze, P. Du Cheyron, J. Daucourt, J. Lorgeou, F. Piraux, L. Guerreiro, P. Perez, M. Calus, J. Burgueno, and G. de los Campos. 2014. A reaction norm model for genomic selection using high-dimensional genomic and environmental data. Theor. Appl. Genet. 127(3): 595-607. doi: 10.1007/s00122-013-2243-1.
#'
#' Malosetti, M., D. Bustos-Korts, M.P.P. Boer, and F. a. van Eeuwijk. 2016. Predicting Responses in Multiple Environments: Issues in Relation to Genotype x Environment Interactions. Crop Sci. 56: 2210-2222. doi: 10.2135/cropsci2015.05.0311.
#'
#' Rincent, R., M. Malosetti, B. Ababaei, G. Touzy, A. Mini, M. Bogard, P. Martre, J. Le Gouis, and F. van Eeuwijk. 2019. Using crop growth model stress covariates and AMMI decomposition to better predict genotype-by-environment interactions. Theor. Appl. Genet. 132(12): 3399-3411. doi: 10.1007/s00122-019-03432-y.
#'
#'
#'
#'
#'
#' @examples
#'
#' env.data <- env_data_merged
#'
#' # Example ec.weights from running ec_variable_selection
#' optVariables <- matrix(
#'   data = 1, nrow = 4,
#'   dimnames = list(c("flowering.daylength_mean", "grain_fill.radn_sum",
#'                     "subsoil_ref_bulk_density", "late_vegetative.radn_sum"),
#'                   "importance"))
#'
#' # Calculate the relationship matrix from all covariables
#' Emat <- ec_relmat(env.data = env.data, env.col = "trial", var.col = "variable",
#'                   val.col = "value")
#'
#' # Calculate the relationship matrix using a data.frame
#' # Use only the optimized variables from 'optVariables'
#' Emat <- ec_relmat(env.data = env.data, ec.weights = optVariables,
#'                   env.col = "trial", var.col = "variable", val.col = "value")
#'
#' # Calculate a n x p matrix of covariables first, then use this to calculate
#' # the relationship matrix
#' ec_mat <- ec_matrix(env.data = env.data, env.col = "trial", var.col = "variable",
#'                     val.col = "value")
#' Emat <- ec_relmat(ec.matrix = ec_mat, ec.weights = optVariables)
#'
#'
#'
#'
#' @export
#'
ec_relmat <- function(env.data, ec.matrix, ec.weights = NULL, method = c("method1", "method2", "method3"), ...) {

  ## Error check ##
  # Capture other arguments
  other.args <- list(...)

  # Check classes
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
    env.col <- other.args$env.col
    # Error if any of these are null
    if (is.null(var.col) | is.null(val.col) | is.null(env.col)) stop("If 'ec.matrix' is not passed, you must pass 'env.col', 'var.col', and 'val.col'. See the documentation for the 'ec_matrix' function for details:\n\nrun `help(ec_matrix)`")
    calc_ec_mat <- TRUE

  }

  # Match the method argument
  method <- match.arg(method)

  # Create the EC matrix, if it is missing
  if (calc_ec_mat) {
    ec.matrix <- do.call("ec_matrix", c(env.data = quote(env.data), subset(other.args, names(other.args) %in% formalArgs(def = "ec_matrix"))))

  } else {
    # Check to make sure the ec.matrix is centered and scaled
    if (any(!c("scaled:center", "scaled:scale") %in% names(attributes(ec.matrix)))) {
      warning("The elements of 'ec.matrix' may not be centered or scaled (i.e. from the 'scale()' function). Proceeding anyway...\n")
    }

  }

  # Check ec.weights
  if (is.null(ec.weights)) {
    ec.weights <- matrix(data = 1 / ncol(ec.matrix), nrow = ncol(ec.matrix), ncol = 1,
                         dimnames = list(colnames(ec.matrix), "importance"))


  } else {
    # Check variable weights and method
    stopifnot(is.matrix(ec.weights))
    # ec.weights must have dimnames
    if (any(sapply(dimnames(ec.weights), is.null))) stop("Input 'ec.weights' must have both row and column names.")
    # ec.weights must have 1 column
    if (ncol(ec.weights) != 1) stop("Input 'ec.weights' must only have 1 column (importance). Please check your input.")

    # Make sure all rownames of ec.weights are in ec.matrix
    if (any(!rownames(ec.weights) %in% colnames(ec.matrix))) stop("The row names in 'ec.weights' must be variables in the 'env.data' input or column names in the 'ec.matrix' input.")

    # Other weight error handling
    # If the weights don't sum to 1; scale them such and issue a warning
    if (!(abs(1 - sum(ec.weights)) < 1e-10)) {
      ec.weights <- ec.weights / sum(ec.weights)
      warning("The provided 'ec.weights' did not sum to 1. This was automatically corrected.\n")

    }

  }

  # Subset the matrix for ecs in ec.weights
  ec.subset <- row.names(ec.weights[ec.weights[,1] > 0,, drop = FALSE])
  ec.matrix1 <- subset.matrix(ec.matrix, select = ec.subset, drop = FALSE)

  # Adjust the ec.matrix with weights
  weightMat <- matrix(data = ec.weights, ncol = ncol(ec.matrix1), nrow = nrow(ec.matrix1), byrow = TRUE)
  X1 <- ec.matrix1 * weightMat


  # Separate based on method
  if (method == "method1") {
    # Jarquin2014 method
    # Take the cross product and multiply by the number of covariables
    EMAT <- tcrossprod(X1) * ncol(X1)

  } else if (method == "method2") {
    # Malosetti2016 method
    # Calculate pairwise euclidian distance
    euc_dist <- apply(X = X1, MARGIN = 2, FUN = function(z) list( as.matrix(dist(z)) / ( max(z) - min(z) ) ) )
    # Get the first element of each list
    euc_dist <- lapply(euc_dist, "[[", 1)

    ## Sum all matrices
    EMAT <- 1 - Reduce(f = `+`, x = euc_dist)

  } else if (method == "method3") {
    # Rincent2019 method
    D <- as.matrix(dist(X1))
    EMAT <- 1 - (D / max(D))

  }

  # Return the matrix
  return(EMAT)

}
