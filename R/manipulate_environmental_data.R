#' Edit environmental data
#'
#' @description
#' Allows simple edits to the environmental data based on a complementary data.frame. This could be useful for, say,
#' ammending rainfal information with irrigation data.
#'
#' @param env.data A \code{data.frame} of environmental data from the outputs of \code{\link{get_weather_data}},
#' \code{\link{get_historical_weather_data}}, or \code{\link{get_soil_data}}.
#' @param suppl.data A \code{data.frame} with data to ammend to the \code{env.data} input.
#' @param operation A named vector specifying the editing operation for each variable. For instance, if
#' \code{operation = c("rain" = "add")}, rainfall information in \code{env.data} will be edited by
#' adding the rainfall data from \code{suppl.data}. Operations can include any accepted mathematical operation (e.g. "+", "-", "*")
#' or "replace" indicating that values will be edited by replacement.
#'
#'
#' @examples
#' \dontrun{
#' # Get daily weather data
#' trial_weather <- get_weather_data(trial.info = trial_info, source = "daymet")
#'
#' # Modify the trial irrigation data to set irrigated water as "rain"
#' trial_irrigation_data$rain <- trial_irrigation_data$irrigation_mm
#'
#' # Edit the trial weather data with this irrigation information
#' trial_weather1 <- edit_environmental_data(env.data = trial_weather, suppl.data = trial_irrigation_data)
#' }
#'
#'
#' @export
#'
edit_environmental_data <- function(env.data, suppl.data, operation = c("all" = "+")) {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  stopifnot(is.data.frame(suppl.data))
  stopifnot(is.character(operation))

  if (is.null(names(operation))) stop("The 'operation' input does not have names.")

  if (!is_environmental_data(env.data)) stop("The 'env.data' input does not appear to be environmental data.")

  # KATS comment: the next code block should just be a function in hidden_functions.R similar to check_trial_info() except that
  #     it takes as another parameter the list 'desired_cols'.
  
  # Make sure that suppl.data contains the appropriate columns
  desired_cols <- c("trial", "yday")
  if (!all(desired_cols %in% names(suppl.data))) {
    stop("The following necessary elements were not found in 'trial.info':",
         paste0(desired_cols[!desired_cols %in% names(suppl.data)], collapse = ", "))
  }

  # The names of operation should either be 'all' or be shared with suppl.data
  if (!(all(names(operation) %in% "all") | all(names(operation) %in% names(suppl.data)))) {
    stop("The names of 'operation' should either be 'all' or they must match the column names in 'suppl.data'.")
  }

  # List of common trials
  trial_to_edit <- intersect(suppl.data$trial, env.data$trial)

  # If env.data and suppl.data contain no shared trials, issue a warning and return env.data
  if (length(trial_to_edit) == 0) {
    warning("No common trials were found between 'env.data' and 'suppl.data'. Returning 'env.data' unmodified...")

  } else {
    # Else begin the editing process

    # Iterate over the edit trials
    for (tr in trial_to_edit) {
      # Index of the trial in the data.frame
      tr_i <- which(env.data$trial == tr)
      # Pull out the env.data for that trial
      env_data_toedit <- env.data$data[[tr_i]]

      variables_edit <- setdiff(intersect(names(env_data_toedit), names(suppl.data)), "yday")
      # Does suppl.data and env_data_toedit contain common names?
      if (length(variables_edit) == 0) {
        stop("The trials to edit in 'env.data' do not share variable names with 'suppl.data'.")

      } else {
        # Else proceed

        # Iterate over variables
        for (vari in variables_edit) {
          # Find the editing operation
          opr <- ifelse(all(names(operation) %in% "all"), operation[1], operation[vari])

          # Subset suppl.data for the variable"
          suppl_data_tomerge <- suppl.data[suppl.data$trial == tr,c("yday", vari)]

          # Merge
          env_data_toedit1 <- merge.data.frame(x = env_data_toedit, y = suppl_data_tomerge, by = "yday", all = TRUE)

          # Convert NAs to 0 if the operation is + or -; convert to 1 if operation is * or / or ^
          .y <- env_data_toedit1[[paste0(vari, '.y')]]
          if (opr %in% c("+", "-")) {
            env_data_toedit1[[paste0(vari, '.y')]] <- ifelse(is.na(.y), 0, .y)
          } else {
            env_data_toedit1[[paste0(vari, '.y')]] <- ifelse(is.na(.y), 1, .y)
          }

          # Edit
          env_data_toedit1[[vari]] <- eval(parse(text = paste("env_data_toedit1[[paste0(vari, '.x')]]",
                                                              opr, "env_data_toedit1[[paste0(vari, '.y')]]")))
          # Remove columns with .x or .y
          env_data_edited <- env_data_toedit1[!grepl(pattern = "\\.x|\\.y", x = names(env_data_toedit1))]

          # Replace env_data_toedit with env_data_edited
          env_data_toedit <- env_data_edited

        } # Close the variable loop

      } # Close the ifelse statement

      # Replace the trial env.data with env_data_edited
      env.data$data[[tr_i]] <-  env_data_toedit

    } # Close the trial loop

  } # Close the ifelse statement

  # Return the edited or undedited env.data
  return(env.data)

} # Close the function




#' Summarize environmental data according to growth stages
#'
#' @description
#' Use categorical growth stage data to summarize quantitative environmental variables.
#'
#' @param env.data A \code{data.frame} of trials with daily environmental data and categorical growth stage information, as created by \code{\link{define_growth_stages}}.
#' @param growth.stage.col The column in the elements of 'data' in \code{env.data} that contains the categorical growth stage information.
#' @param .funs A named list of formulas specifying the summary function for a specific variable (the names of \code{.funs}). Only the variables listed in the names of \code{.funs} are summarized.
#' @param ... Other arguments to pass to the functions listed in \code{.funs} (e.g. \code{na.rm = TRUE}).
#' @param unite Logical. Should growth stages and variable names be combined?
#'
#'
#' @examples
#' \dontrun{
#' # Get weather data for each trial
#' trial_weather <- get_weather_data(trial.info = trial_info, source = "daymet")
#'
#' # Run a crop model for each trial
#' trial_crop_growth <- run_apsim_crop_model(env.data = trial_weather, base.model.path = "barley")
#'
#' # Use the zadok_stage to assign growth stages
#' barley_stages <- stages <- list(
#'   early_vegetative = "x >= 10 & x <= 30", late_vegetative = "x > 30 & x <= 50",
#'   flowering = "x > 50 & x <= 70", grain_fill = "x > 70 & x <= 91"
#' )
#'
#' # Assign growth stages using the zadok stage
#' trial_crop_stages <- define_growth_stages(env.data = trial_crop_growth, growth.stage.delim = "zadok_stage",
#'                                           stages = barley_stages)
#'
#' # A list of funs for summary
#' summary_funs <- list(rain = ~sum, radn = ~sum, maxt = ~mean, mint = ~mean, daylength = ~mean)
#'
#' # Summarize environmental data according to the growth stages
#' trial_data_summarized <- summarize_environmental_data(env.data = trial_crop_stages, .funs = summary_funs)
#'
#' }
#'
#' @export
#'
summarize_environmental_data <- function(env.data, growth.stage.col = "growth_stage", .funs, ...,
                                         unite = FALSE) {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  stopifnot(is.character(growth.stage.col))
  stopifnot(length(growth.stage.col) == 1)
  stopifnot(is.list(.funs))
  # Make sure .funs has names
  if (is.null(names(.funs))) stop("'.funs' must have names.")
  stopifnot(is.logical(unite))

  # Make sure weather is in the data.col attribute
  if (!grepl(pattern = "weather", x = attr(env.data, "data.col"))) {
    stop("The 'env.data' input does not appear to contain weather (not soil) data.")
  }

  # Check for growth stages in the attributes of env.data
  if (!grepl(pattern = "growth.stages", x = attr(env.data, "data.col"))) {
    stop("The 'env.data' input does not appear to contain growth stage information.")
  }

  # Make sure growth.stage.col is in each of the 'data' elements
  is_col_present <- all(sapply(X = lapply(X = env.data$data, FUN = names), function(nm) growth.stage.col %in% nm) )
  if (!is_col_present) stop("The variable passed in 'growth.stage.col' (", growth.stage.col,
                            ") is not found in all of the 'data' elements of 'env.data'.")

  # Each element of stages must be a formula
  if (!all(sapply(.funs, inherits, "formula"))) stop("All elements of '.funs' must be formulas.")

  # Make sure that the names of .funs are in each element of 'data'
  are_vars_present <- all(sapply(X = lapply(X = env.data$data, FUN = names), function(nm) all(names(.funs) %in% nm)) )
  if (!are_vars_present) stop("The variable names found in the names of '.funs' are not found in all of the 'data' elements of 'env.data'.")

  ##


  # Loop over the elements in 'data'
  # Define an output list
  nTrial <- nrow(env.data)
  output <- vector("list", length = nTrial)

  # Iterate over rows in env.data
  for (i in seq_along(output)) {

    # Pull out the env data
    env_data_i <- env.data$data[[i]]
    # Create a list for storing summaries
    summaries <- vector("list", length = length(.funs))

    # Iterate over the summaries/.funs
    for (j in seq_along(summaries)) {
      # Parse the jth formula in funs and use that as the formula to summarize in tapply.
      fun_j <- all.vars(.funs[[j]])
      # Use the jth name in .funs to subset env_data_i
      var_sum <- aggregate(formula = reformulate(termlabels = growth.stage.col, response = names(.funs)[[j]]),
                           data = env_data_i, FUN = eval(parse(text = fun_j)), ... = ...)
      # Append the summary function to the variable name
      names(var_sum)[names(var_sum) == names(.funs)[[j]]] <- paste0(names(.funs)[[j]], "_", fun_j)

      # Add to the list
      summaries[[j]] <- var_sum

    }

    # Collapse the summaries
    summaries1 <- Reduce(f = function(x, y) merge.data.frame(x = x, y = y, by = growth.stage.col), x = summaries)

    # Unite growth stages and variable, if called
    if (unite) {
      summaries1_long <- reshape(data = summaries1, idvar = growth.stage.col, direction = "long", varying = list(2:ncol(summaries1)),
                                 v.names = "value", timevar = "variable", times = setdiff(names(summaries1), growth.stage.col))
      row.names(summaries1_long) <- NULL
      # Combine the growth stage and variable
      summaries1_long$variable <- paste(summaries1_long[[growth.stage.col]], summaries1_long$variable, sep = ".")
      summaries1_long <- summaries1_long[setdiff(names(summaries1_long), growth.stage.col)]

      # Convert to wide
      summaries1_wide <- as.data.frame(t(summaries1_long$value))
      names(summaries1_wide) <- summaries1_long$variable

      # Add this to the output
      output[[i]] <- summaries1_wide

    } else {
      # Else just add summaries to the output
      # Add to the output list
      output[[i]] <- summaries1

    }

  }

  # Create a copy of env.data
  env_data1 <- env.data
  # Replace 'data' in this copy with the output
  env_data1$data <- output

  # Return the df with a new attribute
  return(structure(env_data1, data.col = "summarized.environmental.data"))

}


#' Merge environmental data
#'
#' @description
#' Merge two or more sources of environmental data to create a single \code{data.frame}.
#'
#' @param ... Two or more \code{data.frames} of environmental data. Note that elements of the 'data' column in each data frame must share either a row or column dimension.
#'
#'
#' @examples
#' \dontrun{
#' # Get weather data for each trial
#' trial_weather <- get_weather_data(trial.info = trial_info, source = "daymet")
#' # Get soil data for each trial
#' hwsd_path <- "path/to/hwsd.bil"
#' trial_soil <- get_soil_data(trial.info = trial_info, hwsd.path = hwsd_path)
#'
#' # Run a crop model for each trial
#' trial_crop_growth <- run_apsim_crop_model(env.data = trial_weather, base.model.path = "barley")
#'
#' # Use the zadok_stage to assign growth stages
#' barley_stages <- stages <- list(
#'   early_vegetative = "x >= 10 & x <= 30", late_vegetative = "x > 30 & x <= 50",
#'   flowering = "x > 50 & x <= 70", grain_fill = "x > 70 & x <= 91"
#' )
#'
#' # Assign growth stages using the zadok stage
#' trial_crop_stages <- define_growth_stages(env.data = trial_crop_growth, growth.stage.delim = "zadok_stage",
#'                                           stages = barley_stages)
#'
#' # A list of funs for summary
#' summary_funs <- list(rain = ~sum, radn = ~sum, maxt = ~mean, mint = ~mean, daylength = ~mean)
#'
#' # Summarize environmental data according to the growth stages
#' trial_data_summarized <- summarize_environmental_data(
#'   env.data = trial_crop_stages,
#'   .funs = summary_funs,
#'   unite = TRUE
#' )
#'
#' # Merge the summaried weather data and the soil data
#' env_data_merged <- merge_environmental_data(trial_data_summarized, trial_soil)
#'
#' }
#'
#' @importFrom dplyr as_tibble
#'
#' @export
#'
merge_environmental_data <- function(...) {

  # Capture the arguments
  env_data_list <- list(...)

  ## Error checking ##
  # Class checks
  if (!all(sapply(env_data_list, is.data.frame))) stop("Data passed to this function must be data.frames.")

  # Check the data.col attribute of each
  if (any(sapply(X = sapply(X = env_data_list, attr, "data.col"), is.null))) {
    stop("One or more of the data frames does not appear to contain environmental (weather or soil) data.")
  }

  # Make sure the data column elements share either a row or column dimension
  env_data_list_dims <- lapply(X = do.call("c", lapply(X = env_data_list, FUN = "[[", "data")), FUN = dim)
  env_data_list_dims_rows <- sapply(X = env_data_list_dims, FUN = "[[", 1)
  env_data_list_dims_col <- sapply(X = env_data_list_dims, FUN = "[[", 2)

  # Determine the common dimension
  if (length(unique(env_data_list_dims_rows)) == 1) {
    common_dim <- "rows"

  } else if (length(unique(env_data_list_dims_col)) == 1) {
    common_dim <- "columns"

  } else {
    stop("The 'data' elements of each passed data frame must share a row or column dimension.")
  }

  # Rename 'data' in each element to prepare for merge
  env_data_list1 <- mapply(env_data_list, seq_along(env_data_list),
                           FUN = function(.x, .y) {names(.x)[names(.x) == "data"] <- paste0("data.", .y); .x })

  # Merge the data
  # Find common columns - but don't use 'data'
  common_cols <- Reduce(intersect, lapply(X = env_data_list1, FUN = names))

  # Merge
  merge_fun <- function(x, y) merge.data.frame(x = x, y = y, by = common_cols, )
  env_data_merged <- Reduce(f = merge_fun, x = env_data_list1)

  # Merge the 'data' cols
  # What function to use for merging
  bind_fun <- ifelse(common_dim == "rows", "cbind", "rbind")
  data_cols <- env_data_merged[grepl(pattern = "data\\.[0-9]{1,}", x = names(env_data_merged))]
  output <- vector("list", length = nrow(env_data_merged))

  # Iterate over rows in env_data_merged
  for (i in seq_along(output)) output[[i]] <- do.call(bind_fun, lapply(X = unname(data_cols), FUN = "[[", i))

  # Add output to the merged data
  env_data_merged$data <- output

  # Remove the other data cols and return
  env_data_merged1 <- as_tibble(env_data_merged[!grepl(pattern = "data\\.[0-9]{1,}", x = names(env_data_merged))])
  # Set data.col attributes
  return(structure(env_data_merged1, data.col = "merged.environmental.data"))

}




#' Create an environmental covariable matrix
#'
#' @description
#' Creates a \code{n x p} matrix for \code{n} environments and \code{p} covariables for use in modeling or to create environmental relationship matrices. Values of the covariables are centered and scaled.
#'
#' @param env.data A \code{data.frame} of environmental variables in long format.
#' @param env.col The column in \code{env.data} that designates environments.
#' @param var.col The column in \code{env.data} that designates the names of the variables.
#' @param val.col The column in \code{env.data} that designates the value of the variables.
#' @param check.data Logical. Should a check for normality of each variables be conducted?
#' @param alpha The significance level at which to reject the null hypothesis of the normality test.
#' @param scale.data Logical. Should the data matrix be centered and scaled prior to output?
#'
#'
#' @details
#'
#'
#'
#' @examples
#' \dontrun{
#' # Get weather data for each trial
#' trial_weather <- get_weather_data(trial.info = trial_info, source = "daymet")
#' # Get soil data for each trial
#' hwsd_path <- "path/to/hwsd.bil"
#' trial_soil <- get_soil_data(trial.info = trial_info, hwsd.path = hwsd_path)
#'
#' # Run a crop model for each trial
#' trial_crop_growth <- run_apsim_crop_model(env.data = trial_weather, base.model.path = "barley")
#'
#' # Use the zadok_stage to assign growth stages
#' barley_stages <- stages <- list(
#'   early_vegetative = "x >= 10 & x <= 30", late_vegetative = "x > 30 & x <= 50",
#'   flowering = "x > 50 & x <= 70", grain_fill = "x > 70 & x <= 91"
#' )
#'
#' # Assign growth stages using the zadok stage
#' trial_crop_stages <- define_growth_stages(env.data = trial_crop_growth, growth.stage.delim = "zadok_stage",
#'                                           stages = barley_stages)
#'
#' # A list of funs for summary
#' summary_funs <- list(rain = ~sum, radn = ~sum, maxt = ~mean, mint = ~mean, daylength = ~mean)
#'
#' # Summarize environmental data according to the growth stages
#' trial_data_summarized <- summarize_environmental_data(
#'   env.data = trial_crop_stages,
#'   .funs = summary_funs,
#'   unite = TRUE
#' )
#'
#' # Merge the summaried weather data and the soil data
#' env_data_merged <- merge_environmental_data(trial_data_summarized, trial_soil)
#'
#' # Use tidyr to reshape the data
#' library(tidyr)
#' env_data_merged1 <- env_data_merged %>%
#'   unnest(data) %>%
#'   gather(variable, value, c("latitude", "longitude", "elevation",
#'                             names(env_data_merged$data[[1]])))
#'
#' # Create a matrix of n environments by p covariables
#' E_matrix <- ec_matrix(env.data = env_data_merged1, env.col = "trial",
#'                       var.col = "variable", val.col = "value")
#'
#' }
#'
#' @export
#'
ec_matrix <- function(env.data, env.col, var.col = "variable", val.col = "value",
                      check.data = TRUE, alpha = 0.05, scale.data = TRUE) {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  stopifnot(is.character(env.col))
  stopifnot(is.character(var.col))
  stopifnot(is.character(val.col))
  stopifnot(is.logical(check.data))
  stopifnot(is.numeric(alpha))
  stopifnot(is.logical(scale.data))

  # Alpha must be between 0 and 1
  stopifnot(alpha > 0, alpha < 1)

  # Make sure env.col, var.col, and val.col are in the names of env.data
  if (!env.col %in% names(env.data)) stop("The column '", env.col, "' is not in 'env.data'.")
  if (!var.col %in% names(env.data)) stop("The column '", var.col, "' is not in 'env.data'.")
  if (!val.col %in% names(env.data)) stop("The column '", val.col, "' is not in 'env.data'.")


  # If check.data, perform a check on the variable data
  if (check.data) {

    # Create a vector of variables to remove
    vars_to_remove <- character()

    # First summarized min, max, mean, sd, and cv for each variable
    env_data_summary <- aggregate(reformulate(var.col, val.col), data = env.data, FUN = function(x) {
      c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
    })
    env_data_summary <- cbind(env_data_summary[var.col], env_data_summary[[val.col]])
    # Add CV
    env_data_summary$cv <- env_data_summary$sd / env_data_summary$mean

    # Remove variables with NA cv
    vars_to_remove <- union(vars_to_remove, env_data_summary[[var.col]][is.na(env_data_summary$cv)])


    ## Test variables for normality
    suppressWarnings(normality_test <- tapply(X = env.data[[val.col]], INDEX = env.data[[var.col]],
                                              FUN = function(x) ks.test(x = x, y = "pnorm", mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))))

    # Add results to the summary df
    env_data_summary$normality_pvalue <- sapply(normality_test, "[[", "p.value")
    # Correct for multiple testing
    env_data_summary$normality_pvalue_adj <- p.adjust(p = env_data_summary$normality_pvalue, method = "bonf")

    # Remove variables that fail the test
    vars_to_remove <- union(vars_to_remove, env_data_summary[[var.col]][env_data_summary$normality_pvalue_adj < alpha])

    # Remove these variables
    env.data1 <- env.data[!env.data[[var.col]] %in% vars_to_remove, ]

  } else {
    env.data1 <- env.data

  }

  # Impute covariate values with the mean
  env.data1_split <- split(env.data1, env.data1[[var.col]])
  env.data2 <- do.call("rbind", lapply(X = env.data1_split, FUN = function(dat) { dat[[val.col]] <- impute(x = dat[[val.col]]); return(dat) }))


  # Create the matrix
  # First subset relevant columns
  env.data3 <- as.data.frame(env.data2[c(env.col, var.col, val.col)])
  # Spread
  env.data4 <- reshape(data = env.data3, direction = "wide", idvar = env.col, v.names = val.col, timevar = var.col)
  # Designate row.names and convert to matrix
  row.names(env.data4) <- env.data4[[env.col]]
  env.data5 <- as.matrix(env.data4[!names(env.data4) %in% env.col])
  # Adjust names
  colnames(env.data5) <- gsub(pattern = paste0(val.col, "."), replacement = "", x = colnames(env.data5))

  # Center and scale the data, if called
  if (scale.data) {
    env.data_scaled <- scale(env.data5)
    return(env.data_scaled)
  } else {
    return(env.data5)
  }

}





















