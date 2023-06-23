#' Query public weather data
#'
#' @description
#' Queries weather data from publicly available datasets based on geographical coordinates.
#'
#' @param trial.info A list of trial metadata that includes the trial name, year,
#' latitude, and longitude. This can be a \code{data.frame}. See \emph{Details} for specific information.
#' @param source The database to query. Can be one of \code{"nasapower"} or \code{"daymet"}.
#' See \emph{Details} for more information.
#' @param verbose Logical. Should progress messages be printed?
#'
#' @details
#'
#'
#' @examples
#' \dontrun{
#' # Use the included trial_info data
#' trial_weather <- get_weather_data(trial.info = trial_info, source = "daymet")
#' }
#'
#'
#' @import nasapower
#' @import daymetr
#' @importFrom stringr str_extract str_remove
#' @importFrom tibble as_tibble
#'
#' @export
#'
get_weather_data <- function(trial.info, source = c("daymet", "nasapower"), verbose = TRUE) {

  ## Error check ##
  invisible(check_trial_info(trial.info = trial.info))

  # Number of trial
  nTrials <- length(trial.info$trial)

  # Match the data source argument
  source <- match.arg(source)
  stopifnot(is.logical(verbose))

  # Create an output list for each trial
  output <- vector("list", length = nTrials)

  # KATS note: We should create functions query_nasapower() and query_daymet() with the truly unique code
  # needed to extract code from those two sources. As it stands, there is a lot of cut and pasted code in
  # the clauses "if (source == "nasapower")" vs "else if (source = "daymet")". This will be helpful to
  # avoid poorly maintainable code as we expand to other sources (e.g., "else if (source = "twc")".

  # Control flow based on site
  if (source == "nasapower") {

    stop("Querying the 'nasapower' database is not yet supported.")

    ## Add the needed pars to the parameters list
    needed_pars <- c("mint" = "TS_MIN", "maxt" = "TS_MAX", "radn" = "ALLSKY_SFC_SW_DWN", "rain" = "PRECTOT")
    # Add names for the parameters (default is to lower)
    # KATS: pars is never defined. Why does this next line not flag an error?
    pars1 <- setNames(needed_pars, tolower(needed_pars))

    # Rename pars
    # KATS: Please expand the above 2-word comment to explain what you are trying to accomplish with
    # these three lines.
    pars2 <- union(needed_pars, pars1)
    names(pars2)[needed_pars %in% pars2] <- names(needed_pars)[needed_pars %in% pars2]
    names(pars2)[pars2 %in% setdiff(pars2, needed_pars)] <- names(pars1)[pars1 %in% setdiff(pars2, needed_pars)]

    # Separate renaming vector (with date)
    pars2_rename <- c("date" = "YYYYMMDD", pars2)

    # Loop over trials
    for (i in seq_along(output)) {

      # Get the name of the trial
      trial_name <- trial.info$trial[i]
      # Print a message
      if (verbose) cat("\nRetrieving weather data for trial: ", trial_name)

      # Get the year
      trial_year <- trial.info$year[i]
      ## Create dates using this year
      dates <- paste0(trial_year, c("-01-01", "-12-31"))

      # Get lat/long
      lonlat <- c(trial.info$longitude[i], trial.info$latitude[i])

      ## Pull data
      data_out <- get_power(community = "AG", pars = pars2, temporal_average = "DAILY", lonlat = lonlat, dates = dates)

      ## Get relevant data and rename
      data_out1 <- as.data.frame(data_out)[pars2_rename]
      names(data_out1) <- names(pars2_rename)

      # Remove missing values
      data_out1[data_out1 == -99] <- NA

      # Extract elevation
      elevation <- as.numeric(str_remove(string = str_extract(string = attr(data_out, "POWER.Elevation"),
                                                              pattern = "[0-9]*.[0-9]* meters"), pattern = " meters"))

      ## Output a  list
      output[[i]] <- list(elevation = elevation, data = data_out1)

    }

  } else if (source == "daymet") {

    # Loop over trials
    for (i in seq_along(output)) {

      # Get the name of the trial
      trial_name <- trial.info$trial[i]
      # Print a message
      if (verbose) cat("\nRetrieving weather data for trial: ", trial_name)

      # Get the year
      trial_year <- trial.info$year[i]
      ## Create dates using this year
      dates <- paste0(trial_year, c("-01-01", "-12-31"))

      # Get lat/long
      lat <- trial.info$latitude[i]
      lon <- trial.info$longitude[i]

      ## Pull data
      data_out <- download_daymet(lat = lat, lon = lon, start = trial_year, end = trial_year, internal = TRUE,
                                  silent = TRUE)

      ## Get relevant data and rename
      data_out1 <- data_out$data
      names(data_out1) <- c("year", "yday", "daylength_sec", "rain", "radn", "swe", "maxt", "mint", "vp")

      # Convert units
      # srad to daily total radiation
      data_out1$radn <- (data_out1$radn * data_out1$daylength_sec) / 1000000
      # Daylength to hours
      data_out1$daylength <- data_out1$daylength_sec / 3600

      # Remove some variables
      data_out2 <- data_out1[,-which(names(data_out1) %in% c("daylength_sec"))]

      # Extract elevation
      elevation <- data_out$altitude

      ## Output a list
      output[[i]] <- list(elevation = elevation, data = data_out2)

    }

  }

  ## Create a data.frame for output
  trial.info1 <- as.data.frame(trial.info)

  ## Add elevation to the df
  trial.info1$elevation <- sapply(X = output, FUN = "[[", "elevation")

  ## Convert to tibble
  trial.info1 <- as_tibble(trial.info1)

  ## Add the list of results
  trial.info1$data <- lapply(X = output, FUN = "[[", "data")

  # Return the df with a special attribute
  return(structure(trial.info1, data.col = "weather"))

}


#' Query public weather data over longer timeframes
#'
#' @description
#' Queries long-term weather data from publicly available datasets based on geographical coordinates.
#'
#' @param location.info A list of location metadata that includes the location name,
#' latitude, and longitude. This can be a \code{data.frame}. See \emph{Details} for specific information.
#' @param start.year The first year to gather data. (Note that the maximum year range between \code{start.year} and \code{end.year} is 30.)
#' @param end.year The last year to gather data. (Note that the maximum year range between \code{start.year} and \code{end.year} is 30.)
#' @param source The database to query. Can be one of \code{"nasapower"} or \code{"daymet"}.
#' See \emph{Details} for more information.
#' @param verbose Logical. Should progress messages be printed?
#'
#' @details
#'
#'
#' @examples
#' \dontrun{
#' # Find distinct location info
#' location_info <- dplyr::distinct(trial_info, location, latitude, longitude)
#'
#' # Retrieve historical weather data from 2005-2014 (10 years)
#' location_historical_weather <- get_historical_weather_data(
#'   location.info = location_info, start.year = 2005,
#'   end.year = 2014, source = "daymet"
#' )
#' }
#'
#'
#' @export
#'
get_historical_weather_data <- function(location.info, start.year, end.year, source = c("daymet", "nasapower"), verbose = TRUE) {

  ## Error check ##
  # Make start.year and end.year numeric
  start.year <- as.numeric(start.year)
  end.year <- as.numeric(end.year)

  # End year must be greater than start year
  stopifnot(end.year >= start.year)
  # Make sure end.year - start.year + 1 is <= 30
  stopifnot(end.year - start.year + 1 <= 30)

  # Create a sequence of years
  year_seq <- seq(start.year, end.year)

  # Check location.info class
  if (!inherits(location.info, c("list", "data.frame"))) stop("'location.info' must be a list or a data frame.")

  # KATS comment: The check element names with desired_cols assignment below
  # is cut and paste code from what is found in the hidden_functions.R
  # check_trial_info() function. This should be made a generic function
  # check_element_names() in the file hidden_functions.R.

  # Check element names
  desired_cols <- c("location", "latitude", "longitude")
  if (!all(desired_cols %in% names(location.info))) {
    stop("The following necessary elements were not found in 'trial.info':",
         paste0(desired_cols[!desired_cols %in% names(location.info)], collapse = ", "))
  }

  # Create a trial.info list from location.info
  trial.info <- expand.grid(location = location.info$location, year = year_seq, stringsAsFactors = FALSE)
  trial.info$trial <- paste0(trial.info$location, "_", trial.info$year)
  trial.info <- merge(trial.info, location.info)
  # Reorder
  trial.info <- trial.info[order(trial.info$location, trial.info$year),]

  # Pass arguments to get_weather_data
  trial.info.weather <- get_weather_data(trial.info = trial.info, source = source, verbose = verbose)

  # Return the df with a special attribute
  return(structure(trial.info.weather, data.col = "historical.weather"))

}









#' Query soil data from the Harmonized World Soil Database
#'
#' @description
#' Queries soil data from the Harmonized World Soil Database based on geographical coordinates.
#'
#' @param trial.info A list of trial metadata that includes the trial name, year,
#' latitude, and longitude. This can be a \code{data.frame}. See \emph{Details} for specific information.
#' @param verbose Logical. Should progress messages be printed?
#'
#' @details
#' Only numeric soil variables are returned, and they are summarized per trial by calculating the average variable
#' value across soil IDs weighted by the share of that soil ID in the mapping unit.
#'
#'
#' @examples
#' \dontrun{
#' # Use the included trial_info data
#' trial_soil <- get_soil_data(trial.info = trial_info)
#' }
#'
#' @import hwsdr
#' @importFrom tibble as_tibble tibble
#'
#' @export
#'
get_soil_data <- function(trial.info, verbose = TRUE) {

  ## Error check ##
  # Check trial.info
  invisible(check_trial_info(trial.info = trial.info))
  # Number of trial
  nTrials <- length(trial.info$trial)
  # Check verbose
  stopifnot(is.logical(verbose))

  # Get unique location, latitude, longitude rows
  trial.info1 <- trial_info[, c("latitude", "longitude")]
  trial.info1 <- trial.info1[!duplicated(trial.info1),]
  trial.info1$site <- paste0("site", seq_len(nrow(trial.info1)))
  trial.info1 <- trial.info1[,c("site", "latitude", "longitude")]

  # Create an output list for each trial
  output <- vector("list", length = nrow(trial.info1))

  # Iterate over the trials
  for (i in seq_along(output)) {
    coords <- c(trial.info1$latitude[i], trial.info1$longitude[i])
    # Print a message
    if (verbose) cat("\nRetrieving soil data for coordinates: ", paste0(round(coords, 2), collapse = ", "))

    grid <- c(coords - grid.size, coords + grid.size)

    values <- ws_subset(location = coords, site = trial.info1$site[i])

    # Add trial names to the ouput
    output[[i]] <- values

  }

  #

  # Edit the output
  # First row bind
  soil_data_out1 <- do.call("rbind", output)

  # Rename variables; merge the variable data.frame
  soil_data_out2 <- merge(x = soil_data_out1[,c("site", "parameter", "value")],
                          y = as.data.frame(hwsd.variables)[,c("field", "type", "variable")],
                          by.x = "parameter", by.y = "field")

  # Subset only properties
  soil_data_out3 <- subset(soil_data_out2, type == "property")

  # Select the site, value, and variable columns
  soil_data_out4 <- soil_data_out3[,c("site", "variable", "value")]

  # Convert from long to wide
  soil_data_out5 <- reshape(data = soil_data_out4, direction = "wide", idvar = "site", timevar = "variable", v.names = "value")
  names(soil_data_out5) <- sub(pattern = "^value\\.", replacement = "", x = names(soil_data_out5))

  # Create a nested tibble
  soil_data_out6 <- tibble(site = soil_data_out5$site, data = split(soil_data_out5, soil_data_out5$site))

  # Merge with the trial_info
  soil_data_out7 <- as_tibble(merge(merge(trial_info, trial.info1), soil_data_out6))
  # Drop unnecessary columns
  soil_data_out8 <- soil_data_out7[,c(names(trial_info), "data")]
  soil_data_out8$data <- unname(soil_data_out8$data)

  # Return the df with a special attribute
  return(structure(soil_data_out8, data.col = "soil"))

}

