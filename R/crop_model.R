#' Predict historical planting dates
#'
#' @description
#' Uses the crop calendar of long-term planting date information from the Center
#' for Sustainability and the Global Environment at the University of Wisconsin to
#' predict historical planting dates using historical weather data.
#'
#' @param env.data A \code{data.frame} of historical environmental data from the
#' output of \code{\link{get_historical_weather_data}}.
#' @param crop The type of crop for which planting date information is desired. Must be one of the crops listed in the crop calendar.
#' @param crop.calendar The data.frame of crop calendar information (available \href{http://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset}{here}). If not passed, the function will read in the data from this url (requires an internet connection).
#' @param meant.adj The amount to adjust the mean temperature at planting from the crop calendar when using this mean temperature as a minimum threshold to determine eligible planting dates from historical weather data.
#' @param daylen.adj The amount to adjust the daylength at planting from the crop calendar when using this daylength as a minimum threshold to determine eligible planting dates from historical weather data.
#' @param mint.lower The lowest daily minimum temperature acceptable for determining eligible planting dates from historical weather data.
#' @param rain.upper The highest daily rainfall acceptable for determining eligible planting dates from historical weather data.
#'
#'
#'
#' @details
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' # Find distinct location info
#' location_info <- dplyr::distinct(trial_info, location, latitude, longitude)
#'
#' # Retrieve historical weather data from 2005-2014 (10 years)
#' location_historical_weather <- get_historical_weather_data(
#'   location.info = location_info, start.year = 2005,
#'   end.year = 2014, source = "daymet"
#' )
#'
#' # Download the crop calendar data
#' crop_calendar <- read.csv("http://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/All_data_with_climate.csv")
#'
#' # Predict planting dates
#' location_predicted_planting <- predict_historical_planting_dates(
#'   env.data = location_historical_weather, crop = "barley - spring",
#'   crop.calendar = crop_calendar
#' )
#'
#' }
#'
#'
#' @references
#' http://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php
#'
#' Sacks, W.J., D. Deryng, J.A. Foley, and N. Ramankutty (2010). Crop planting dates: an analysis of global patterns.
#' Global Ecology and Biogeography 19, 607-620. DOI: 10.1111/j.1466-8238.2010.00551.x.
#'
#'
#' @importFrom sp spDists
#'
#'
#' @export
#'
predict_historical_planting_dates <- function(env.data, crop, crop.calendar, meant.adj = -2, daylen.adj = -0.5,
                                              mint.lower = 0, rain.upper = 0.5) {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  stopifnot(is.character(crop))

  # Check the env.data for the defining attribute
  if (!is_environmental_data(env.data)) stop("The 'env.data' input does not appear to be environmental data.")
  # Make sure it is historical data
  if (!attr(env.data, "data.col") == "historical.weather") {
    stop("The 'env.data' input does not appear to be historical environmental data.")
  }

  # Check if crop.calendar is missing; if so, read it in
  if (missing(crop.calendar)) {
    crop.calendar <- read.csv("http://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/All_data_with_climate.csv")

  } else {
    # Else verify that it is a data.frame
    stopifnot(is.data.frame(crop.calendar))

  }

  # Get a vector of unique crops
  all_crops <- sort(tolower(unique(crop.calendar$Crop.name.in.original.data)))
  # Make sure crop is among these:
  crop <- tolower(crop)
  crop_match <- pmatch(x = crop, table = all_crops)
  # If NA, error
  if (is.na(crop_match)) {
    stop("The 'crop' input does not match available crops in the crop calendar.")

  } else {
    # Else use the matching crop name
    crop_use <- all_crops[crop_match]

  }

  # Filter the crop calendar for this crop
  crop.calendar1 <- crop.calendar[tolower(crop.calendar$Crop.name.in.original.data) == crop_use,]
  # Create a matrix of latitude and longitude from this subset
  crop.calendar1.latlong <- crop.calendar1[,c("Location", "lon.avg", "lat.avg")]
  row.names(crop.calendar1.latlong) <- crop.calendar1.latlong$Location
  crop.calendar1.latlong <- as.matrix(crop.calendar1.latlong[-1])
  crop.calendar1.latlong <- crop.calendar1.latlong[rowSums(is.na(crop.calendar1.latlong)) == 0, , drop = FALSE]

  # Create lat/long information for the env.data
  env.data.latlong <- as.data.frame(env.data[c("trial", "longitude", "latitude")])
  row.names(env.data.latlong) <- env.data.latlong$trial
  env.data.latlong <- as.matrix(env.data.latlong[-1])

  # Calculate the GCD between each coordinate in env.data and the coordinates in the crop calendar
  coord_dist <- spDists(x = env.data.latlong, y = crop.calendar1.latlong, longlat = TRUE)
  # For each coordinate in env.data, find the index of the closest coordinate in the crop calendar
  which_coord_closest <- apply(X = coord_dist, MARGIN = 1, FUN = which.min)
  # Add this information to the env.data
  env.data$closest_location <- row.names(crop.calendar1.latlong)[which_coord_closest]

  # Add and empty vector for planting date
  env.data$planting_date <- as.character(NA)

  ## Loop over each row
  ## Determine the planting date in each year for each location
  for (i in seq(nrow(env.data))) {

    ## First pull the state to use with the crop calendar
    crop_calendar_state <- crop.calendar1[crop.calendar1$Location == env.data$closest_location[i],]

    ## Pull out relevant climate thresholds
    tmean <- floor(crop_calendar_state$temp.at.planting[1])
    daylen <- floor(crop_calendar_state$daylength.at.planting[1]) - 1

    # Pull out the historical weather data
    hist_env_data_i <- env.data$data[[i]]
    # Calculate meant
    hist_env_data_i$meant <- (hist_env_data_i$mint + hist_env_data_i$maxt) / 2

    # Filter dates that meet the thresholds defined by the crop calendar with modifiers
    # given by the user
    eligible_dates <- (hist_env_data_i$meant >= tmean + meant.adj & hist_env_data_i$daylength >= daylen + daylen.adj &
                         hist_env_data_i$mint >= mint.lower & hist_env_data_i$rain <= rain.upper)
    # Pick the first date
    planting_date_index <- min(which(eligible_dates), na.rm = TRUE)
    # Determine the date
    planting_date <- as.Date(x = hist_env_data_i$yday[planting_date_index] - 1, origin = paste0(unique(hist_env_data_i$year), "-01-01"))

    # Convert to character and add to the data.frame
    env.data$planting_date[i] <- as.character(planting_date)

  } # Close the loop

  # Return the dataframe
  return(env.data)

} # Close the function










#' Run APSIM crop model using current or historical weather data
#'
#' @description
#' Uses the APSIM software to run crop models based on user-supplied environmental data.
#'
#' @param env.data A \code{data.frame} of environmental data from the outputs of \code{\link{get_weather_data}}
#' or \code{\link{get_historical_weather_data}}. This must include planting date information.
#' @param wd The working directory. This function will write and read files, so it is recommended that you set an appropriate working directory.
#' @param base.model.path The path to a base crop model file (\code{.apsimx}) or the name of a crop model example
#' that is distributed in this package.
#' @param exe.path The path to the APSIM executable. If missing, the function will try to
#' guess where the executable is.
#' @param verbose Logical. Should progress messages be printed?
#'
#' @details
#' This function relies on the user having an installation of the \href{https://www.apsim.info/apsim-next-generation/}{APSIM Next Generation} software, which is freely available for academic use. Make sure you install this software prior to using this function.
#'
#'
#' This function relies heavily on the \code{\href{https://cran.r-project.org/package=apsimx}{apsimx}} package.
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
#' }
#'
#'
#' @importFrom apsimx apsimx apsimx_options edit_apsimx
#'
#' @export
#'
run_apsim_crop_model <- function(env.data, wd = ".", base.model.path, exe.path, verbose = TRUE) {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  stopifnot(is.logical(verbose))

  # Check the env.data for the defining attribute
  if (!is_environmental_data(env.data)) stop("The 'env.data' input does not appear to be environmental data.")

  # Make sure weather is in the environmental.data attribute
  if (!grepl(pattern = "weather", x = attr(env.data, "data.col"))) {
    stop("The 'env.data' input does not appear to contain weather (not soil) data.")
  }

  # Check env.data for planting date
  if (! "planting_date" %in% names(env.data)) {
    stop("'env.data' does not contain the necessary column 'planting date'.\n\nDid you include custom planting date information or use the 'predict_historical_planting_dates' function for historical environmental data?\n\n")
  }

  # Check base.model.path
  # First check if the file exists
  if (!file.exists(base.model.path)) {

    # Next check if it matches a file from the package
    package_path <- system.file(paste0("growth_model_resources/", base.model.path, ".apsimx"), package = "envirotyping")

    # If this does not exist, error out
    if (!file.exists(package_path)) {
      stop("The file specified in 'base.model.path' could not be found.
           \nThis must be either a) a path to a .apsimx file or b) one of the following base model examples distributed in this package: ",
           paste0(gsub(pattern = ".apsimx", replacement = "",
                       x = list.files(path = system.file("growth_model_resources", package = "envirotyping"))), collapse = ", "))

    }

  } else {
    package_path <- base.model.path

  }

  # Get the basenames of package_path without the extension
  package_path_base <- gsub(pattern = "(.*)(\\.apsimx)", replacement = "\\1", x = basename(package_path))

  # If exe.path is missing, use apsimx to find it
  if (missing(exe.path)) {
    exe.path <- apsimx:::auto_detect_apsimx()

  } else {
    # Else check it exists
    if (!file.exists(exe.path)) stop("The 'exe.path' file does not exist. Do you have APSIM installed?")

  }

  # Set options for apsim
  apsimx_options(exe.path = exe.path)


  ## Run the crop model per trial ##

  # Create an output list
  output <- vector("list", length = nrow(env.data))

  ## Loop over the trials
  for (i in seq_along(output)) {

    # Get the name of the trial
    trial_name <- env.data$trial[i]
    # Get the planting date
    pd <- as.Date(env.data$planting_date[i])
    # Get the environmental data
    env_data_i <- env.data$data[[i]]

    # Print a message if verbose == T
    if (verbose) cat("\nRunning the", package_path_base, "crop growth model for trial:", trial_name)

    # Create the MET file
    metfile <- create_MET(env.data = env.data[i,], dir = wd)

    # Create the name for the new crop model file
    apsim_i <- file.path(wd, paste0(package_path_base, "_", trial_name, ".apsimx"))
    # Copy the base apsim to this new file
    invisible(file.copy(from = package_path, to = apsim_i, overwrite = TRUE))

    ## Convert planting date to dd-mmm
    pd_apsim <- tolower(format.Date(pd, "%d-%b"))

    ## Edit the APSIMX file ##

    # Get the start and end dates of the weather data
    env_data_year <- unique(env_data_i$year)
    env_data_date_range <- range(as.Date(env_data_i$yday - 1, origin = paste0(env_data_year, "-01-01")))

    # Make the edits
    # Change start/end date
    edit_apsimx(file = basename(apsim_i), src.dir = wd, overwrite = TRUE, verbose = FALSE,
                node = c("Clock"), parm = c("Start", "End"),
                value = format(c(min(env_data_date_range), max(env_data_date_range)), "%m/%d/%Y"))
    # metfile
    edit_apsimx(file = basename(apsim_i), src.dir = wd, node = c("Weather"), parm = "met", value = metfile,
                overwrite = TRUE, verbose = FALSE)
    # planting date
    edit_apsimx(file = basename(apsim_i), src.dir = wd, node = c("Crop"), parm = "SowDate", value = pd_apsim,
                overwrite = TRUE, verbose = FALSE)

    # Run the simulation
    apsimx(file = basename(apsim_i), src.dir = wd, value = "none", silent = TRUE)
    apsim_out <- read_apsimx_report(dir = wd, apsim.name = gsub(pattern = ".apsimx", replacement = "", x = basename(apsim_i)))

    # Rename and add to the output list
    names(apsim_out) <- tolower(names(apsim_out))
    # Merge environmental data
    apsim_envdata_merge <- cbind(env_data_i, apsim_out)
    # Make the names correct
    names(apsim_envdata_merge) <- make.names(names(apsim_envdata_merge), unique = TRUE)
    # Remove any columns with .1
    apsim_envdata_merge1 <- apsim_envdata_merge[!endsWith(x = names(apsim_envdata_merge), suffix = ".1")]

    output[[i]] <- apsim_envdata_merge1

    # Delete the .apsimx file
    invisible(file.remove(apsim_i))
    # Delete the .apsimx db file
    invisible(file.remove(gsub(pattern = ".apsimx", replacement = ".db", x = apsim_i)))
    # Delete the met file
    invisible(file.remove(metfile))


  } # Close loop

  # Create a copy of env.data
  env_data1 <- env.data
  # Replace 'data' in this copy with the output
  env_data1$data <- output

  # Return the df with a new attribute
  return(structure(env_data1, data.col = paste0(attr(env.data, "data.col"), ".apsim.crop.model.data")))

}


#' Define crop growth stages
#'
#' @description
#' Uses the output of a crop growth model, or some other user-defined quantity,
#' to assign crop growth stages to environmental data.
#'
#' @param env.data A \code{data.frame} of trials with daily environmental data, as created from \code{\link{get_weather_data}} or \code{\link{get_historical_weather_data}} (or another function that uses this data as an input, e.g. \code{\link{run_apsim_crop_model}}. The variable you want to use to delineate growth stages must be contained in the 'data' element of this object.
#' @param growth.stage.delim The variable you want to use to delineate growth stages in the 'data' element of \code{env.data}. For example, this may be the day of the year, or Zadok numeric stages, or growing degree days. It is up to the user to make sure that this variable is present.
#' @param stages A named list with \code{character} expressions defining the growth stages based off the \code{growth.stage.delim} variable.  See \emph{Details} for examples.
#'
#' @details
#' The elements of \code{stages} must be parsable logical expressions using the dummy variable "x," which will automatically be converted to the variable specified in \code{growth.stage.delim}. The rows in the 'data' elements of \code{env.data} that meet the criteria specified in \code{stages} will be designated to the appropriate growth stage, as given by the names of \code{stages}. For example, if \code{growth.stage.delim = "zadok_stage"} and \code{stages = list(stage1 = "x >= 1 & x <= 10", stage2 = "x > 10 & x <= 20")}, then rows in the 'data' element in which 'zadok_stage' is >= 1 or <= 10 will be designated as 'stage1', and rows in which 'zadok_stage' is > 10 and <= 20 will be designated as 'stage2' and so on.
#'
#' @examples
#' \dontrun{
#' # Get weather data for each trial
#' trial_weather <- get_weather_data(trial.info = trial_info, source = "daymet")
#'
#' # Run a crop model for each trial
#' # Use the zadok_stage to assign growth stages
#' trial_crop_growth <- run_apsim_crop_model(env.data = trial_weather,base.model.path = "barley")
#'
#' barley_stages <- list(
#'   early_vegetative = "x >= 10 & x <= 30", late_vegetative = "x > 30 & x <= 50",
#'   flowering = "x > 50 & x <= 70", grain_fill = "x > 70 & x <= 91"
#' )
#'
#' trial_crop_stages <- define_growth_stages(env.data = trial_crop_growth, growth.stage.delim = "zadok",
#'                                           stages = barley_stages)
#'
#' }
#'
#'
#' @export
#'
define_growth_stages <- function(env.data, growth.stage.delim, stages) {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  stopifnot(is.character(growth.stage.delim))
  stopifnot(length(growth.stage.delim) == 1)
  stopifnot(is.list(stages))
  # Make sure stages has names
  if (is.null(names(stages))) stop("'stages' must have names.")

  # Make sure weather is in the environmental.data attribute
  if (!grepl(pattern = "weather", x = attr(env.data, "data.col"))) {
    stop("The 'env.data' input does not appear to contain weather (not soil) data.")
  }

  # Make sure growth.stage.delim is in each of the 'data' elements
  is_delim_present <- all(sapply(X = lapply(X = env.data$data, FUN = names), function(nm) growth.stage.delim %in% nm) )
  if (!is_delim_present) stop("The variable passed in 'growth.stage.delim' (", growth.stage.delim,
                              ") is not found in all of the 'data' elements of 'env.data'.")

  # Make sure that 'x' is in each element of stages
  if (!all(grepl(pattern = "x", x = stages))) {
    stop("You must use the dummy variable 'x' in the expressions in 'stages'. This represents the variable passed in 'growth.stage.delim'.")
  }

  # Each element of stages must be a character
  if (!all(sapply(stages, is.character))) stop("All elements of 'stages' must be characters.")

  ##

  # Parse the expressions
  stages1 <- lapply(X = stages, FUN = function(ep) parse(text = ep))


  # Define an output list
  nTrial <- nrow(env.data)
  output <- vector("list", length = nTrial)

  # Iterate over rows in env.data
  for (i in seq_along(output)) {

    # Pull out the env data
    env_data_i <- env.data$data[[i]]
    # Create a character vector of NAs for the growth stages
    growth_stages_i <- character(nrow(env_data_i))

    # Pull out a vector of the variable growth.stage.delim
    x <- env_data_i[[growth.stage.delim]]

    # Use the expressions in stages to define growth stages
    growth_stage_indices <- lapply(X = stages1, FUN = function(exp) which(eval(exp)))
    # Iterate over this list and assign stages
    for (j in seq_along(growth_stage_indices)) {
      growth_stages_i[growth_stage_indices[[j]]] <- names(growth_stage_indices)[j]
    }

    # Any element of growth_stages_i that is empty is assigned NA
    growth_stages_i[growth_stages_i == ""] <- NA

    # Add this to env_data_i and add that to the output
    env_data_i$growth_stage <- growth_stages_i
    output[[i]] <- env_data_i

  }

  # Create a copy of env.data
  env_data1 <- env.data
  # Replace 'data' in this copy with the output
  env_data1$data <- output

  # Return the df with a new attribute
  return(structure(env_data1, data.col = paste0(attr(env.data, "data.col"), ".growth.stages")))



}







