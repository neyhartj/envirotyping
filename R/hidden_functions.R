#' Hidden functions generally not to be directly called by the user
#'
check_trial_info <- function(trial.info) {

  # Check trial.info class
  if (!inherits(trial.info, c("list", "data.frame"))) stop("'trial.info' must be a list or a data frame.")

  # Check element names
  desired_cols <- c("trial", "year", "latitude", "longitude")
  if (!all(desired_cols %in% names(trial.info))) {
    stop("The following necessary elements were not found in 'trial.info':",
         paste0(desired_cols[!desired_cols %in% names(trial.info)], collapse = ", "))
  }

  # Check element classes
  if (!is.character(trial.info$trial)) stop("Trial names in 'trial.info' must be characters.")
  if (!is.numeric(trial.info$year)) stop("Years in 'trial.info' must be numeric")
  if (!is.numeric(trial.info$latitude)) stop("Latitude in 'trial.info' must be numeric")
  if (!is.numeric(trial.info$longitude)) stop("Longitude in 'trial.info' must be numeric")


  # KATS comment about the check made below: Is that check superfluous? R's data frames and the tidyverse
  # tibbles don't allow columns to have unequal length. I tried a lot of test examples and verified this

  # All elements must be of equal length
  if (length(unique(sapply(X = trial.info[desired_cols], FUN = length))) > 1) stop("All elements of 'trial.info' must be the same length.")

  return(TRUE)

}


#' @describeIn check_trial_info
#'
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#'
read_apsimx_report <- function(dir, apsim.name) {

  ## This function queries the database files produced by a completed apsimx
  ## simulation and reads in the report information
  ##
  ## Parts of this function were copied from read_apsimx from the apsimx package

  # Create the file
  db.file <- file.path(dir, paste0(apsim.name, ".db"))
  # Check if the file exists
  stopifnot(file.exists(db.file))

  # Establish a database con
  con <- dbConnect(SQLite(), db.file)
  # Get the report table
  tbl0 <- dbReadTable(con, "Report")
  # Disconnect
  dbDisconnect(con)

  if (any(grepl("Clock.Today", names(tbl0)))) {
    tbl0$Date <- try(as.Date(sapply(tbl0$Clock.Today, function(x) strsplit(x,  " ")[[1]][1])), silent = TRUE)
  }

  return(tbl0)

}

















#' @describeIn check_trial_info
#'
#' @importFrom raster crop unique extent
#' @importFrom DBI dbWriteTable dbGetQuery dbRemoveTable
#'
get_hwsd2 <- function(lat, long, gridsize, hwsd.raster, con) {

  # Error check
  stopifnot(class(hwsd.raster) == "RasterLayer")
  stopifnot(class(con) == "SQLiteConnection")


  # Create a box
  box <- c(long, long, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
  names(box) <- c("lon", "lon", "lat", "lat")

  # Trim the raster
  hwsd_crop <- crop(hwsd.raster, extent(box))

  # Place a temporary table in the connection
  dbWriteTable(con, name = "WINDOW_TMP", value = data.frame(smu_id = raster::unique(hwsd_crop)), overwrite = TRUE)
  # Query the database
  result <- dbGetQuery(con, "select T.* from HWSD_DATA as T join\nWINDOW_TMP as U on T.mu_global=u.smu_id order by su_sym90")
  # Remove the temporary table
  dbRemoveTable(con, "WINDOW_TMP")

  ## Return the results
  return(result)

}


#' @describeIn check_trial_info
#'
impute <- function(x) {
  x1 <- x
  x1[is.na(x1)] <- mean(x1, na.rm = T)
  return(x1)
}

#' @describeIn check_trial_info
#'
cv <- function(x, na.rm = FALSE) sd(x = x, na.rm = na.rm) / mean(x = x, na.rm = na.rm)


#' @describeIn check_trial_info
#'
fct_contr_sum <- function(x, drop.levels = FALSE) {
  stopifnot(is.factor(x))
  stopifnot(is.logical(drop.levels))

  # Drop levels, if called for
  x1 <- if (drop.levels) droplevels(x = x) else x

  # Redefine contrasts as sum-to-zero
  x1_contrasts <- contr.sum(levels(x1))
  colnames(x1_contrasts) <- head(levels(x1), -1)
  contrasts(x1) <- x1_contrasts
  return(x1)
}



#' @describeIn check_trial_info
#' Checks if an object contains weather or soil data from the functions \code{\link{get_weather_data}},
#' \code{\link{get_historical_weather_data}}, or \code{\link{get_soil_data}}.
#'
is_environmental_data <- function(x) attr(x, "data.col") %in% c("historical.weather", "weather", "soil")


#' @describeIn check_trial_info
#'
#' @importFrom APSIM prepareMet writeMetFile
#'
create_MET <- function(env.data, dir = ".") {

  ## Error checking ##
  # Class checks
  stopifnot(is.data.frame(env.data))
  # Make sure the directory exists
  stopifnot(dir.exists(dir))

  if (!is_environmental_data(env.data)) stop("The 'env.data' input does not appear to be environmental data.")

  # Make sure weather is in the data.col attribute
  if (!grepl(pattern = "weather", x = attr(env.data, "data.col"))) {
    stop("The 'env.data' input does not appear to contain weather (not soil) data.")
  }

  # Units
  met_units <- c("(oC)", "(oC)", "(MJ/m^2/day)", "(mm)", "()", "()")
  nTrial <- nrow(env.data)

  # Loop over trials
  for (i in seq_len(nTrial)) {

    # Get the name of the trial
    trial_name <- env.data$trial[i]
    # Create export name
    filename <- file.path(dir, paste0(trial_name, ".met"))

    # Get lat/long
    lonlat <- unlist(env.data[i,c("longitude", "latitude")])


    ## Re-order the data.frame
    trial_data_tosave <- env.data$data[[i]]
    ## Extract year and doy from the date
    trial_data_tosave$day <- trial_data_tosave$yday

    trial_data_tosave1 <- trial_data_tosave[c("maxt", "mint", "radn", "rain", "year", "day")]

    ## Prepare a met and then export it
    invisible(capture.output(met_to_save <- prepareMet(data = trial_data_tosave1, lat = lonlat[2], lon = lonlat[1], units = met_units)))
    writeMetFile(fileName = filename, met = met_to_save)

  } # Close loop

  # If only one MET file is produced, return the file path
  if (nTrial == 1) return(filename)

} # Close function

