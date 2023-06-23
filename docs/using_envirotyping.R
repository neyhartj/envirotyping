## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

# Other packages for this vignette
library(dplyr)
library(tidyr)

## ----setup--------------------------------------------------------------------
#  library(envirotyping)

## ----get.daily.weather--------------------------------------------------------
#  # Retrieve daily weather observations in the same years as the trials
#  daily_weather <- get_weather_data(trial.info = trial_info)

## ----get.soil-----------------------------------------------------------------
#  # Path to the HWSD raster folder with hwsd.bil
#  hwsd_path <- "path/to/hwsd.bil"
#  
#  # Retrieve soil data for each location
#  soil_data <- get_soil_data(trial.info = trial_info, hwsd.path = hwsd_path)

## ----get.historical .weather--------------------------------------------------
#  # Distinct set of location info
#  location_info <- dplyr::distinct(trial_info, location, latitude, longitude)
#  
#  # Retrieve 5 years of historical daily weather observations
#  historical_daily_weather <- get_historical_weather_data(
#    location.info = location_info, start.year = 2010, end.year = 2014
#  )

## ----edit.weather-------------------------------------------------------------
#  # Modify the trial irrigation data to set irrigated water as "rain"
#  trial_irrigation_data$rain <- trial_irrigation_data$irrigation_mm
#  
#  # Edit the trial weather data with this irrigation information
#  daily_weather1 <- edit_environmental_data(env.data = daily_weather, suppl.data = trial_irrigation_data,
#                                            operation = c("rain" = "+"))
#  

## ----run.crop.model-----------------------------------------------------------
#  # Run a crop model for each trial
#  trial_crop_growth <- run_apsim_crop_model(env.data = daily_weather1, base.model.path = "barley")

## ----define.growth.stages-----------------------------------------------------
#  # Create a list of growth stages delineated by a variable "x"
#  barley_stages <- list(
#   early_vegetative = "x >= 10 & x <= 30",
#   late_vegetative = "x > 30 & x <= 50",
#   flowering = "x > 50 & x <= 70",
#   grain_fill = "x > 70 & x <= 91"
#  )
#  

## ----define.growth.stages2----------------------------------------------------
#  # Use these delineations to mark growth stages from the crop model
#  trial_crop_stages <- define_growth_stages(
#    env.data = trial_crop_growth,
#    growth.stage.delim = "zadok",
#    stages = barley_stages
#  )

## ----define.growth.stages.alt, eval=FALSE, include=FALSE----------------------
#  
#  # This code block is for testing purposes only.
#  
#  daily_weather2 <- daily_weather1 %>%
#    mutate(planting_date_yday = lubridate::yday(planting_date),
#           data = purrr::map2(data, planting_date_yday, ~mutate(.x, dap = yday - .y)))
#  
#  
#  # Create a list of growth stages delineated by a variable "x"
#  barley_stages <- list(
#   early_vegetative = "x >= 10 & x <= 30",
#   late_vegetative = "x > 30 & x <= 50",
#   flowering = "x > 50 & x <= 70",
#   grain_fill = "x > 70 & x <= 91"
#  )
#  
#  
#  trial_crop_stages <- define_growth_stages(
#    env.data = daily_weather2,
#    growth.stage.delim = "dap",
#    stages = barley_stages
#  )
#  
#  

## ----define.growth.stages3----------------------------------------------------
#  # The early_vegatative element is not an expression
#  stages <- list(
#   early_vegetative = "x 10 & x <= 30", late_vegetative = "x > 30 & x <= 50",
#   flowering = "x >  50 & x <= 70", grain_fill = "x > 70 & x <= 91"
#  )
#  
#  # Use these delineations to mark growth stages from the crop model
#  define_growth_stages(
#    env.data = trial_crop_growth,
#    growth.stage.delim = "zadok",
#    stages = stages
#  )
#  

## ----define.growth.stages4----------------------------------------------------
#  # This list uses the variable name to be passes in growth.stage.delim
#  stages <- list(
#   early_vegetative = "zadok 10 & zadok <= 30",
#   late_vegetative = "zadok > 30 & zadok <= 50",
#   flowering = "zadok >  50 & zadok <= 70",
#   grain_fill = "zadok > 70 & zadok <= 91"
#  )
#  
#  # Use these delineations to mark growth stages from the crop model
#  define_growth_stages(
#    env.data = trial_crop_growth,
#    growth.stage.delim = "zadok_stage",
#    stages = stages
#  )
#  

## ----summarize.weather.data---------------------------------------------------
#  # A list defining how to summarize each variable
#  summary_funs <- list(rain = ~sum, radn = ~sum, maxt = ~mean, mint = ~mean, daylength = ~mean)
#  
#  # Summarize environmental data according to the growth stages
#  trial_data_summarized <- summarize_environmental_data(
#    env.data = trial_crop_stages,
#    growth.stage.col = "growth_stage",
#    .funs = summary_funs,
#    unite = TRUE
#  )
#  

## ----download.planting.date.info----------------------------------------------
#  # Download the crop calendar data
#  url <- "http://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/All_data_with_climate.csv"
#  crop_calendar <- read.csv(url)

## ----planting.date.historical-------------------------------------------------
#  # Predict planting dates
#  historical_daily_weather1 <- predict_historical_planting_dates(
#   env.data = historical_daily_weather,
#   crop = "barley - spring",
#   crop.calendar = crop_calendar,
#   meant.adj = -2, daylen.adj = -0.5,
#   mint.lower = 0, rain.upper = 0.5
#  )
#  

## ----growth.stage.historical1-------------------------------------------------
#  # Run a crop model for each historical "trial"
#  hist_crop_growth <- run_apsim_crop_model(
#    env.data = historical_daily_weather1, base.model.path = "barley"
#  )

## ----growth.stage.historical2-------------------------------------------------
#  # Define growth stages using the same set of barley stages
#  hist_crop_stages <- define_growth_stages(
#    env.data = hist_crop_growth,
#    growth.stage.delim = "zadok",
#    stages = barley_stages
#  )

## ----growth.stage.historical3-------------------------------------------------
#  # Summarize data using these growth stages
#  hist_data_summarized <- summarize_environmental_data(
#    env.data = hist_crop_stages,
#    .funs = summary_funs,
#    unite = TRUE
#  )
#  

## ----merge.trial.data---------------------------------------------------------
#  # Merge the daily weather data (summarized by growth stage) with the soil data
#  daily_weather_soil <- merge_environmental_data(trial_data_summarized, soil_data)
#  

## ----merge.trial.data1--------------------------------------------------------
#  # This will produce a data frame with zero rows
#  merge_environmental_data(trial_data_summarized, hist_data_summarized)
#  

## ----merge.historical.data----------------------------------------------------
#  # Merge the historical weather data (summarized by growth stage) with the soil data
#  # This function will merge based on location
#  hist_weather_soil <- merge_environmental_data(
#    hist_data_summarized,
#    dplyr::select(soil_data, location, data)
#  )

## ----tidy.daily.data----------------------------------------------------------
#  # Load the tidyr package
#  library(tidyr)
#  
#  # Reshape the data
#  daily_weather_soil1 <- daily_weather_soil %>%
#    unnest(cols = data) %>%
#    gather(variable, value, c("latitude", "longitude", "elevation",
#                              names(daily_weather_soil$data[[1]])))
#  

## ----create.daily.ecmat-------------------------------------------------------
#  # Create a n x p environmental covariable matrix
#  daily_ecmat <- ec_matrix(env.data = daily_weather_soil1, env.col = "trial",
#                           var.col = "variable", val.col = "value", check.data = TRUE)
#  

## ----tidy.hist.data-----------------------------------------------------------
#  # Reshape the data
#  hist_weather_soil1 <- hist_weather_soil %>%
#    unnest(cols = data) %>%
#    gather(variable, value, c("latitude", "longitude", "elevation",
#                              names(hist_weather_soil$data[[1]])))
#  

## ----summarize.hist.data------------------------------------------------------
#  # Calculate the average value of each covariable in each location over a few years
#  hist_weather_soil_aggregated <- aggregate(value ~ variable + location, data = hist_weather_soil1,
#                                            FUN = mean, subset = year %in% 2012:2014)
#  

## ----create.hist.ecmat--------------------------------------------------------
#  # Create a n x p environmental covariable matrix
#  hist_ecmat <- ec_matrix(env.data = hist_weather_soil_aggregated, env.col = "location",
#                          var.col = "variable", val.col = "value", check.data = TRUE)
#  

## ----all.ec.relmats-----------------------------------------------------------
#  # Construct a relationship matrix for environments
#  env_all_relmat <- ec_relmat(ec.matrix = daily_ecmat)
#  
#  # Construct a relationship matrix for locations using historical data:
#  loc_all_relmat <- ec_relmat(ec.matrix = hist_ecmat)

## ----model.daily.weather------------------------------------------------------
#  # Use stepwise regression to determine the important set of environmental
#  # covariables
#  variable_selection_out <- ec_variable_selection(
#   pheno.data = pheno_data_train,
#   env.data = daily_weather_soil1,
#   env.col = "trial",
#   gen.col = "line_name",
#   y.col = "value",
#   var.col = "variable",
#   val.col = "value"
#  )

## ----save.covariates----------------------------------------------------------
#  ec_table <- save_ec_selection_table(x = variable_selection_out)
#  ec_table
#  

## ----lm.ec.relmats------------------------------------------------------------
#  # Subset the environmental covariable matrix based on the results of the
#  # variable selection operation
#  # First select the main effect variables
#  daily_ecmat_main <- subset.matrix(x = daily_ecmat, select = variable_selection_out$variableEffects$main_effect)
#  # Next interaction variables
#  daily_ecmat_interaction <- subset.matrix(x = daily_ecmat, select = variable_selection_out$variableEffects$interaction_effect)
#  
#  # Create the relationship matrix
#  env_sel_relmat_main <- ec_relmat(ec.matrix = daily_ecmat_main)
#  env_sel_relmat_interaction <- ec_relmat(ec.matrix = daily_ecmat_interaction)
#  

## ----create.timeframes--------------------------------------------------------
#  # Create a list of years that accumulate from latest to earliest
#  timeframe_list <- Reduce(c, sort(unique(hist_weather_soil1$year)), accumulate = TRUE, right = TRUE)
#  
#  # Create a list of years forming a sliding window of two years
#  # Use the slider package to create this
#  window_list <- slider::slide(.x = sort(unique(hist_weather_soil1$year)), .f = c,
#                               .after = 1, .step = 1, .complete = TRUE)
#  # Remove NULL elements
#  window_list <- subset(window_list, !sapply(window_list, is.null))
#  

## ----hist.varsel--------------------------------------------------------------
#  # Add location information to the pheno_data_train object
#  pheno_data_train1 <- merge(pheno_data_train[-2], trial_info[c("trial", "location")])
#  
#  # Variable selection using the list of time frames
#  timeframe_ec_sel <- ec_variable_selection_slide(
#    pheno.data = pheno_data_train1, env.data = hist_weather_soil1,
#    env.col = "location", gen.col = "line_name", y.col = "value",
#    var.col = "variable", val.col = "value", time.col = "year",
#    time.list = timeframe_list
#  )
#  
#  # Variable selection using the list of sliding windows
#  window_ec_sel <- ec_variable_selection_slide(
#    pheno.data = pheno_data_train1, env.data = hist_weather_soil1,
#    env.col = "location", gen.col = "line_name", y.col = "value",
#    var.col = "variable", val.col = "value", time.col = "year",
#    time.list = window_list
#  )
#  

## ----create.hist.ecmat1-------------------------------------------------------
#  # Extract the historical environmental covariable matrix
#  hist_ecmat1 <- window_ec_sel$ecMatrixSelected
#  # Subset for the important covariables
#  hist_ecmat1 <- subset.matrix(x = hist_ecmat1, select = row.names(window_ec_sel$variableSelectionResults$optVariables))
#  
#  # Construct a relationship matrix
#  hist_sel_relmat <- ec_relmat(ec.matrix = hist_ecmat1)

## ----predict.unobs.env--------------------------------------------------------
#  # Rename the relationship matrices for consistency
#  Emat <- env_sel_relmat_main
#  Gmat <- Gmat
#  # The kronecker product models the interaction between the two matrices
#  GEmat <- kronecker(X = Gmat, Y = env_sel_relmat_interaction, make.dimnames = TRUE)
#  
#  # Subset phenotype data based on the genotypes that are in the Gmat
#  pheno_data_train2 <- subset(pheno_data_train1, line_name %in% row.names(Gmat))
#  # Add factors for genotypes and environments
#  pheno_data_train2$line_name <- factor(pheno_data_train2$line_name, levels = row.names(Gmat))
#  pheno_data_train2$trial <- factor(pheno_data_train2$trial, levels = row.names(Emat))
#  # Add a factor for interactions
#  pheno_data_train2$gxe <- interaction(pheno_data_train2$line_name, pheno_data_train2$trial, sep = ":")
#  
#  # Train the model using sommer
#  library(sommer)
#  model_train <- mmer(
#    fixed = value ~ 1,
#    random = ~ vs(line_name, Gu = Gmat) + vs(trial, Gu = Emat) + vs(gxe, Gu = GEmat),
#    data = pheno_data_train2,
#    verbose = FALSE
#  )
#  

## ----gen.pred-----------------------------------------------------------------
#  # Add the same levels to factors in pheno_data_target
#  pheno_data_test1 <- subset(pheno_data_test, line_name %in% row.names(Gmat))
#  pheno_data_test1$line_name <- factor(pheno_data_test1$line_name, levels = levels(pheno_data_train2$line_name))
#  pheno_data_test1$trial <- factor(pheno_data_test1$trial, levels = levels(pheno_data_train2$trial))
#  # Add a factor for interactions
#  pheno_data_test1$gxe <- interaction(pheno_data_test1$line_name, pheno_data_test1$trial, sep = ":")
#  
#  
#  ## Retrieve the predicted values
#  # Grand mean
#  mu <- model_train$Beta$Estimate
#  
#  # Genotypes
#  Z_genotypes <- model.matrix(~ 0 + line_name, pheno_data_test1)
#  u_genotypes <- as.matrix(model_train$U$`u:line_name`$value)
#  # Environments
#  Z_environments <- model.matrix(~ 0 + trial, pheno_data_test1)
#  u_environments <- as.matrix(model_train$U$`u:trial`$value)
#  # GxE
#  Z_gxe <- model.matrix(~ 0 + gxe, pheno_data_test1)
#  u_gxe <- as.matrix(model_train$U$`u:gxe`$value)
#  
#  # Generate predictions
#  pred_vals <- mu + (Z_genotypes %*% u_genotypes) + (Z_environments %*% u_environments) + (Z_gxe %*% u_gxe)
#  
#  # Add the predicted values to the data.frame
#  pheno_data_test1$predValues <- pred_vals[,1]
#  
#  # Measure the model performance by calculating the root mean squared error
#  # and correlation between predicted and observed phenotypes
#  library(dplyr)
#  pheno_data_test1 %>%
#    group_by(trial) %>%
#    summarize(accuracy = cor(value, predValues),
#              RMSE = sqrt(mean((value - predValues)^2)))
#  

