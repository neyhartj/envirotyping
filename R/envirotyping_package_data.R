#' Example datasets for the envirotyping package
#'
#' @name barley_ex_data
#' @aliases Gmat env_data_merged hist_env_data_merged pheno_data_target pheno_data_test pheno_data_train trial_info trial_irrigation_data
#' @docType data
#'
#' @description
#' An example dataset used to demonstrate the functionality of the \code{envirotyping} package. It includes phenotypic data for model training and testing, metadata about the trials in which the phenotypic data were collected, and genomic information in the form of a relationship matrix between the barley lines that were phenotyped.
#'
#'
#' @format
#' A description of the objects within the dataset is below:
#'
#' \describe{
#'   \item{trial_info}{A \code{data.frame} of metadata for the phenotyping trials; includes the trial name, location name, year, environment name planting and harvest dates, geographic coordinates, and a categorical variable indicating whether the trial is meant for model training or testing.}
#'   \item{trial_irrigation_data}{A \code{data.frame} of date-specific irrigation information for some trials.}
#'   \item{pheno_data_train}{A \code{data.frame} of phenotypic observations meant for model training; data is available for 183 barley lines in 6 trials across 4 locations and 3 years and is stored in "long" format.}
#'   \item{pheno_data_test}{A \code{data.frame} of phenotypic observations meant for model testing; data is available for 48 barley lines in 2 trials across 2 locations and 2 years and is stored in "long" format.}
#'   \item{pheno_data_target}{A \code{data.frame} identical to \code{pheno_data_test}, except the phenotypic values are missing.}
#'   \item{Gmat}{A \code{matrix} of genomic relationship information for 222 barley lines.}
#'   \item{env_data_merged}{A \code{data.frame} with in-season environmental (weather and soil) data for the trials in \code{trial_info}; includes information for 56 environmental variables and is stored in "long" format.}
#'   \item{hist_env_data_merged}{A \code{data.frame} with historical environmental (weather and soil) data for each phenotypic trial \emph{location} for the years 2010-2014; includes information for 56 environmental variables and is stored in "long" format.}
#' }
#'
#'
NULL



#' Variable names from the Harmonic World Soil Database (HWSD)
#'
#' @format
#' A \code{data.frame} with 60 rows and 5 variables:
#'
#' \describe{
#'   \item{field}{The variable name as expressed in the HWSD}
#'   \item{type}{The type of variable, one of "general", "other", or "property"}
#'   \item{variable}{A variable nickname}
#'   \item{units}{The variable units}
#'   \item{unitsR}{The variable units formatted for use in an expression}
#' }
#'
"hwsd.variables"
