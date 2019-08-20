#' dataqualitycontrol: A package to produce standardized data quality output
#'
#' The data quality control package provides four functions:
#' - batch quality checks
#' - quality log to long format
#' - dataframe anonymiser
#' - separate on multiple separators
#'
#' @section Batch quality checks function
#' The "Batch quality checks" function funs the list of quality checks the user provides through the dataframe that needs to be reviewed
#' and produces a standardized output as a .cvs document in wide format that can be used to have quick overview of which errors are the most
#' frequent. The wide format allows for better integration with PivotTables as well as dashboard-design tools such as Tableau and PowerBi.
#' The final output will contains all the metadata required by the use and a number of columns equal to the number of data quality checks provided
#' to the script. Each column will be filled with a 1 if the check returns an issue and 0 otherwise. Each row will be a specific household/KI uuid.
#' Multiple checks could be applied to one specific uuid.
#'
#' To produce the output table the user needs firstly to create the list of quality checks using an excel document that can be imported in R.
#' This document requires only two columns: "quality check name" and "quality check conditions". "Quality check name" is the name of the quality
#' check the user wishes to control within the dataset. It can be assigned by the user without restriction. "Quality check conditions" are the
#' specific set of conditions within and/or between variables the script will control for.
#'
#'
#' @section Quality log to long format
#' The "Quality log to long format" is a helper script that takes the quality log output create using the "batch quality checks" function
#' and transforms it into long format. The new format will keep the set of the requested metadata but each row will represent the specific variable
#' containing the quality issue, hence, it could apply to the same uuid according to the number of quality issues present.
#' The long format, although larger and less easily readable, will allow partners or the person responsible for data cleaning th ability to select
#' which of the variable in question needs to be corrected.
#'
#'
#' @docType package
#' @name dataqualitycontrol

NULL
