#' Batch check of predefined data quality conditions
#'
#' The following function checks for the presence of invalid combinations of variables within the
#' supplied data and produced and editable output that can be used for data quality review.
#'
#' @param df the dataframe you wish to check for invalid combinations of variables
#' @param conditions the set of conditions you wish to check
#' @param tests the set of test related to each condition
#' @param meta_to_keep a list of metadata you wish to include in the final output
#'
#' @return Standardized issues table for quality check in long format.
#'
#'
#' @author Alberto Gualtieri, \email{alberto.gualtieri@@reach-initiative.org}
#' @references \url{https://github.com/agualtieri/dataqualitycontrol}
#' @keywords yemen, mcla, quality check, platypus
#'
#'
#' @export


batch_issue_checks <- function(df, conditions, tests, meta_to_keep = c()){

  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.character(conditions))
  assertthat::assert_that(is.character(tests))
  assertthat::assert_that(is.vector(meta_to_keep))


  data_with_issues <- composr::recode_batch(df, tos = rep(1,length(conditions)),
                                            wheres = conditions,
                                            targets = tests) %>% composr::end_recoding


  unique_targets <- unique(tests)

  data_with_issues[, unique_targets] <- lapply(data_with_issues[, unique_targets], function(x){
    x[is.na(x)] <-0
    x }) %>% tibble::as_tibble

  data_with_issues %>% dplyr::select(c(meta_to_keep, unique_targets))

}

#' Run predefined quality check test on a raw dataset
#'
#' The following function checks for the presence of invalid data quality combinations within a dataframe
#' using the supplied condition_list.
#'
#' The condition_list must be a dataframe containingat least two variables: condition as the name of the condition one wishes to check
#' and test as the test applied to each condition.
#'
#'
#' @param df the dataset you wish to check for invalid combinations of variables
#' @param conditions_df the dataframe contaning the name of the checks and the list of controls to perform
#' @param condition.column the column name that containes the conditions you want to check in the dataset
#' @param test.name.column the column name that contains the description of the checks you want to perform
#' @param meta_to_keep a list of metadata you wish to include in the final output
#'
#' @return Standardized issues table for quality check in long format.
#'
#'
#' @author Alberto Gualtieri, \email{alberto.gualtieri@@reach-initiative.org}
#' @references \url{https://github.com/agualtieri/dataqualitycontrol}
#' @keywords yemen, mcla, quality check, platypus
#'
#'
#' @export


run_checks_from_dataframe<-function(df, conditions_df, condition.column, test.name.column, meta_to_keep = c()){

  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.data.frame(conditions_df))
  assertthat::assert_that(is.vector(condition.column))
  assertthat::assert_that(is.vector(test.name.column))
  assertthat::assert_that(is.vector(meta_to_keep))

  if(!(condition.column%in% names(conditions_df))){stop(paste(condition.column, " not a column name in conditions_df"))}
  if(!(test.name.column %in% names(conditions_df))){stop(paste(test.name.column, " not a column name in conditions_df"))}

  conditions <- conditions_df[[condition.column]]
  tests <- conditions_df[[test.name.column]]

  batch_issue_checks(df, conditions = conditions, tests = tests, meta_to_keep = meta_to_keep)

}

