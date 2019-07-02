#' Batch check of predefined data quality conditions
#'
#' The following function checks for the presence of invalid combinations of variables within the
#' supplied data and produced and editable output that can be used for data quality review.
#'
#' @param data the dataset you wish to check for invalid combinations of variables
#' @param condition the set of condition you wish to check
#' @param tests the set of test related to each condition
#' @param meta_to_keep a list of metadata you wish to include in the final output
#'
#' @return Standardized issues table for quality check in long format.
#'
#'
#' @author Alberto Gualtieri, \email{alberto.gualtieri@@reach-initiative.org}
#' @references \url{https://github.com/agualtieri/mcla_cleaneR}
#' @keywords yemen, mcla, quality check, platypus
#'
#'
#' @examples
#' batch_issue_checks()

#' @export


batch_issue_checks <- function(data, conditions, tests, meta_to_keep = c()){

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.character(conditions))
  assertthat::assert_that(is.character(tests))
  assertthat::assert_that(is.vector(meta_to_keep))


  data_with_issues <- data %>% recode_batch(tos = rep(1,length(conditions)),
                                            wheres = conditions,
                                            targets = tests) %>% end_recoding


  unique_targets <- unique(tests)

  data_with_issues[,unique_targets] <- lapply(data_with_issues[,unique_targets], function(x){
    x[is.na(x)] <-0
    x }) %>% as_tibble

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
#' @param data the dataset you wish to check for invalid combinations of variables
#' @param condition_dataframe the dataframe contaning the name of the checks and the list of controls to perform
#' @param condition.column the column name that containes the conditions you want to check in the dataset
#' @param test.name.column the column name that contains the description of the checks you want to perform
#' @param meta_to_keep a list of metadata you wish to include in the final output
#'
#' @return Standardized issues table for quality check in long format.
#'
#'
#' @author Alberto Gualtieri, \email{alberto.gualtieri@@reach-initiative.org}
#' @references \url{https://github.com/agualtieri/mcla_cleaneR}
#' @keywords yemen, mcla, quality check, platypus
#'
#'
#' @examples
#' run_checks_from_dataframe()
#'
#'
#' @export


run_checks_from_dataframe<-function(data, conditions_dataframe, condition.column, test.name.column, meta_to_keep = c()){

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.data.frame(conditions_dataframe))
  assertthat::assert_that(is.vector(condition.column))
  assertthat::assert_that(is.vector(test.name.column))
  assertthat::assert_that(is.vector(meta_to_keep))

  if(!(condition.column%in% names(conditions_dataframe))){stop(paste(condition.column, " not a column name in conditions_dataframe"))}
  if(!(test.name.column %in% names(conditions_dataframe))){stop(paste(test.name.column, " not a column name in conditions_dataframe"))}

  conditions <- conditions_dataframe[[condition.column]]
  tests <- conditions_dataframe[[test.name.column]]
  batch_issue_checks(data,conditions = conditions,tests = tests,meta_to_keep = meta_to_keep)

}

