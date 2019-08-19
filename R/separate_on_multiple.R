#' Function to separate a string variable into to columns using multiple separators
#'
#' The following function helps you in separating a variable into two column using up to three separators
#'
#' @param data the dataset containing the variable that needs to be separated
#' @param var_to_separate the variable that needs to be separated
#' @param sep1 the first separator included in the variable that needs to be separated
#' @param sep2 the second separator included in the variable that needs to be separated (default is NULL)
#' @param sep3 the third separator included in the variable that needs to be separated (defaul is NULL)
#'
#' @return the @param var_to_separate will be divided into two column based on the included separators
#'
#'
#' @author Alberto Gualtieri, \email{alberto.gualtieri@@reach-initiative.org}
#' @references \url{https://github.com/agualtieri/dataqualitycontrol}
#' @keywords yemen, mcla, quality check, platypus
#'
#' @export



separate_on_multiple <- function(data, var_to_separate, sep1, sep2 = NULL, sep3 = NULL){

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.character(var_to_separate))
  assertthat::assert_that(is.character(sep1))



  x <- splitstackshape::cSplit(data, var_to_separate, sep = sep1, "long")

  if (!is.null(sep2)) {

    y <- splitstackshape::cSplit(x, var_to_separate, sep = sep2, "long")

  } else if (!is.null(sep3)) {

    z <- splitstackshape::cSplit(y, var_to_separate, sep = sep3, "long")
    z

  }


}
