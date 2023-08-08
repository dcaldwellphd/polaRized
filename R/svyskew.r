#' @title svyskew
#'
#' @description Calculates standardized skewness with complex survey data
#'
#' @param formula A formula (e.g., ~var1) specifying the variable on which to estimate skewness.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' 
#' @details
#' This function extends the \code{survey} package by calculating the skew of a variable in a complex survey design. It uses \code{svymean} to calculate the ..., and then transforms these into skewness using \code{svycontrast}.
#'
#' @return An object of class \code{svrepstat} giving the skew and its standard error.
#' 
#' @seealso [`svymean()`][survey::svymean], [`svycontrast()`][survey::svycontrast]

#' @note This function builds on code provided in the "Examples" section of the documentation for \code{survey::svycontrast}. It has been developed without the knowledge or endorsement of the \code{survey} package authors.
#'
#' @examples
#'  data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata_w, ids = ~1, weights = ~weight)
#' # Print the standardized skewness coefficient of a variable
#' svyskew(formula = ~att_5, design = toydesign)
#'
#' @export
#'
#' @importFrom survey svymean svycontrast

svyskew <- function(
    formula,
    design,
    na.rm = FALSE
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  

}

