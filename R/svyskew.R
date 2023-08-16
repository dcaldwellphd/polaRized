#' @title svyskew
#'
#' @description Calculates standardized skewness with complex survey data
#'
#' @param formula A formula (e.g., ~var1) specifying the variable on which to estimate skewness.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#'
#' @details
#' This function extends the \code{survey} package by calculating the skewness of a variable in a complex survey design. It writes skewness in terms of raw moments and transforms via \code{svycontrast}.
#'
#' @return An object of class \code{svrepstat} giving the skewness and its standard error.
#'
#' @seealso [`svycontrast()`][survey::svycontrast]

#' @note This function generalizes the approach to estimating skewness provided in the "Examples" section of the documentation for \code{survey::svycontrast}. It has been developed without the knowledge or endorsement of the \code{survey} package authors.
#'
#' @examples
#' library(survey)
#' data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata, ids = ~1, weights = ~weight)
#' # Print the standardized skewness coefficient of a variable
#' svyskew(formula = ~att_val, design = toydesign, na.rm = TRUE)
#'
#' @export
#'
#' @importFrom survey svymean svycontrast

svyskew <- function(
    formula,
    design,
    na.rm = FALSE
) {
  # design must be a survey design object
  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  # Storing variable in formula as string
  var_name <- as.character(formula[[2]])
  # Pasting var_name in strings representing moments of the distribution
  moments_char <- paste0("I(", var_name, "^", 1:3, ")")
  # Creating formula to calculate moments using svymean
  moments_formula <- make.formula(moments_char)
  moments <- svymean(moments_formula, design, na.rm = na.rm)

  # Storing expression of skewness in terms of raw moments, which allows us to work around the need to quote names that contain backticks in svycontrast
  skewexpr <- substitute(
    (three - 3 * two * one + 3 * one * one^2 - one^3) / (two - one^2)^1.5,
    list(
      one = as.name(moments_char[1]),
      two = as.name(moments_char[2]),
      three = as.name(moments_char[3])
    )
  )

  skewness <- svycontrast(
    moments,
    skewexpr
  )

  attr(skewness, "statistic") <- "skewness"
  attr(skewness, "names") <- var_name

  return(skewness)

}
