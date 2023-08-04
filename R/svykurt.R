#' @title svykurt
#'
#' @description Calculates Pearson kurtosis with complex survey data
#'
#' @param x A formula (e.g., ~var1) specifying the variable on which to estimate kurtosis.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param excess Logical. The default (TRUE) subtracts 3 from the output, giving excess kurtosis.
#'
#' @return An object of class \code{svykurt} giving the kurtosis on x
#'
#' @examples
#' if (requireNamespace("survey")) {
#'  library(survey)
#'  data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata_w, ids = ~1, weights = ~weight)
#' # Print the excess kurtosis of a variable
#' svykurt(x = ~att_5, design = toydesign)
#'
#' @export
#'
#' @importFrom survey svymean svycontrast

svykurt <- function(
    formula,
    design,
    na.rm = FALSE,
    excess = TRUE
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  var_name <- as.character(formula[[2]])
  moments_char <- paste0("I(", var_name, "^", 1:4, ")")
  moments_formula <- make.formula(moments_char)

  moments <- svymean(moments_formula, design, na.rm = na.rm)

  mu4expr <- substitute(
    -3 * one^4 + 6 * one^2 * two - 4 * one * three + four,
    list(
      one = as.name(moments_char[1]),
      two = as.name(moments_char[2]),
      three = as.name(moments_char[3]),
      four = as.name(moments_char[4])
    )
  )

  sigma2expr <- substitute(
    two - one^2,
    list(
      one = as.name(moments_char[1]),
      two=as.name(moments_char[2])
    )
  )

  central_moments <- svycontrast(
    moments,
    list(
      mu4 = mu4expr,
      sigma2 = sigma2expr
    )
  )

  kurt <- svycontrast(central_moments, quote(mu4 / (sigma2 * sigma2)))

  if (excess) {
    kurt <- kurt - 3
  }

  attr(kurt, "statistic") <- "kurtosis"
  attr(kurt, "names") <- var_name


  return(kurt)

}

