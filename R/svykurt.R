#' @title svykurt
#'
#' @description Calculates kurtosis with complex survey data
#'
#' @param formula A formula (e.g., ~var1) specifying the variable on which to estimate kurtosis.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param excess Logical. The default (TRUE) subtracts 3 from the output, giving excess kurtosis.
#'
#' @details
#' This function extends the \code{survey} package by calculating the kurtosis of a variable in a complex survey design. It writes the variance and fourth central moment in terms of raw moments and then transforms into kurtosis via \code{svycontrast}. Setting \code{excess} to TRUE allows the output to be compared with 0, the excess kurtosis of any univariate normal distribution.
#'
#' @return An object of class \code{svrepstat} giving the kurtosis its standard error.
#'
#' @seealso \code{\link[survey]{svycontrast}}

#' @note The approach used in this function is based on guidance from Thomas Lumley, the \code{survey} package's author (see \href{https://stackoverflow.com/questions/76733872/using-svyrecvar-to-get-the-variance-of-a-statistic-in-the-survey-r-package}{here} and \href{https://stackoverflow.com/questions/76830298/using-svycontrast-inside-a-function-when-contrasts-involve-backticks-and-i}{here}).
#'
#' @examples
#' library(survey)
#' data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata, ids = ~1, weights = ~weight)
#' # Print the excess kurtosis of a variable
#' svykurt(formula = ~att_val, design = toydesign, na.rm = TRUE)
#'
#' @export
#'
#' @importFrom survey make.formula svymean svycontrast

svykurt <- function(
    formula,
    design,
    na.rm = FALSE,
    excess = TRUE
) {
  # design must be a survey design object
  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  # Storing variable in formula as string
  var_name <- as.character(formula[[2]])
  # Pasting var_name in strings representing moments of the distribution
  moments_char <- paste0("I(", var_name, "^", 1:4, ")")
  # Creating formula to calculate moments using svymean
  moments_formula <- make.formula(moments_char)
  moments <- svymean(moments_formula, design, na.rm = na.rm)

  # Storing expression of fourth central moment and variance in terms of raw moments, which allows us to work around the need to quote names that contain backticks in svycontrast
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

  # Now we can use svycontrast to get the kurtosis and its standard error
  kurt <- svycontrast(central_moments, quote(mu4 / (sigma2 * sigma2)))

  if (excess) {
    kurt <- kurt - 3
  }

  attr(kurt, "statistic") <- "kurtosis"
  attr(kurt, "names") <- var_name


  return(kurt)

}

