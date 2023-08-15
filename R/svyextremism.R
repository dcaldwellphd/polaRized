#' @title svyextremism
#'
#' @description Calculates the proportion of extremism on ordered ratings scales in complex survey data
#'
#' @param formula A formula (e.g., ~var) specifying the variable on which to estimate extremism.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param method How to estimate confidence interval. See \code{\link[survey]{svyciprop}} for more details.
#' @param level Confidence level for interval.
#' @param df Denominator degrees of freedom. See \code{\link[survey]{svyciprop}} for more details.
#'
#' @details
#' This function wraps around the \code{svyciprop} function from the \code{survey} package. It is like calling \code{svyciprop(~I(variable == value), design)}, but \code{svyextremism} automatically classifies and measures the proportion of extreme values on ordered ratings scales with different lengths. If the scale has less than 10 (and more than 3) unique values, the function uses its minimum and maximum as extreme values. If the scale has 10 or 11 unique values, extreme values also include the second lowest and highest response categories. If the scale is a feeling thermometer with 100 or 101 unique response categories, the top 20 and bottom 20 unique values are classed as extreme. Calling \code{svyextremism} on any other scale length returns an error. The approach to measuring extremism implemented here is based on previous research into attitude polarization (Adams et al. 2012; Caldwell 2023; Cohen & Cohen 2021), and allows the \code{polarize_distr} function from this package to loop over heterogeneous rataing scales.
#'
#' @references
#'
#' Adams, J., Green, J., and Milazzo, C. (2012b). Has the British Public Depolarized Along With Political Elites? An American Perspective on British Public Opinion. \emph{Comparative Political Studies}, 45(4):507–530.
#'
#' Caldwell, D. (2023) \emph{Polarisation and Cultural Realignment in Britain, 2014-2019}. Doctoral thesis, Durham University.
#'
#' Cohen, G. and Cohen, S. (2021). Depolarization, Repolarization and Redistributive Ideological Change in Britain, 1983–2016. \emph{British Journal of Political Science}, 51(3):1181–1202.
#'
#' @seealso [`svyciprop()`][survey::svyciprop]
#'
#' @note This function was written without the knowledge or endorsement of the \code{survey} package authors.
#'
#' @return An object of class \code{svyciprop} giving the proportion and its confidence interval
#'
#' @examples
#' library(survey)
#' data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata_w, ids = ~1, weights = ~weight)
#' # Print the proportion of extremism on a variable without specifying extreme values
#' svyextremism(~att5val, design = toydesign)
#'
#' @export
#'
#' @importFrom survey svyciprop

svyextremism <- function(
    formula,
    design,
    na.rm = FALSE,
    method = "logit",
    level = 0.95,
    df = degf(design)
) {
  # design must be a survey design object
  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  # Storing variable in formula as string
  # This is necessary to construct the svyciprop formula below
  var_name <- as.character(formula)[2]

  x <- model.frame(formula, design$variables, na.action = na.pass)
  # Converting to matrix so that length(unique(x)) evaluates the
  # number of unique values in a variable rather than the number
  # of variables specified in the formula argument
  x <- as.matrix(x)
  # Limiting function to formulas involving one variable,
  # both because svyciprop itself takes only a single binary variable
  # and to avoid complications in counting the number of unique values
  # across multiple scales
  if (ncol(x) > 1)
    stop("Only calculate extremism one variable at a time")

  if(na.rm){
    x <- x[!is.na(x)]
  }
  # What is an extreme value in a scales with two or three response categories?
  if (length(unique(x)) < 4) {
    stop("Scale length too short for meaningfully extreme values")
  } else if (length(unique(x)) <= 9) {
    extremities <- c(
      max(x, na.rm = na.rm),
      min(x, na.rm = na.rm)
    )
    # Sometimes "10-point" scales include 0
  } else if (length(unique(x)) %in% c(10:11)) {
    extremities <- c(
      sort(unique(x), decreasing = TRUE)[1:2],
      sort(unique(x))[1:2]
    )
    # Sometimes feeling thermometers include 0
  } else if (length(unique(x)) %in% c(100:101)) {
    extremities <- c(
      sort(unique(x), decreasing = TRUE)[1:20],
      sort(unique(x))[1:20]
    )
  } else {
stop("Unusual scale length. The svvyextremism function is designed for ordered ratings scales, such as likert scales or feeling thermometers.")
  }

  # Cannot reference "extremities" directly in svyciprop formula without
  # it being mistaken for a variable that is missing from the survey design,
  # so we paste extreme values into a string and convert that to a formula
  fmla <- as.formula(paste0(
    "~I(",
    var_name,
    "%in% c(",
    paste(
      extremities,
      collapse = ","
      ),
    "))"
    )
  )

  prop_extremism <- svyciprop(
    fmla,
    design,
    method,
    level,
    df
    )

  return(prop_extremism)

}
