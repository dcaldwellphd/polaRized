#' @title svyextremism
#'
#' @description Calculates proportion of extremism with complex survey data
#'
#' @param x A formula (e.g., ~var1) specifying the variable on which to estimate extremism.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param extreme_vals Optional values to use in the proportion calculation. The default (NULL) attempts to classify extremism based on scale length.
#'
#' @return An object of class \code{svystat} giving the proportion and its variance
#'
#' @examples
#' if (requireNamespace("survey")) {
#'  library(survey)
#'  data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata, ids = ~1, weights = ~weight)
#' # Print the excess kurtosis of a variable
#' svyextremism(x = ~att_val, design = toydesign, na.rm = TRUE)
#'
#' @export
#'
#' @importFrom survey svymean



svyextremism <- function(
    x,
    design,
    na.rm = FALSE,
    extreme_vals = NULL
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  x <- model.frame(x, design$variables, na.action = na.pass)
  x <- as.matrix(x)

  if (ncol(x) > 1)
    stop("Only calculate extremism one variable at a time")

  if(na.rm){
    x <- x[!is.na(x)]
  }

  if (!is.null(extreme_vals)) {
    minmax <- extreme_vals
  } else if (length(unique(x)) < 4) {
    stop("Scale length too short for meaningfully extreme values")
  } else if (length(unique(x)) < 9) {
    minmax <- c(
      max(x, na.rm = TRUE),
      min(x, na.rm = TRUE)
    )
  } else if (length(unique(x)) < 20) {
    minmax <- c(
      sort(unique(x), decreasing = TRUE)[1:2],
      sort(unique(x))[1:2]
    )
  } else if (length(unique(x)) %in% c(100, 101)) {
    minmax <- c(
      sort(unique(x), decreasing = TRUE)[1:20],
      sort(unique(x))[1:20]
    )
  } else {
    stop("Unusual scale length: specify extreme values manually")
  }

  prop_extremism <- svymean(
    x %in% minmax,
    design,
    na.rm = na.rm
  )

  class(prop_extremism) <- "svystat"
  attr(prop_extremism, "statistic") <- "proportion"

  return(prop_extremism)

}
