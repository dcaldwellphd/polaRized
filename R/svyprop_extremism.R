#' @title svyprop_extremism
#'
#' @description Calculates proportion of extremism in complex survey data
#'
#' @param x A formula (e.g., ~var1) specifying the variable on which to estimate extremism.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param extreme_vals Optional values to use in the proportion calculation. The default (NULL) attempts to classify extremism based on scale length.
#' @param method How to estimate confidence interval. See \code{\link[survey]{svyciprop}} for more details.
#' @param level Confidence level for interval
#' @param df Denominator degrees of freedom. See \code{\link[survey]{svyciprop}} for more details.
#'
#' @details
#' This function is a wrapper around \code{survey::svyciprop}. It is designed to estimate the proportion of extreme responses to likert scale or feeling thermometer items. Extreme values can be specified by the user, or else the function will attempt to classify these based on scale length. If the scale has less than 10 (and more than 3) unique values, the function uses the minimum and maximum as extreme values. If the scale has 10 or 11 unique values, extreme values also include the second lowest and highest response categories. If the scale is a feeling thermometer with 100 or 101 unique response categories, the top 20 and bottom 20 unique values are classed as extreme. This approach is based on previous research into attitude polarization (Adams et al. 2012; Caldwell 2023; Cohen & Cohen 2021).
#'
#' @references
#'
#' Adams, J., Green, J., and Milazzo, C. (2012b). Has the British Public Depolarized Along With Political Elites? An American Perspective on British Public Opinion. \emph{Comparative Political Studies}, 45(4):507–530.
#'
#' #' Caldwell, D. (2023) \emph{Polarisation and Cultural Realignment in Britain, 2014-2019}. Doctoral thesis, Durham University.
#'
#' Cohen, G. and Cohen, S. (2021). Depolarization, Repolarization and Redistributive Ideological Change in Britain, 1983–2016. \emph{British Journal of Political Science}, 51(3):1181–1202.
#'
#'
#' @return An object of class \code{svyciprop} giving the proportion and its confidence interval
#'
#' @examples
#' if (requireNamespace("survey")) {
#'  library(survey)
#'  data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata_w, ids = ~1, weights = ~weight)
#' # Print the proportion of extremism on a variable without specifying extreme values
#' svyprop_extrmism(x = ~att_5, design = toydesign)
#' # Print the proportion of extremism on a variable by specifying extreme values
#' svyprop_extrmism(x = ~att_5, design = toydesign, extreme_vals = c(1, 5))
#'
#' @export
#'
#' @importFrom survey svyciprop

svyprop_extrmism <- function(
    x,
    design,
    na.rm = FALSE,
    extreme_vals = NULL,
    method = "logit",
    level = 0.95,
    df = degf(design)
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  # Storing variable in formula as string
  # This is necessary to construct the svyciprop formula below
  var_name <- as.character(x)[2]

  x <- model.frame(x, design$variables, na.action = na.pass)
  x <- as.matrix(x)

  if (ncol(x) > 1)
    stop("Only calculate extremism one variable at a time")

  if(na.rm){
    x <- x[!is.na(x)]
  }

  if (!is.null(extreme_vals)) {
    extremities <- extreme_vals
  } else if (length(unique(x)) < 4) {
    stop("Scale length too short for meaningfully extreme values")
  } else if (length(unique(x)) < 9) {
    extremities <- c(
      max(x, na.rm = TRUE),
      min(x, na.rm = TRUE)
    )
    # Sometimes "10-point" scales include 0
  } else if (length(unique(x)) %in% c(10, 11)) {
    extremities <- c(
      sort(unique(x), decreasing = TRUE)[1:2],
      sort(unique(x))[1:2]
    )
    # Sometimes feeling thermometers include 0
  } else if (length(unique(x)) %in% c(100, 101)) {
    extremities <- c(
      sort(unique(x), decreasing = TRUE)[1:20],
      sort(unique(x))[1:20]
    )
  } else {
    stop("Unusual scale length: specify extreme values manually")
  }

  fmla <- paste0(
    "~I(",
    var_name,
    "%in% c(",
    paste(
      extremities,
      collapse = ","
      ),
    "))"
    )

  prop_extremism <- svyciprop(
    as.formula(fmla),
    design,
    method,
    level,
    df
    )

  return(prop_extremism)

}
