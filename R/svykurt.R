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
#' @importFrom survey svymean svyvar


svykurt <- function(
    x,
    design,
    na.rm = FALSE,
    excess = TRUE
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  # Storing variable in formula as string
  # This is necessary to construct the svyciprop formula below
  var_name <- as.character(x)[2]

  x <- model.frame(x, design$variables, na.action = na.pass)
  x <- as.matrix(x)

  if (ncol(x) > 1)
    stop("Only calculate kurtosis one variable at a time")

  if(na.rm){
    x <- x[!is.na(x)]
  }

  momnts_fmla <- paste0(
    "~",
    var_name,
    " + I(",
    var_name,
    "^2) + I(",
    var_name,
    "^3) + I(",
    var_name,
    "^4)")

  momnts <- svymean(
    as.formula(momnts_fmla),
    design,
    na.rm = na.rm
    )



  mu4_fmla <- paste0(
    "mu4 = quote(-3 * ",
    var_name,
    "^4 + 6 * ",
    var_name,
    "^2 * `I(",
    var_name,
    "^2)` - 4 * ",
    var_name,
    " * `I(",
    var_name,
    "^3)` + `I(",
    var_name,
    "^4)`)"
  )

  sigma2_flma <- paste0(
    "sigma2 = quote(`I(",
    var_name,
    "^2)` - ",
    var_name,
    "^2)"
  )


  centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4_fmla,
      sigma2_flma
      )
    )

  return(centrl_momnts)

}

print.svykurt <- function(x) {
  m <- as.matrix(x, ncol = 1)
  rownames(m) <- names(x)
  colnames(m) <- "kurtosis"

  print(m)
}
