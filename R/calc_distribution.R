#' @title calc_distribution
#'
#' @description Calculates distributional measures of polarization
#'
#' @param data A data set object
#' @param value A column containing values on some variable or set of variables, such as attitude item responses.
#' @param measure A string specifying the measure of distributional polarization.
#' @param by A character vector of optional groups to nest observations by (e.g., survey wave, country, social group).
#' @param ids Variables specifying cluster ids from largest level to smallest level (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param probs Variables specifying cluster sampling probabilities.
#' @param strata Variables specifying strata.
#' @param fpc Variables specifying a finite population correct, see
#' \code{\link[survey]{svydesign}} for more details.
#' @param nest If \code{TRUE}, relabel cluster ids to enforce nesting within strata.
#' @param check_strata If \code{TRUE}, check that clusters are nested in strata.
#' @param weights Variables specifying weights (inverse of probability).
#' @param pps "brewer" to use Brewer's approximation for PPS sampling without replacement. "overton" to use Overton's approximation. An object of class HR to use the Hartley-Rao approximation. An object of class ppsmat to use the Horvitz-Thompson estimator.
#' @param variance For pps without replacement, use variance="YG" for the Yates-Grundy estimator instead of the Horvitz-Thompson estimator
#'
#' @return A data frame object containing the measure of association between two sets of values
#'
#' @examples
#'
#' @export
#'
#' @importFrom survey svymean
#' @importFrom jtools svysd
#' @importFrom tidyr drop_na
#' @importFrom tidytable mutate map
#' @importFrom srvyr as_survey_design
#' @importFrom tidyselect any_of
#' @importFrom dplyr select filter nest_by across mutate

calc_distribution <- function(
    data,
    value,
    measure,
    by = NULL,
    # Arguments to set in as_survey_design
    ids = NULL,
    probs = NULL,
    strata = NULL,
    fpc = NULL,
    nest = FALSE,
    check_strata = !nest,
    weights = NULL,
    pps = FALSE,
    variance = c("HT", "YG")
) {


  # Function to iterate over "by" groups,
  # calculating the association betwen values using syntax
  # compatible with functions related to the survey R package
  distribute_values <- function(data, col = value) {
    fmla <- as.formula(paste0("~", col))

    if (measure == "mean") {
      distr <- survey::svymean(fmla, design = data, na.rm = TRUE)
    } else if (measure == "sd") {
      distr <- jtools::svysd(fmla, design = data, na.rm = TRUE)
    } else {
      stop("Did you forget to set the 'measure' argument?")
    }

    return(distr)
  }

  input <- data |>
    select(
      {{ value }},
      {{ weights }},
      any_of(by)
    ) |>
    # Filtering pairwise complete observations
    drop_na({{ value }})

  if (!is.null(weights)) {
    input <- input |>
      # Subsetting to weighted sample
      drop_na({{ weights }}) |>
      filter(.data[[weights]] != 0)
  }



  return(output)

}
