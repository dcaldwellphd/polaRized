#' @title polarize_distr
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
#' @importFrom dplyr select filter nest_by across mutate rename_with
#' @importFrom agrmt agreement polarization Leik consensus entropy BerryMielke BlairLacy Kvalseth lsquared dsquared MRQ

polarize_distr <- function(
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

  # For referencing values passed to value_1, value_2, and weights
  value <- substitute(value)
  weights <- substitute(weights)

  # Function to iterate over "by" groups,
  # calculating the association betwen values using syntax
  # compatible with functions related to the survey R package
  calc_distribution <- function(
    data,
    col = value
    ) {
    fmla <- as.formula(paste0("~", col))

    if (measure == "mean") {
      distr <- survey::svymean(
        fmla, design = data, na.rm = TRUE
        )
    } else if (
      measure == "median" | measure == "iqr"
      ) {
      distr <- survey::svyquantile(
        fmla, design = data, quantiles = c(
          0.25, 0.5, 0.75
          )
        )
    } else if (measure == "sd") {
      distr <- jtools::svysd(
        fmla, design = data, na.rm = TRUE
        )
    } else if (measure == "kurtosis") {
      distr <- svykurt(
        fmla, design = data, na.rm = TRUE
        )
    } else if (measure == "prop_extremism") {
      distr <- svyprop_extrmism(
        fmla, design = data, na.rm = TRUE
        )
    } else if (
      measure %in% c(
        "agreement", "polarization", "Leik", "consensus",
        "entropy", "BerryMielke", "BlairLacy", "Kvalseth",
        "lsquared", "dsquared", "MRQ"
        )
    ) {
      distr <- survey::svytable(
        fmla, design = data, round = ifelse(
          measure == "consensus", TRUE, FALSE
          )
        )
    } else {
      stop("Unrecognized measure argument")
    }

    return(distr)
  }

  input <- data |>
    select(
      {{ value }},
      any_of(by),
      {{ ids }},
      {{ probs }},
      {{ strata }},
      {{ fpc }},
      {{ weights }}
    ) |>
    # Filtering complete observations
    drop_na({{ value }})

  if (!is.null(weights)) {
    input <- input |>
      # Subsetting to weighted sample
      drop_na({{ weights }}) |>
      filter(.data[[weights]] != 0)
  }

  # Creating survey design objects nested by the "by" argument
  nested_distr <- input |>
    nest_by(across(any_of(by))) |>
    # Using tidytable for speed
    tidytable::mutate(
      design_list = tidytable::map(
        data,
        as_survey_design,
        ids = {{ ids }},
        probs = {{ probs }},
        strata = {{ strata }},
        fpc = {{ fpc }},
        nest = nest,
        check_strata = check_strata,
        weights = {{ weights }},
        pps = pps,
        variance = variance
      )
    ) |>
    # Looping through survey objects to calculate association
    tidytable::mutate(
      distr_list = tidytable::map(
        design_list,
        calc_distribution
      )
      ) |>
    select(-data, -design_list)

  if (measure == "median") {
    unnested_distr <- nested_distr |>
      mutate(
        quants = map(
        distr_list,
        `[[`,
        value
        ),
      value = map(quants, `[`, 2)
      ) |>
      select(-quants, -distr_list)
  } else if (measure == "iqr") {
    unnested_distr <- nested_distr |>
      mutate(
        quants = map(
          distr_list,
          `[[`,
          value
          ),
        q1 = as.numeric(map(quants, `[`, 1)),
        q3 = as.numeric(map(quants, `[`, 3)),
        value = q3 - q1
        ) |>
      select(
        -quants, -q1, -q3, -distr_list
        )
  } else if (
    measure %in% c(
      "mean", "sd", "kurtosis", "prop_extremism"
      )
    ) {
    unnested_distr <- nested_distr |>
      mutate(
        value = unlist(distr_list)
        ) |>
      select(-distr_list)
  } else {

    agrmt_lookup <- list(
      "agreement" = agrmt::agreement,
      "polarization" = agrmt::polarization,
      "Leik" = agrmt::Leik,
      "consensus" = agrmt::consensus,
      "entropy" = agrmt::entropy,
      "BerryMielke" = agrmt::BerryMielke,
      "BlairLacy" = agrmt::BlairLacy,
      "Kvalseth" = agrmt::Kvalseth,
      "lsquared" = agrmt::lsquared,
      "dsquared" = agrmt::dsquared,
      "MRQ" = agrmt::MRQ
    )
    agrmt_func <- agrmt_lookup[[measure]]

    unnested_distr <- nested_distr |>
      unnest_wider(col = distr_list) |>
      pivot_longer(
        cols = -c(any_of(by)),
        names_to = "response",
        values_to = "freq"
        ) |>
      drop_na(freq) |>
      summarise(
        value = agrmt_func(freq),
        .by = any_of(by)
      )
  }

  output <- dplyr::rename_with(
    unnested_distr,
    ~ paste0(
      value, "_", measure, recycle0 = TRUE
      ),
    value
    )

  return(output)

}
