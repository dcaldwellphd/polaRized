#' @title polarize_distr
#'
#' @description Calculates distributional measures of polarization
#'
#' @param data A data set object
#' @param value A column containing values on some variable or set of variables, such as attitude item responses.
#' @param measure A string specifying the measure of distributional polarization. See Details below. 
#' @param by A character vector of optional groups to nest observations by (e.g., survey wave, country, social group).
#' @param rescale_0_1 Logical. Setting to TRUE rescales values to range between 0 and 1.
#' @param ids Variables specifying cluster ids from largest level to smallest level (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param probs Variables specifying cluster sampling probabilities.
#' @param strata Variables specifying strata.
#' @param fpc Variables specifying a finite population correct, see
#' \code{\link[survey]{svydesign}} for more details.
#' @param nest If \code{TRUE}, relabel cluster ids to enforce nesting within strata.
#' @param check_strata If \code{TRUE}, check that clusters are nested in strata.
#' @param weights Variables specifying weights (inverse of probability).
#' @param pps "brewer" to use Brewer's approximation for PPS sampling without replacement. "overton" to use Overton's approximation. An object of class HR to use the Hartley-Rao approximation. An object of class ppsmat to use the Horvitz-Thompson estimator.
#' @param variance For pps without replacement, use variance="YG" for the Yates-Grundy estimator instead of the Horvitz-Thompson estimator.
#'
#' @return A data frame object containing the distributional measure of polarization.
#' 
#' @detals
#' This function is a one-stop-shop for distributional measures of polarization. It is designed around the \code{survey} package, allowing the incorporation of complex survey design features into the estimation of common distributional measures of polarization. It is useful when you want to summarise a large number of attitude distributions at once. This was previously less convenient, as the \code{survey} package requires the user to specify variables manually. Pass columns containing grouping information (such as variable names) to the \code{by} argument, and \code{polarize_distr} will automatically nest the data and apply various functions related to the \code{survey} package.
#' 
#' The following values are currently accepted by the \code{measure} argument:
#' \itemize{
#'  \item \code{"mean"}: Mean of the distribution, using \code{survey::svymean}
#'  \item \code{"median"}: Median of the distribution, using \code{survey::svyquantile}
#' \item \code{"iqr"}: Interquartile range of the distribution, using \code{survey::svyquantile}
#' \item \code{"sd"}: Standard deviation of the distribution, using \code{jtools::svysd}, which is a wrapper around \code{survey::svyvar}
#' \item \code{"kurtosis"}: Kurtosis of the distribution, using the \code{svykurt} function implemented in this package
#' \item \code{"extremism"}: Proportion of respondents who are extreme on the distribution, using the \code{svyextremism} function implemented in this package.
#' \item And these methods for estimating disagreement on ordered rating scales from the \code{agrmt} package, each of which are run on frequency vectors created using \code{survey::svytable}: \code{"agreement"}, \code{"polarization"}, \code{"Leik"}, \code{"consensus"}, \code{"entropy"}, \code{"BerryMielke"}, \code{"BlairLacy"}, \code{"Kvalseth"}, \code{"lsquared"}, \code{"dsquared"}, \code{"MRQ"}, \code{"concentration"}, \code{"dispersion"}, and \code{"Reardon"}. See \code{\link[agrmt]{agreement}} for more details.}
#' 
#' @return A data frame object containing the distributional measure applied to the \code{value} column.
#'
#' @examples
#' data(toydata)
#' # Filter attitude items with a length of unique values above 3
#' toydata_l <- filter_scale_length(toydata, scale_names = att_name, scale_values = att_val)
#' # Describing the central tendency of distributions surrounding attitude items
#' att_means <- polarize_distr(data = toydata_l, value = att_val, measure = "mean", by = c("att_name", "group", "time"), rescale_0_1 = TRUE)
#' att_medians <- polarize_distr(data = toydata_l, value = att_val, measure = "median", by = c("att_name", "group", "time"), rescale_0_1 = TRUE)
#' 
#' # Describing the dispersion of distributions surrounding attitude items
#' att_sd <- polarize_distr(data = toydata_l, value = att_val, measure = "sd", by = c("att_name", "group", "time"), rescale_0_1 = TRUE)
#' att_iqr <- polarize_distr(data = toydata_l, value = att_val, measure = "iqr", by = c("att_name", "group", "time"), rescale_0_1 = TRUE)
#' att_kurt <- polarize_distr(data = toydata_l, value = att_val, measure = "kurtosis", by = c("att_name", "group", "time"), rescale_0_1 = TRUE)
#' att_extremism <- polarize_distri(data = toydata_l, value = att_val, measure = "extremism", by = c("att_name", "group", "time"))
#' 
#' # Using measures of ordinal disgreement from the agrmt package
#' att_agreement <- polarize_distr(data = toydata_l, value = att_val, measure = "agreement", by = c("att_name", "group", "time"))
#' att_polarization <- polarize_distr(data = toydata_l, value = att_val, measure = "polarization", by = c("att_name", "group", "time"))
#' att_Leik <- polarize_distr(data = toydata_l, value = att_val, measure = "Leik", by = c("att_name", "group", "time"))
#' att_consensus <- polarize_distr(data = toydata_l, value = att_val, measure = "consensus", by = c("att_name", "group", "time"))
#' att_entropy <- polarize_distr(data = toydata_l, value = att_val, measure = "entropy", by = c("att_name", "group", "time"))
#' att_BerryMielke <- polarize_distr(data = toydata_l, value = att_val, measure = "BerryMielke", by = c("att_name", "group", "time"))
#' att_BlairLacy <- polarize_distr(data = toydata_l, value = att_val, measure = "BlairLacy", by = c("att_name", "group", "time"))
#' att_Kvalseth <- polarize_distr(data = toydata_l, value = att_val, measure = "Kvalseth", by = c("att_name", "group", "time"))
#' att_lsquared <- polarize_distr(data = toydata_l, value = att_val, measure = "lsquared", by = c("att_name", "group", "time"))
#' att_dsquared <- polarize_distr(data = toydata_l, value = att_val, measure = "dsquared", by = c("att_name", "group", "time"))
#' att_MRQ <- polarize_distr(data = toydata_l, value = att_val, measure = "MRQ", by = c("att_name", "group", "time"))
#' att_concentration <- polarize_distr(data = toydata_l, value = att_val, measure = "concentration", by = c("att_name", "group", "time"))
#' att_dispersion <- polarize_distr(data = toydata_l, value = att_val, measure = "dispersion", by = c("att_name", "group", "time"))
#' att_Reardon <- polarize_distr(data = toydata_l, value = att_val, measure = "Reardon", by = c("att_name", "group", "time"))
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
#' @importFrom agrmt agreement polarization Leik consensus entropy BerryMielke BlairLacy Kvalseth lsquared dsquared MRQ, concentration, dispersion, Reardon

polarize_distr <- function(
    data,
    value,
    measure,
    by = NULL,
    rescale_0_1 = FALSE,
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

  # Need a lookup table to flexibly use functions from the agrmt package
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
      "MRQ" = agrmt::MRQ,
      "concentration" = agrmt::concentration,
      "dispersion" = agrmt::dispersion,
      "Reardon" = agrmt::Reardon
    )

  # Function to iterate over "by" groups,
  # summarising the distribution of scales in the value column
  # using syntax compatible with functions related to the survey R package.
  calc_distribution <- function(
    data,
    col = value
    ) {
    fmla <- as.formula(paste0("~", col))

    if (measure == "mean") {
      distr <- survey::svymean(
        fmla, design = data
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
        fmla, design = data
        )
    } else if (measure == "kurtosis") {
      distr <- svykurt(
        fmla, design = data
        )
    } else if (measure == "extremism") {
      distr <- svyextremism(
        fmla, design = data
        )
    } else if (measure %in% names(agrmt_lookup)) {
      distr <- survey::svytable(
        fmla, design = data, round = ifelse(
          measure == "consensus", TRUE, FALSE
          )
        )
    } else {
      stop("Unrecognized measure argument.")
    }

    return(distr)
  }

  rescale_value <- function(x) {
    x_scaled <- x - min(x, na.rm = TRUE)
    x_scaled / max(x_scaled, na.rm = TRUE)
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
    drop_na({{ value }})

  if (rescale_0_1) {
    input <- input |>
      mutate(
        {{ value }} := rescale_value({{ value }}),
        .by = any_of(by)
      )
      }

  if (!is.null(weights)) {
    input <- input |>
      # Subsetting to weighted sample
      drop_na({{ weights }}) |>
      filter(.data[[weights]] != 0)
  }

  nested_distr <- input |>
    nest_by(across(any_of(by))) |>
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
    # Looping through survey objects to calculate distribution
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
      "mean", "sd", "kurtosis", "extremism"
      )
    ) {
    unnested_distr <- nested_distr |>
      mutate(
        value = unlist(distr_list)
        ) |>
      select(-distr_list)
  } else {
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
