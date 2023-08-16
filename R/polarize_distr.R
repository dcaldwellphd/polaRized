#' @title polarize_distr
#'
#' @description Calculates distributional measures of polarization
#'
#' @param data A data set object
#' @param value A column containing values on some variable or set of variables, such as attitude item responses.
#' @param measure A string specifying the measure of distributional polarization. See Details below.
#' @param by A character vector of optional groups to nest observations by (e.g., survey wave, country, social group).
#' @param rescale_0_1 Logical. Setting to TRUE normalizes values prior to summarizing their distribution.
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
#' @details
#' This function is a one-stop-shop for distributional measures of polarization. It is designed around the \code{survey} package, allowing for the incorporation of complex survey design features. It is useful when you want to summarise a large number of attitude distributions at once. This was previously less convenient, as the \code{survey} package requires the user to specify variables manually. Pass columns containing grouping information (such as variable names) to the \code{by} argument, and \code{polarize_distr} will automatically nest the data and apply functions related to the \code{survey} package.
#'
#' The latter is set by the \code{measure} argument, which currently accepts the following values:
#' \itemize{
#'  \item \code{"mean"}: Mean of the distribution, using \code{survey::svymean}
#'  \item \code{"median"}: Median of the distribution, using \code{survey::svyquantile}
#' \item \code{"iqr"}: Interquartile range of the distribution, using \code{survey::svyquantile}
#' \item \code{"var"}: Variance of the distribution, using \code{survey::svyvar}
#' \item \code{"std"}: Standard deviation of the distribution, using \code{jtools::svysd}, which is a wrapper around \code{survey::svyvar}
#' \item \code{"kurt"}: Kurtosis of the distribution, using the \code{svykurt} function implemented in this package
#' \item \code{"skew"}: Skewness of the distribution, using the \code{svyskew} function implemented in this package
#' \item \code{"extremism"}: Proportion of respondents who are extreme on the distribution, using the \code{svyextremism} function implemented in this package.
#' \item And these methods for estimating consensus and disagreement on ordered rating scales from the \code{agrmt} package, each of which are run on frequency vectors created using \code{survey::svytable}: \code{"agreement"}, \code{"polarization"}, \code{"Leik"}, \code{"consensus"}, \code{"entropy"}, \code{"BerryMielke"}, \code{"BlairLacy"}, \code{"Kvalseth"}, \code{"lsquared"}, \code{"dsquared"}, \code{"MRQ"}, \code{"concentration"}, \code{"dispersion"}, and \code{"Reardon"}. See \code{\link[agrmt]{agreement}} for more details.}
#'
#' @seealso [`svymean()`][survey::svymean], [`svyquantile()`][survey::svyquantile], [`svyvar()`][survey::svyvar], [`svysd()`][jtools::svysd], [`svykurt()`][polaRized::svykurt], [`svyskew()`][polaRized::svyskew], [`svyextremism()`][polaRized::svyextremism], [`svytable()`][survey::svytable], [`agreement()`][agrmt::agreement], [`polarization()`][agrmt::polarization], [`Leik()`][agrmt::Leik], [`consensus()`][agrmt::consensus], [`entropy()`][agrmt::entropy], [`BerryMielke()`][agrmt::BerryMielke], [`BlairLacy()`][agrmt::BlairLacy], [`Kvalseth()`][agrmt::Kvalseth], [`lsquared()`][agrmt::lsquared], [`dsquared()`][agrmt::dsquared], [`MRQ()`][agrmt::MRQ], [`concentration()`][agrmt::concentration], [`dispersion()`][agrmt::dispersion], [`Reardon()`][agrmt::Reardon]
#'
#' @return A data frame object containing the distributional measure applied to \code{value} for any groups in \code{by}.
#'
#' @examples
#' data(toydata)
#' # Filter attitude items with a length of unique values above 3
#' filtered_toydata <- filter_scale_length(toydata, scale_names = att_name, scale_values = att_val)
#'
#' # Describing the central tendency of distributions surrounding attitude items
#' att_means <- polarize_distr(data = filtered_toydata, value = att_val, measure = "mean", by = c("att_name", "group"), rescale_0_1 = TRUE)
#' att_medians <- polarize_distr(data = filtered_toydata, value = att_val, measure = "median", by = c("att_name", "group"), rescale_0_1 = TRUE)
#'
#' # Describing the dispersion of distributions surrounding attitude items
#' att_var <- polarize_distr(data = filtered_toydata, value = att_val, measure = "var", by = c("att_name", "group"), rescale_0_1 = TRUE)
#' att_std <- polarize_distr(data = filtered_toydata, value = att_val, measure = "std", by = c("att_name", "group"), rescale_0_1 = TRUE)
#' att_iqr <- polarize_distr(data = filtered_toydata, value = att_val, measure = "iqr", by = c("att_name", "group"), rescale_0_1 = TRUE)
#' att_kurt <- polarize_distr(data = filtered_toydata, value = att_val, measure = "kurt", by = c("att_name", "group"))
#' att_skew <- polarize_distr(data = filtered_toydata, value = att_val, measure = "skew", by = c("att_name", "group"))
#' att_extremism <- polarize_distri(data = filtered_toydata, value = att_val, measure = "extremism", by = c("att_name", "group"))
#'
#' # Using measures of ordinal disgreement from the agrmt package
#' att_agreement <- polarize_distr(data = filtered_toydata, value = att_val, measure = "agreement", by = c("att_name", "group"))
#' att_polarization <- polarize_distr(data = filtered_toydata, value = att_val, measure = "polarization", by = c("att_name", "group"))
#' att_Leik <- polarize_distr(data = filtered_toydata, value = att_val, measure = "Leik", by = c("att_name", "group"))
#' att_consensus <- polarize_distr(data = filtered_toydata, value = att_val, measure = "consensus", by = c("att_name", "group"))
#' att_entropy <- polarize_distr(data = filtered_toydata, value = att_val, measure = "entropy", by = c("att_name", "group"))
#' att_BerryMielke <- polarize_distr(data = filtered_toydata, value = att_val, measure = "BerryMielke", by = c("att_name", "group"))
#' att_BlairLacy <- polarize_distr(data = filtered_toydata, value = att_val, measure = "BlairLacy", by = c("att_name", "group"))
#' att_Kvalseth <- polarize_distr(data = filtered_toydata, value = att_val, measure = "Kvalseth", by = c("att_name", "group"))
#' att_lsquared <- polarize_distr(data = filtered_toydata, value = att_val, measure = "lsquared", by = c("att_name", "group"))
#' att_dsquared <- polarize_distr(data = filtered_toydata, value = att_val, measure = "dsquared", by = c("att_name", "group"))
#' att_MRQ <- polarize_distr(data = filtered_toydata, value = att_val, measure = "MRQ", by = c("att_name", "group"))
#' att_concentration <- polarize_distr(data = filtered_toydata, value = att_val, measure = "concentration", by = c("att_name", "group"))
#' att_dispersion <- polarize_distr(data = filtered_toydata, value = att_val, measure = "dispersion", by = c("att_name", "group"))
#' att_Reardon <- polarize_distr(data = filtered_toydata, value = att_val, measure = "Reardon", by = c("att_name", "group"))
#'
#' @export
#'
#' @importFrom survey svymean svyvar
#' @importFrom jtools svysd
#' @importFrom tidyr drop_na
#' @importFrom tidytable mutate map
#' @importFrom srvyr as_survey_design
#' @importFrom tidyselect any_of
#' @importFrom dplyr select filter nest_by across rename_with
#' @importFrom agrmt agreement polarization Leik consensus entropy BerryMielke BlairLacy Kvalseth lsquared dsquared MRQ concentration dispersion Reardon

polarize_distr <- function(
    data,
    value,
    measure,
    by = NULL,
    rescale_0_1 = FALSE,
    # Arguments to set up survey design using as_survey_design from the srvyr package
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

  # For referencing values passed to value and weights arguments
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

  # Function to iterate over groups supplied to by argument,
  # summarising the distribution of values using syntax
  # compatible with functions related to the survey R package.
  calc_distribution <- function(
    data,
    col = value
    ) {
    fmla <- as.formula(paste0("~", col))

    if (measure == "mean") {
      distr <- survey::svymean(
        fmla, design = data
        )
    # The median,  first quartile and third quartile are
    # calculated together and filtered out later,
    # depending on whether measure is "median" or "iqr"
    } else if (
      measure == "median" | measure == "iqr"
      ) {
      distr <- survey::svyquantile(
        fmla, design = data, quantiles = c(
          0.25, 0.5, 0.75
          )
        )
    } else if (measure == "var") {
      distr <- survey::svyvar(
        fmla, design = data
        )
    } else if (measure == "std") {
      distr <- jtools::svysd(
        fmla, design = data
        )
    } else if (measure == "kurt") {
      distr <- svykurt(
        fmla, design = data
        )
    } else if (measure == "skew") {
      distr <- svyskew(
        fmla, design = data
        )
    } else if (measure == "extremism") {
      distr <- svyextremism(
        fmla, design = data
        )
    # Supported functions from the agrmt package are
    # stored in the lookup table above.
    } else if (measure %in% names(agrmt_lookup)) {
      # The consensus measure from that package requires
      # a frequency vector of integers, whereas other
      # measures can handle mixed numbers
      distr <- survey::svytable(
        fmla, design = data, round = ifelse(
          measure == "consensus", TRUE, FALSE
          )
        )
    } else {
      stop("Unrecognized measure argument!")
    }

    return(distr)
  }
  # Function to normalize value
  # if rescale_0_1 is set to TRUE
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
    # Filtering complete cases
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
  # Creating a separate survey design object for each group level
  nested_distr <- input |>
    nest_by(across(any_of(by))) |>
    mutate(
      design_list = map(
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
    # Looping through survey design objects to calculate distribution
    mutate(
      distr_list = map(
        design_list,
        calc_distribution
      )
      ) |>
    # Removing nested data
    select(-data, -design_list)

  if (measure == "median") {
    # For the median, we subset the second quartile
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
    # For the interquartile range, we subset the first and third quartiles
    # and calculate the difference
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
  # These measures are already calculated as a single value,
  # so we can easily unlist them.
  } else if (
    measure %in% c(
      "mean", "var", "std",
      "kurt", "skew", "extremism"
      )
    ) {
    unnested_distr <- nested_distr |>
      mutate(
        value = unlist(distr_list)
        ) |>
      select(-distr_list)
  } else {
    # All measures from the agrmt package take a frequency vector as input.
    # I unnest into n columns, where n is equal to the number of unique values
    # in the longest scale. This results in NAs for group levels involving shorter
    # scale lengths. These can be dropped after gathering frequency vectors into a
    # single column, which is then used to summarize the measure by group.
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
  # The resulting value column has the name of the input
  # and the measure called, separated by an underscore.
  output <- dplyr::rename_with(
    unnested_distr,
    ~ paste0(
      value, "_", measure, recycle0 = TRUE
      ),
    value
    )

  return(output)

}
