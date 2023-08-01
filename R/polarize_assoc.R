#' @title polarize_assoc
#'
#' @description Calculates associational measures of polarization
#'
#' @param data A data set object
#' @param value_1 A column containing values on some variable or set of variables, such as attitude item responses.
#' @param value_2 A column containing values on a second variable or set of variables (e.g., political party affiliation).
#' @param r_or_r2 A string specifying the measure of association. See Details below.
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
#' @param variance For pps without replacement, use variance="YG" for the Yates-Grundy estimator instead of the Horvitz-Thompson estimator.
#'
#' @return A data frame object containing the measure of association between two sets of values.
#' 
#' @details 
#' This function implements two prominent approaches to estimating polarization as the association between two sets of values, such as attitudes and partisanship. In a mostly US-dominated literature, the established approach is to use the Pearson correlation between an ordinal or binary attitude item and an ordinal measure of partisanship (e.g., Baldassarri and Gelman 2008). This approach is implemented by setting the \code{r_or_r2} argument to "r", returing the Pearson correlation between two value columns of class \code{numeric}.
#' 
#' The second approach is to use the $R^2$ or adjusted $R^2$ from OLS model predicting values supplied to the \code{value_1} argument from values supplied to the \code{value_2} argument, including categorical predictors (such as unordered multiparty values). This approach is implemented by setting the \code{r_or_r2} argument to "r2", returning the $R^2$ and adjusted $R^2$ corresponding to square of the correlation between observed and predicted outcomes (see Caldwell, Cohen, and Vivyan 2023).
#' 
#' The function is useful when you want to calculate associational measures of polarization over multiple variables or groups, such as attitude items nested in survey waves. It is designed to work with the \code{survey} package, allowing the incorporation of complex survey design features. This was previously inconvenient when working with large numbers of attitude items simultaneously, as the \code{survey} package requires the user to specify the variable name in a formula. The function allows the user to specify variable names and any other grouping information in the \code{by} argument, which are used to nest attitude item responses and apply functions related to the \code{survey} package.
#' 
#' In the case where \code{r_or_r2} is set to "r2", the function will automatically fit an OLS regression model of class \code{svyglm} (see \code{\link[survey]{svyglm}} for more details). Values supplied to the \code{value_1} argument will be used as the dependent variable, and values supplied to the \code{value_2} argument will be used as the independent variable. The function will then extract the $R^2$ and adjusted $R^2$ from each model and return them in the output data frame.
#' 
#' When \code{r_or_r2} is set to "r", the function uses \code{jtools::svycor} to calculate the Pearson correlation between \code{value_1} and \code{value_2}. \code{jtools::svycor} is a wrapper around 
#' 
#' 
#'
#' @examples
#' data(toydata)
#' # Partisan polarization using Pearson correlation between attitude item and ordinal party affiliation variable
#' party_pol <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_val, r_or_r2 = "r", by = c("att_name", "group", "year"))
#'
#' # Partisan polarization using unordered party categories and no groups
#' multiparty_pol <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_val, r_or_r2 = "r2")
#'
#' # Ideological polarization using the Pearson correlation between attitude pairs and no groups
#' paired_toydata <- spread_pairs(data = toydata, name_key = att_name, value_key = att_val, other_keys = other_keys = c("id", "group", "year"))
#' ideology_pol <- polarize_assoc(data = paired_toydata, value_1 = att_val1, value_2 = att_val2, r_or_r2 = "r")
#'
#' # Ideological polarization grouping the coefficient of determination by attitude pair
#' ideology_pol2 <- polarize_assoc(data = paired_toydata, value_1 = att_val1, value_2 = att_val2, r_or_r2 = "r2", by = c("att_name1", "att_name2"))
#'
#' @export
#'
#' @importFrom survey svyglm
#' @importFrom jtools svycor
#' @importFrom tidyr drop_na
#' @importFrom tidytable mutate map
#' @importFrom srvyr as_survey_design
#' @importFrom tidyselect any_of
#' @importFrom dplyr select filter nest_by across mutate

polarize_assoc <- function(
    data,
    value_1,
    value_2,
    r_or_r2,
    by = NULL,
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

  # For referencing values passed to value_1, value_2, and weights
  value_1 <- substitute(value_1)
  value_1_eval <- eval(value_1, data)
  value_2 <- substitute(value_2)
  value_2_eval <- eval(value_2, data)
  weights <- substitute(weights)

  # Function to iterate over "by" groups,
  # calculating the association betwen values using syntax
  # compatible with functions related to the survey R package
  calc_association <- function(
    data,
    col1 = value_1,
    col2 = value_2
    ) {
    if (r_or_r2 == "r") {
      if (
        !is.numeric(value_1_eval) | !is.numeric(value_2_eval)
        ) {
        stop(
          "Pearson correlations require two value columns of class numeric."
        )
      }

      fmla <- as.formula(paste0("~", col1, " + ", col2))
      assoc <- jtools::svycor(fmla, design = data)

      return(assoc)

      } else if (r_or_r2 == "r2") {

      fmla <- as.formula(paste0(col1, "~", col2))
      assoc <- survey::svyglm(fmla, design = data)
      return(summary.lm(assoc))

      } else {
      stop("Did you forget to set the 'r_or_r2' argument to one of these values?")
      }

    }

  input <- data |>
    select(
      {{ value_1 }},
      {{ value_2 }},
      any_of(by),
      {{ ids }},
      {{ probs }},
      {{ strata }},
      {{ fpc }},
      {{ weights }}
    ) |>
    # Filtering pairwise complete observations
    drop_na(
      {{ value_1 }}, {{ value_2 }}
    )

  if (!is.null(weights)) {
    input <- input |>
      # Subsetting to weighted sample
      drop_na({{ weights }}) |>
      filter(.data[[weights]] != 0)
  }

  nested_assocs <- input |>
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
    # Looping through survey objects to calculate association
    tidytable::mutate(
      assoc_list = tidytable::map(
        design_list,
        calc_association
      )
    )

  if (r_or_r2 == "r") {
    # Extracting the informative Pearson correlation coefficient from 2*2 matrices
    output <- mutate(
      nested_assocs,
      r = map(
        assoc_list,
        ~ .x$cors[2]
      )
    )
  } else if (r_or_r2 == "r2") {
    # Extracting coefficient of determination and
    # adjusted R-squared from every summary.lm object.
    output <- mutate(
      nested_assocs,
      r2 = map(
        assoc_list,
        `[[`,
        "r.squared"
      ),
      adj_r2 = map(
        assoc_list,
        `[[`,
        "adj.r.squared"
      )
    )
  }
  
  output <- select(
    output,
    -data, -design_list, -assoc_list
  )

  return(output)

}
