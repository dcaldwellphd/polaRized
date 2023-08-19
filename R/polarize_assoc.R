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
#' @details
#' This function implements two prominent associational measures of polarization. It is designed around the \code{survey} package, allowing the incorporation of complex survey design features. It is useful when you want to calculate associational measures of polarization over a large number of attitude items. This was previously less convenient in the \code{survey} package, which requires the user to specify variables manually. Pass columns containing grouping information (such as variable names) to the \code{by} argument, and \code{polarize_assoc} will automatically nest the data and apply functions related to the \code{survey} package.
#'
#'  When \code{r_or_r2} is set to "r", \code{jtools::svycor} is used to calculate the Pearson correlation between \code{value_1} and \code{value_2}. The \code{svycor} function is essentially a wrapper around \code{survey::svyvar}. It calculates the variance-covariance matrix of variables supplied to the formula and extracts the correlation coefficient using \code{cov2cor} (see Long 2023).
#'
#' When \code{r_or_r2} is set to "r2", the function fits OLS regression models using \code{survey::svyglm}. The column supplied to \code{value_1} will be used as the dependent variable and the column supplied to \code{value_2} will be the independent variable. The direction of this relationship makes no difference in practice, however. The $R^2$ and adjusted $R^2$ are extracted from each model and returned as the output, giving the square of the correlation between observed and predicted outcomes. These statistics are similar to squaring the Pearson correlation between two variables, only the regression framework allows for the incorporation of categorical predictors, such as unordered party values (see Caldwell, Cohen, and Vivyan 2023).
#'
#' @references
#'
#' Caldwell, D.,  Cohen, C. and Vivyan, N. (2023). Long-Run Trends in Political Polarization of Climate Policy-Relevant Attitudes Across Countries. \emph{Working Paper}.
#'
#' Long, J. (2023). \emph{Calculate correlations and correlation tables with complex survey data}. Retrieved from \url{https://cran.r-project.org/web/packages/jtools/vignettes/svycor.html}
#'
#' @seealso [`svycor()`][jtools::svycor], [`svyglm()`][survey::svyglm]
#'
#' @return A data frame object containing the measure of association between \code{value_1} and \code{value_2} for any groups in \code{by}.
#'
#' @examples
#' data(toydata)
#' # Partisan polarization using Pearson correlation between attitude item and ordinal party affiliation variable
#' party_pol <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_ord, r_or_r2 = "r", by = c("att_name", "group"))
#'
#' # Partisan polarization using unordered party categories and no groups
#' multiparty_pol <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_cat, r_or_r2 = "r2")
#'
#' # Ideological polarization using the Pearson correlation between attitude pairs and no groups
#' paired_toydata <- spread_pairs(data = toydata, name_key = att_name, value_key = att_val, other_keys = c("id", "group"))
#' ideology_pol <- polarize_assoc(data = paired_toydata, value_1 = att_val1, value_2 = att_val2, r_or_r2 = "r")
#'
#' # Ideological polarization grouping R-squared statistics by attitude pair
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
#' @importFrom dplyr select filter nest_by across

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

  # For referencing values passed to value_1, value_2, and weights arguments
  value_1 <- substitute(value_1)
  value_1_eval <- eval(value_1, data)
  value_2 <- substitute(value_2)
  value_2_eval <- eval(value_2, data)
  weights <- substitute(weights)

  # Function to iterate over groups supplied to by argument,
  # calculating the association betwen values using syntax
  # compatible with functions related to the survey R package.
  calc_association <- function(
    data,
    col1 = value_1,
    col2 = value_2
    ) {

      if (r_or_r2 == "r") {
      # Syntax for the formula compatible with svycor
      fmla <- as.formula(paste0("~", col1, " + ", col2))
      assoc <- jtools::svycor(fmla, design = data)

      return(assoc)

      } else if (r_or_r2 == "r2") {
      # Syntax for the formula compatible with svyglm
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
  # Creating a separate survey design object for each group level
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
    # Looping through survey design objects to calculate association
    tidytable::mutate(
      assoc_list = tidytable::map(
        design_list,
        calc_association
      )
    )

  if (r_or_r2 == "r") {
    # svycor returns a 2*2 matrix named "cors"
    # Extracting a copy of the correlation between value_1 and value_2 from every cors object
    output <- tidytable::mutate(
      nested_assocs,
      r = tidytable::map(
        assoc_list,
        ~ .x$cors[2]
      )
    )
  } else if (r_or_r2 == "r2") {
    # Extracting the R-squared and adjusted R-squared from every summary.lm object
    output <- tidytable::mutate(
      nested_assocs,
      r2 = tidytable::map(
        assoc_list,
        `[[`,
        "r.squared"
      ),
      adj_r2 = tidytable::map(
        assoc_list,
        `[[`,
        "adj.r.squared"
      )
    )
  }
  # Removing nested data from output
  output <- select(
    output,
    -data, -design_list, -assoc_list
  )

  return(output)

}
