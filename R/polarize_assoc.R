#' @title polarize_assoc
#'
#' @description Calculates associational measures of polarization
#'
#' @param data A data set object
#' @param value_1 A column containing values on some variable or set of variables, such as attitude item responses.
#' @param value_2 A column containing values on a second variable or set of variables (e.g., political party affiliation).
#' @param r_or_r2 A string specifying the measure of association. See Details below.
#' @param by Optional groups to nest observations by (e.g., attitude item, survey wave, country).
#' @param ids Variables specifying cluster ids from largest level to smallest level (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param probs Variables specifying cluster sampling probabilities.
#' @param strata Variables specifying strata.
#' @param fpc Variables specifying a finite population correct, see
#' \code{\link[survey]{svydesign}} for more details.
#' @param weights Variables specifying weights (inverse of probability).
#' @param nest If \code{TRUE}, relabel cluster ids to enforce nesting within strata.
#'
#' @details
#' This function implements two prominent associational measures of polarization. It is designed around the \code{survey} package, allowing the incorporation of complex survey design features. It is useful when you want to calculate associational measures of polarization over a large number of attitude items. This was previously less convenient in the \code{survey} package, which requires the user to specify variables manually. Pass columns containing grouping information (such as variable names) to the \code{by} argument, and \code{polarize_assoc} will automatically nest the data and apply functions related to the \code{survey} package.
#'
#' When \code{r_or_r2} is set to "r", \code{polarize_assoc} transforms the underlying covariance matrix calculated by \code{survey::svyvar} into Pearson correlation coefficients using the \code{cov2cor} function. This approach is recommended by the \code{survey} package author, Thomas Lumley (see {https://stackoverflow.com/questions/34418822/pearson-correlation-coefficient-in-rs-survey-package#41031088}{this} discussion on Stack Overflow).
#'
#' When \code{r_or_r2} is set to "r2", \code{polarize_assoc} fits OLS regression models using \code{survey::svyglm}. The column supplied to \code{value_1} will be used as the dependent variable and the column supplied to \code{value_2} will be the independent variable. The direction of this relationship makes no difference in practice, however. The R-squared and adjusted R-squared are extracted from each model and returned as the output, giving the square of the correlation between observed and predicted outcomes. These statistics are similar to squaring the Pearson correlation between two variables, only the regression framework allows for the incorporation of categorical predictors, such as unordered party values (see Caldwell, Cohen, and Vivyan 2023).
#'
#' @references
#'
#' Caldwell, D.,  Cohen, C. and Vivyan, N. (2023). Long-Run Trends in Political Polarization of Climate Policy-Relevant Attitudes Across Countries. \emph{Working Paper}.
#'
#' @seealso \code{\link[survey]{svyvar}}, \code{\link[survey]{svyglm}}
#'
#' @return A data frame object containing the measure of association between \code{value_1} and \code{value_2} for any groups in \code{by}.
#'
#' @examples
#' data(toydata)
#' # Partisan polarization using Pearson correlation between attitude item and ordinal party affiliation variable
#' party_pol <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_ord, r_or_r2 = "r", by = c(att_name, group))
#'
#' # Partisan polarization using unordered party categories and no groups
#' multiparty_pol <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_cat, r_or_r2 = "r2")
#'
#' # Ideological polarization using the Pearson correlation between attitude pairs and no groups
#' paired_toydata <- spread_pairs(data = toydata, name_key = att_name, value_key = att_val, other_keys = c(id, group))
#' ideology_pol <- polarize_assoc(data = paired_toydata, value_1 = att_val1, value_2 = att_val2, r_or_r2 = "r")
#'
#' # Ideological polarization grouping R-squared statistics by attitude pair
#' ideology_pol2 <- polarize_assoc(data = paired_toydata, value_1 = att_val1, value_2 = att_val2, r_or_r2 = "r2", by = c(att_name1, att_name2))
#'
#' @export
#'
#' @importFrom survey svyglm svyvar
#' @importFrom tidyr drop_na
#' @importFrom srvyr as_survey_design
#' @importFrom tidyselect any_of
#' @importFrom dplyr select filter nest_by across mutate ungroup
#' @importFrom purrr pluck

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
    weights = NULL,
    nest = FALSE
    ) {

  # For referencing values passed to value_1, value_2, and weights arguments
  value_1 <- substitute(value_1)
  value_2 <- substitute(value_2)
  weights <- substitute(weights)

  if (r_or_r2 == "r" && 
      (!is.numeric(data[[deparse(value_1)]]) || 
       !is.numeric(data[[deparse(value_2)]]))) {
    stop("Cannot calculate Pearson correlation with non-numeric value columns.")
  }

  if (r_or_r2 == "r2" && !is.numeric(data[[deparse(value_1)]])) {
    stop("Cannot calculate OLS model(s) with non-numeric value_1 column.")
  }

  # Convert unquoted column names in 'by' to character strings
  by_sub <- substitute(by)
  if (is.null(by_sub) || is.symbol(by_sub)) {
    by <- deparse(by_sub)
    } else {
      by <- sapply(as.list(by_sub)[-1L], deparse)
      }

  # Function to iterate over groups supplied to by argument,
  # calculating the association betwen values using syntax
  # compatible with functions related to the survey R package.
  calc_association <- function(
    data,
    col1 = value_1,
    col2 = value_2
    ) {

      if (r_or_r2 == "r") {

      # Syntax for the formula compatible with svyvar
      fmla <- as.formula(paste0("~", col1, " + ", col2))
      var <- survey::svyvar(fmla, design = data)
      var <- as.matrix(var)
      assoc <- cov2cor(var)
      assoc <- assoc[1:nrow(assoc), 1:nrow(assoc)]

      return(assoc)

      } else if (r_or_r2 == "r2") {
        
      # Syntax for the formula compatible with svyglm
      fmla <- as.formula(paste0(col1, "~", col2))
      assoc <- survey::svyglm(fmla, design = data)
      return(summary.lm(assoc))

      } else {
      stop("The 'r_or_r2' argument is not set to one of these values.")
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
  # Subsetting to weighted sample
  if (!is.null(weights)) {
    input <- input |>
      drop_na({{ weights }}) |>
      filter(.data[[weights]] != 0)
  }
  # Nesting by groups supplied to by argument
  nested_data <- nest_by(input, across(any_of(by)))
  # Setting up survey design for each group
  design_list <- lapply(
    nested_data$data,
    function(nested_group) {
      survey_design <- as_survey_design(
        .data = nested_group,
        ids = {{ ids }},
        probs = {{ probs }},
        strata = {{ strata }},
        fpc = {{ fpc }},
        weights = {{ weights }},
        nest = nest
        )
      return(survey_design)
      }
    )
  nested_data$design_list <- design_list
  # Calculating associational value for each group
  assoc_list <- lapply(
    nested_data$design_list,
    function(survey_design) {
      assoc_val <- calc_association(
        survey_design
        )
        return(assoc_val)
    }
  )
  nested_data$assoc_list <- assoc_list
  # Discaring nested data and design lists
  nested_data <- select(
    nested_data,
    -data,
    -design_list
    )
  # calc_association returns a correlation matrix when
  # measure is set to "r", so we subset to one of the
  # off-diagonal values using pluck
  if (r_or_r2 == "r") {
    results <- nested_data |>
      mutate(
        r = pluck(
          assoc_list, 2
          )
        )
    } else if (r_or_r2 == "r2") {
      results <- nested_data |>
        mutate(
          r2 = pluck(
            assoc_list,
            "r.squared"
          ),
          adj_r2 = pluck(
            assoc_list,
            "adj.r.squared"
          )
        )
    }

  # Removing nested data from output
  output <- results |>
    select(-assoc_list) |>
    ungroup()

  return(output)

}
