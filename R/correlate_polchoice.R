#' @title correlate_polchoice
#'
#' @description Estimates the association between attitudes and partisanship
#'
#' @param data A data set object
#' @param attitude_col A column containing attitude item responses
#' @param party_col A column containing party affiliation responses
#' @param r_or_r2 A string specifying the measure of association. Use "r" to estimate the Pearson correlation between attitudes and an ordinal or dummy variable measure of partisanship. The value "r2" returns the coefficient of determination and adjusted R-squared from OLS models predicting attitudes from indicators of party support, which is useful for estimating partisan polarization in multiparty systems.
#' @param by A character vector of optional groups to nest observations by (e.g. survey wave, country, social group)
#' @param weight_col An optional column of survey weights

#'
#' @return A data frame object containing the measure of association between attitudes and partisanship

#' @examples
#' data(toydata)
#' attitude_partisanship <- correlate_polchoice(data = toydata, attitude_col = attitude_vals, party_col = "party", r_or_r2 = "r2", by = c("attitude_names", cntry", "year"), weight_col = weight)
#' @export
#' @importFrom survey svyglm
#' @importFrom jtools svycor
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tidytable mutate map
#' @importFrom srvyr as_survey_design
#' @importFrom rlang enquo as_name
#' @import dplyr

correlate_polchoice <- function(
    data,
    attitude_col,
    party_col,
    r_or_r2,
    by = NULL,
    weight_col = NULL) {

  # Substituting the expression passed to these arguments
  weight_col <- substitute(weight_col)
  party_col <- substitute(party_col)
  # party_col is a symbol, so checking that it is not numeric always evaluates to TRUE in the calc_assoc function
  party_col_value <- eval(party_col, data)

  # Function to calculate the association between attitudes and partisanship using syntax compatible with functions related to the survey package
  calc_assoc <- function(
      data,
      col1 = "att_val",
      col2 = party_col) {
    if (r_or_r2 == "r") {
      if (!is.numeric(party_col_value)) {
        stop(
          "Estimating Pearson correlations requires numeric values (e.g., ordinal party strength, left-right party family, or a single dummy variable)."
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
      stop("The r_or_r2 argument must be set to `r' or `r2'.")
    }
  }

  input <- data |>
    select(
      {{ attitude_col }},
      {{ party_col }},
      {{ weight_col }},
      any_of(by)
    ) |>
    # Filtering pairwise complete observations that are used to estimate partisan polarization
    drop_na(
      {{ party_col }}, {{ attitude_col }}
      )

  if (!is.null(weight_col)) {
    # Subsetting to weighted sample
    input <- input |>
      drop_na({{ weight_col }}) |>
      filter(.data[[weight_col]] != 0)
  }

    # Creating survey design objects nested
    # by any columns declared in the by argument
    nested_cors <- input |>
      nest_by(across(any_of(by))) |>
      tidytable::mutate(
        design_list = tidytable::map(
          data,
          as_survey_design,
          ids = 1,
          weights = {{ weight_col }}
          )
        ) |>
      # Looping through survey objects to calculate association
      tidytable::mutate(
        assoc_list = tidytable::map(
          design_list,
          calc_assoc
          )
        )

    if (r_or_r2 == "r") {
      # Extracting Pearson correlation coefficient from every 2*2 matrix
      output <- mutate(
        nested_cors,
        r = map(
          assoc_list,
          ~ .x$cors[2]
          )
        )
      } else if (r_or_r2 == "r2") {
        # Extracting coefficient of determination and
        # adjusted R-squared from every summary.lm object.
        output <- mutate(
          nested_cors,
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
    # Removing columns with nested data, survey objects,
    # and correlation matrices/summary.lm objects.
    output <- select(
      output,
      -data, -design_list, -assoc_list
      )

    return(output)

    }
