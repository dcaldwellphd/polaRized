#' @title correlate_polchoice
#'
#' @description Estimates the association between attitudes and partisanship
#'
#' @param data A data set object
#' @param attitude_cols A character vector with the names of attitude item columns
#' @param party_col A character vector with the name of the party affiliation column
#' @param measure A string specifying the measure of association. Use "r" to estimate the Pearson correlation between attitudes and an ordinal or dummy variable measure of partisanship. The value "r2" returns the coefficient of determination and adjusted R-squared from OLS models predicting attitudes from indicators of party support, which is useful for estimating partisan polarization in multiparty systems.
#' @param by A character vector of optional groups to nest observations by (e.g. survey wave, country, social group)
#' @param weight_col An optional column of survey weights
#' @param marginalize_attitude_cols A logical value indicating whether to estimate association across or by attitude item. The default, FALSE, groups by attitude items

#'
#' @return A data frame object containing the measure of association between attitudes and partisanship

#' @examples
#' data(toydata)
#' attitude_items <- c("item1", "item2", "item3", "item4", "item5")
#' attitude_partisanship <- correlate_polchoice(data = toydata, attitude_cols = attitude_items, party_col = "party", measure = "r2", by = c("cntry", "year"), weight_col = weight)
#' @export
#' @importFrom survey svyglm
#' @importFrom jtools svycor
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tidytable mutate map
#' @importFrom srvyr as_survey_design
#' @import dplyr

correlate_polchoice <- function(
    data,
    attitude_cols,
    party_col,
    measure,
    by = NULL,
    weight_col = NULL,
    marginalize_attitude_cols = FALSE) {

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
    if (measure == "r") {
      if (!is.numeric(party_col_value)) {
        stop(
          "Estimating Pearson correlations requires numeric values (e.g., ordinal party strength, left-right party family, or a single dummy variable)."
        )
      }

      fmla <- as.formula(paste0("~", col1, " + ", col2))
      assoc <- jtools::svycor(fmla, design = data)
      return(assoc)
    } else if (measure == "r2") {
      fmla <- as.formula(paste0(col1, "~", col2))
      assoc <- survey::svyglm(fmla, design = data)
      return(summary.lm(assoc))
    } else {
      stop("The measure argument must be set to `r' or `r2'.")
    }
  }

  input <- data |>
    select(
      any_of(attitude_cols),
      any_of(by),
      {{ party_col }},
      {{ weight_col }}
    ) |>
    pivot_longer(
      cols = any_of(attitude_cols),
      names_to = "att_item",
      values_to = "att_val"
    ) |>
    # Filtering pairwise complete observations that are used to estimate partisan polarization
    drop_na({{ party_col }}, att_val)

  if (!is.null(weight_col)) {
    # Subsetting to weighted sample
    input <- input |>
      drop_na({{ weight_col }}) |>
      filter(.data[[weight_col]] != 0)
  }

  if (marginalize_attitude_cols) {
    # Treating all attitude items as a single variable
    nested_data <- nest_by(
      input,
      across(any_of(by))
    )
  } else {
    # Maintaining attitude items as separate variables
    nested_data <- nest_by(
      input,
      att_item,
      across(any_of(by))
    )
  }
  # Creating survey objects by group
  survey_objects <- tidytable::mutate(
    nested_data,
    design_list = tidytable::map(
      data,
      as_survey_design,
      ids = 1,
      weights = {{ weight_col }}
    )
  )
  # Looping through survey objects to calculate association
  output <- tidytable::mutate(
    survey_objects,
    assoc_list = tidytable::map(
      design_list,
      calc_assoc
    )
  )

  if (measure == "r") {
    # Extracting Pearson correlation coefficient from every 2*2 matrix
    output <- mutate(
      output,
      r = map(
        assoc_list,
        ~ .x$cors[2]
      )
    )
  } else if (measure == "r2") {
    # Extracting coefficient of determination and adjusted R-squared from every summary.lm object
    output <- mutate(
      output,
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
  # Removing columns with nested data, survey objects, and correlation matrices/summary.lm objects
  output <- select(
    output,
    -data, -design_list, -assoc_list
  )

  return(output)
}
