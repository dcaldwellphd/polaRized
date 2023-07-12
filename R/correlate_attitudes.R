
# NOTE to self: thinking about creating separate function to generate attitude pairs. Then I can collapse correlate_polchoice and correlate_attitudes into single function (calc_association?) where I pass two value columns (attitude*partisanship or attitude*attitude).

correlate_attitudes <- function(
    data,
    attitude_col,
    r_or_r2,
    by = NULL,
    weight_col = NULL) {

  # Substituting the expression passed to these arguments
  weight_col <- substitute(weight_col)
  weight_col_quo <- enquo(weight_col)

  # Function to calculate the association between pairs of attitudes using syntax compatible with functions related to the survey package
  calc_assoc <- function(
    data,
    col1 = "att_val1",
    col2 = "att_val2") {
    if (r_or_r2 == "r") {

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
      {{ weight_col }},
      any_of(by)
    )

  # Creating character vector of unique attitude names
  # to pass to combn below


  select(att_item) |>
    distinct() |>
    pull(att_item)

  # Create a vector of all possible pairs of attitude items
  att_pairs <- combn(
    attitude_cols |>
      input |>
      select({{  }}),
    2,
    simplify = TRUE
  ) |>
    t() |>
    as.data.frame()

  paired_data <- input |>
    left_join(
      att_pairs,
      by = c("att_item" = "V1"),
      relationship = "many-to-many"
    )

  if(!is.null(weight_col)) {

    paired_data <- left_join(
      paired_data, input,
      by = c(
        by,
        as_name(weight_col_quo),
        "V2" = "att_item"
      ),
      relationship = "many-to-many"
    )
  } else {

    paired_data <- left_join(
      paired_data, input,
      by = c(
        by,
        "V2" = "att_item"
      ),
      relationship = "many-to-many"
    )
  }

  paired_data <- paired_data |>
    rename(
      att_item1 = att_item,
      att_item2 = V2,
      att_val1 = att_val.x,
      att_val2 = att_val.y
    ) |>
    drop_na(att_val1, att_val2)

  if (!is.null(weight_col)) {
    # Subsetting to weighted sample
    paired_data <- paired_data |>
      drop_na({{ weight_col }}) |>
      filter(.data[[weight_col]] != 0)
  }

  if (marginalize_attitude_cols) {
    # Treating all attitude item pairs as a single variable
    nested_data <- nest_by(
      paired_data,
      across(any_of(by))
    )
  } else {
    # Maintaining attitude item pairs as separate variables
    nested_data <- nest_by(
      paired_data,
      att_item1,
      att_item2,
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

  if (r_or_r2 == "r") {
    # Extracting Pearson correlation coefficient from every 2*2 matrix
    output <- mutate(
      output,
      r = map(
        assoc_list,
        ~ .x$cors[2]
      ),
      r = as.numeric(r)
    )
  } else if (r_or_r2 == "r2") {
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
      ),
      r2 = as.numeric(r2),
      adj_r2 = as.numeric(adj_r2)
    )
  }
  # Removing columns with nested data, survey objects, and correlation matrices/summary.lm objects
  output <- select(
    output,
    -data, -design_list, -assoc_list
  )

  return(output)


}
