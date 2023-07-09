

correlate_attitudes <- function(
    data,
    attitude_cols,
    measure,
    by = NULL,
    weight_col = NULL,
    marginalize_attitude_cols = FALSE) {

  # Substituting the expression passed to these arguments
  weight_col <- substitute(weight_col)
  weight_col_quo <- enquo(weight_col)

  # Function to calculate the association between pairs of attitudes using syntax compatible with functions related to the survey package
  calc_assoc <- function(
    data,
    col1 = "att_val1",
    col2 = "att_val2") {
    if (measure == "r") {

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
      {{ weight_col }}
    ) |>
    pivot_longer(
      cols = any_of(attitude_cols),
      names_to = "att_item",
      values_to = "att_val"
    )

  # Create a vector of all possible pairs of attitude items
  att_pairs <- combn(
    attitude_cols,
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
    ) |>
    left_join(
      input,
      by = c(
        by,
        as_name(weight_col_quo),
        "V2" = "att_item"
      ),
      relationship = "many-to-many"
    ) |>
    rename(
      att_item1 = att_item,
      att_item2 = V2,
      att_val1 = att_val.x,
      att_val2 = att_val.y
    ) |>
    drop_na(att_val1, att_val2)



}
