#' @title spread_pairs
#'
#' @description Takes name and value keys and spreads across (n = 2) combinations of unique levels in the name column
#'
#' @param data A data set object
#' @param name_key A column containing name keys
#' @param value_key A column containing value keys
#' @param other_keys A character vector of other columns to join by and preserve in the output
#'
#' @return A data frame object with name and value keys spread across (n = 2) combinations of unique levels in the name column
#'
#' @examples
#' data(toydata)
#' paired_items <- spread_pairs(data = toydata, name_key = att_name, value_key = att_val, other_keys = c("id", "cntry", "year"))
#' @export
#' @importFrom dplyr distinct pull rename select left_join rename_with
#' @importFrom tidyselect any_of
#'
#'
spread_pairs <- function(
    data,
    name_key,
    value_key,
    other_keys = NULL
    ) {

  # For referencing values passed to name_key and value_key
  name_key <- substitute(name_key)
  value_key <- substitute(value_key)

  # Store character vector of unique names
  names <- data |>
    distinct({{ name_key }}) |>
    pull({{ name_key }})

  # Creating dataframe with unique name pairs
  att_pairs <- combn(
    names,
    2,
    simplify = TRUE
  ) |>
    # Transposing to get pairs in rows
    t() |>
    as.data.frame()

  input <- data |>
    # Renaming to avoid NSE issues in joining statements
    dplyr::rename(
      "name_key_join" = {{ name_key }}
      ) |>
    select(
      name_key_join,
      {{ value_key }},
      any_of(other_keys)
      )

  # Use attitude pairs to spread name column and value colmn in the data
  output <- input |>
    # Filtering to names found in first column of att_pairs
    left_join(
      att_pairs,
      by = c("name_key_join" = "V1"),
      relationship = "many-to-many"
    ) |>
    # Adding in second column of att_pairs
    left_join(
      input,
      by = c(
        other_keys,
        "V2" = "name_key_join"
      ),
      suffix = c("1", "2"),
      relationship = "many-to-many"
    ) |>
    # Using original name column to rename pair keys
    dplyr::rename_with(
      ~ paste0(name_key, "1", recycle0 = TRUE),
      name_key_join
    ) |>
    dplyr::rename_with(
      ~ paste0(name_key, "2", recycle0 = TRUE),
      V2
    )

  return(output)

  }
