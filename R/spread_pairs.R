#' @title spread_pairs
#'
#' @description Takes a name key and value key and spreads into unique pairs of observations
#'
#' @param data A data set object
#' @param name_key A column containing name keys
#' @param value_key A column containing value keys
#' @param other_keys Other columns to join by and preserve in the output
#'
#' @return A data frame object with name and value keys spread across unique pairs of observations
#'
#' @examples
#' data(toydata)
#' paired_items <- spread_pairs(data = toydata, name_key = att_name, value_key = att_val, other_keys = c(id, group))
#'
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

  # For referencing values passed to name_key and value_key arguments
  name_key <- substitute(name_key)
  value_key <- substitute(value_key)

  # Convert unquoted column names in 'other_keys' to character strings
  other_keys_sub <- substitute(other_keys)
  if (is.null(other_keys_sub) || is.symbol(other_keys_sub)) {
    other_keys <- deparse(other_keys_sub)
  } else {
    other_keys <- sapply(as.list(other_keys_sub)[-1L], deparse)
  }

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
      # suffixes pasted to the value key in x and y
      suffix = c("1", "2"),
      relationship = "many-to-many"
    ) |>
    # Removing rows 
    drop_na(name_key_join, V2) |>
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
