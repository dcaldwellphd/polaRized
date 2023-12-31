#' @title filter_scale_length
#'
#' @description Filters ordered ratings scales with a minimum length of unique values.
#'
#' @param data A data set object.
#' @param scale_names A column containing scale names.
#' @param scale_values A column containing scale values.
#' @param min_scale_length The threshold below which to remove ordered ratings scales.
#'
#' @return The data set with ordered ratings scales below the threshold removed.
#'
#' @examples
#' data(toydata)
#' # The default is to filter ordered ratings scales with a minimum length of 4 unique values
#' toydata <- filter_scale_length(toydata, scale_names = att_name, scale_values = att_val)
#' # To change the threshold, set min_scale_length to the desired value
#' toydata <- filter_scale_length(toydata, scale_names = att_name, scale_values = att_val, min_scale_length = 5)
#'
#' @export
#'
#' @importFrom dplyr summarise filter pull n_distinct

filter_scale_length <- function(
    data,
    scale_names,
    scale_values,
    min_scale_length = 4
    ) {

  # Create vector of scale names that meet the minimum length threshold
  meets_min_scale <- data |>
    summarise(
      scale_length = n_distinct(
        {{ scale_values }}, na.rm = TRUE
        ),
      .by = {{ scale_names }}
      ) |>
    filter(scale_length >= min_scale_length) |>
    pull({{ scale_names }})

  # Filter data to scale names in meets_min_scale
  output <- data |>
    filter({{ scale_names }} %in% meets_min_scale)

  return(output)

}
