

filter_scale_length <- function(
    data,
    scale_names,
    scale_values,
    min_scale_length = 4
    ) {

  scale_length <- function(x, na.rm = TRUE){
    if(na.rm){
      x <- x[!is.na(x)]
    }
    (length(unique(x)))
  }

  meets_min_scale <- data |>
    summarise(
      scale_lengths = scale_length({{ scale_values }}),
      .by = {{ scale_names }}
      ) |>
    filter(scale_lengths >= min_scale_length) |>
    pull({{ scale_names }})

  output <- data %>%
    filter({{ scale_names }} %in% meets_min_scale)

  return(output)

}
