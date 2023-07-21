

filter_scale_length <- function(
    data,
    name_key,
    value_key,
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
      value_key_length = scale_length({{ value_key }}),
      .by = {{ name_key }}
      ) |>
    filter(value_key_length >= min_scale_length) |>
    select({{ name_key }})

  output <- inner_join(
    data, meets_min_scale, by = {{ name_key }}
    )

  return(output)

}
